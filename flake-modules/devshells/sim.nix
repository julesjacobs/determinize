# Toolchain for ./sim: the newest Node.js in nixpkgs (node --test; nodejs_latest, so a flake update
# brings a new major version), npm and Biome (formatter and linter, configured in sim/biome.json),
# and sim/node_modules, which Nix builds from sim/package-lock.json (esbuild is an npm
# devDependency). @types/node follows Node's major version.
{
  perSystem =
    { lib, pkgs, ... }:
    let
      package = lib.importJSON ../../sim/package.json;
      packageLock = lib.importJSON ../../sim/package-lock.json;

      # npm installs an optional package only if its os, cpu and (on Linux) libc fields admit the
      # host. Each field lists names, or names negated with "!".
      host = pkgs.stdenv.hostPlatform;
      admits =
        field: name: entry:
        let
          names = entry.${field} or [ ];
          wanted = lib.filter (n: !lib.hasPrefix "!" n) names;
        in
        !lib.elem "!${name}" names && (wanted == [ ] || lib.elem name wanted);
      forHost =
        entry:
        admits "os" host.node.platform entry
        && admits "cpu" host.node.arch entry
        && (host.isLinux -> admits "libc" host.libc entry);

      # The optional packages that are not for the host, such as esbuild's native binaries for
      # other systems, are replaced by a package.json alone, so that Nix does not download them.
      stub =
        path: entry:
        let
          name = entry.name or (lib.last (lib.splitString "node_modules/" path));
        in
        pkgs.writeTextFile {
          name = "${lib.replaceStrings [ "@" "/" ] [ "" "-" ] name}-${entry.version}-stub";
          destination = "/package.json";
          text = builtins.toJSON {
            inherit name;
            inherit (entry) version;
          };
        };
      stubs = lib.mapAttrs stub (
        lib.filterAttrs (path: entry: entry.optional or false && !forHost entry) packageLock.packages
      );

      nodeModules = pkgs.importNpmLock.buildNodeModules {
        nodejs = pkgs.nodejs_latest;
        inherit package packageLock;
        derivationArgs.npmDeps = pkgs.importNpmLock {
          inherit package packageLock;
          packageSourceOverrides = stubs;
        };
      };
    in
    {
      devShells.sim =
        let
          types = packageLock.packages."node_modules/@types/node".version;
          node = pkgs.nodejs_latest.version;
        in
        assert lib.assertMsg (lib.versions.major types == lib.versions.major node) ''
          sim/package-lock.json has @types/node ${types}, but the shell's Node.js is ${node}. In
          sim/, run npm install --save-dev @types/node@${lib.versions.major node}'';
        pkgs.mkShell {
          name = "determinize-sim";

          packages = [
            pkgs.nodejs_latest
            pkgs.biome
            pkgs.importNpmLock.hooks.linkNodeModulesHook
          ];

          # Read by linkNodeModulesHook. Shells that take this one in inputsFrom, such as the default
          # shell, do not inherit it, so they skip the shell hook below and link nothing.
          npmDeps = nodeModules;

          # Links sim/node_modules of the checkout that contains the working directory, wherever in it
          # the shell starts, and puts node_modules/.bin on PATH.
          shellHook = ''
            if [[ -n ''${npmDeps-} ]]; then
              root=$PWD
              until [[ -f $root/flake.nix && -f $root/sim/package-lock.json || $root == / ]]; do
                root=$(dirname "$root")
              done
              if [[ $root == / ]]; then
                echo "Not in the determinize repository: sim/node_modules is not linked." >&2
              else
                pushd "$root/sim" >/dev/null
                linkNodeModulesHook
                popd >/dev/null
              fi
              unset root
            fi
          '';
        };
    };
}
