# Toolchain for ./sim: Node.js (node --test) and npm (esbuild is an npm devDependency).
# git lets CI check that the committed app.bundle.js is the one the sources build.
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.sim = pkgs.mkShell {
        name = "determinize-sim";

        packages = [
          pkgs.nodejs
          pkgs.git
        ];
      };
    };
}
