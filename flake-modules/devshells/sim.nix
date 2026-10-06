# Toolchain for ./sim: Node.js 24 (node --test), npm (esbuild is an npm devDependency) and Biome
# (formatter and linter, configured in sim/biome.json).
# git lets CI check that the committed app.bundle.js is the one the sources build.
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.sim = pkgs.mkShell {
        name = "determinize-sim";

        packages = [
          pkgs.nodejs_24
          pkgs.biome
          pkgs.git
        ];
      };
    };
}
