# Toolchain for ./sim: Node.js (node --test) and npm (esbuild is an npm devDependency).
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.sim = pkgs.mkShell {
        name = "determinize-sim";

        packages = [
          pkgs.nodejs
        ];
      };
    };
}
