# Toolchain for ./lean (Lake project depending on Mathlib).
# Mathlib only works with the exact Lean release it was built with, which is newer
# than nixpkgs' `lean4`; so the shell provides `elan`, which reads lean/lean-toolchain
# and installs that release into ~/.elan (nixpkgs' elan patchelfs the binaries on Linux).
# git and curl are needed by `lake` (cloning dependencies) and `lake exe cache get`
# (downloading Mathlib's prebuilt .olean files).
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.lean = pkgs.mkShell {
        name = "determinize-lean";

        packages = [
          pkgs.elan
          pkgs.git
          pkgs.curl
        ];
      };
    };
}
