# Toolchain for ./lean (Lake project depending on Mathlib).
# Mathlib only works with the exact Lean release it was built with, which is newer
# than nixpkgs' `lean4`; so the shell provides `elan`, which reads lean/lean-toolchain
# and installs that release into ~/.elan (nixpkgs' elan patchelfs the binaries on Linux).
# git and curl are needed by `lake` (cloning dependencies) and `lake exe cache get`
# (downloading Mathlib's prebuilt .olean files).
# nixpkgs' elan links with the Nix compiler wrapper, whose `bindnow` hardening resolves every
# symbol of a shared library when it is loaded. Lean's own toolchain binds lazily, and
# doc-gen4's dependency UnicodeBasic relies on that: the libraries of its precompiled modules
# leave their C functions undefined. So the shell turns `bindnow` off.
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.lean = pkgs.mkShell {
        name = "determinize-lean";

        hardeningDisable = [ "bindnow" ];

        packages = [
          pkgs.elan
          pkgs.git
          pkgs.curl
          pkgs.python3
          pkgs.uv
        ];
      };
    };
}
