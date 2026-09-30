# Toolchain for ./lean (Lake project depending on Mathlib).
# Mathlib only works with the exact Lean release it was built with, which is newer
# than nixpkgs' `lean4`; so the shell provides `elan`, which reads lean/lean-toolchain
# and installs that release into ~/.elan (nixpkgs' elan patchelfs the binaries on Linux).
# git and curl are needed by `lake` (cloning dependencies) and `lake exe cache get`
# (downloading Mathlib's prebuilt .olean files).
{ inputs, ... }:
{
  perSystem =
    { pkgs, lib, ... }:
    let
      # Comparator runs builds in a landrun sandbox and wants landrun's latest release. The
      # pinned nixpkgs has 0.1.15, whose recipe patches that release's test script, so the
      # override skips upstream's tests; it is dropped once nixpkgs is updated.
      landrun =
        if lib.versionAtLeast pkgs.landrun.version "0.1.17" then
          pkgs.landrun
        else
          pkgs.landrun.overrideAttrs (
            finalAttrs: _: {
              version = "0.1.17";
              src = pkgs.fetchFromGitHub {
                owner = "Zouuup";
                repo = "landrun";
                tag = "v${finalAttrs.version}";
                hash = "sha256-BjIRO5qDd5lnNZEE8gmMJP0CN6ZOAIFEb/DzDRD2fu8=";
              };
              vendorHash = "sha256-gmmXTffuHFPbPKNY2DrFApXT2xazwnvmM4/aQiepMuY=";
              postPatch = "";
              doInstallCheck = false;
            }
          );

      # `comparator-check` runs tools/comparator.sh with the pinned comparator and lean4export
      # sources. Both are Lean programs that must be built with the project's Lean release,
      # which nixpkgs does not have, so the script builds them with elan on first use.
      comparator-check = pkgs.writeShellApplication {
        name = "comparator-check";
        runtimeInputs = [
          landrun
          pkgs.git
        ];
        runtimeEnv = {
          COMPARATOR_SRC = "${inputs.comparator}";
          LEAN4EXPORT_SRC = "${inputs.lean4export}";
        };
        text = ''exec "$(git rev-parse --show-toplevel)/tools/comparator.sh" "$@"'';
      };
    in
    {
      devShells.lean = pkgs.mkShell {
        name = "determinize-lean";

        packages = [
          pkgs.elan
          pkgs.git
          pkgs.curl
          pkgs.python3
          pkgs.uv
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          landrun
          comparator-check
        ];
      };
    };
}
