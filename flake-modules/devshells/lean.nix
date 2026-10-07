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
# On Linux the shell also sets STORM_PYTHON, a Python with stormpy, so that the tests compare
# the certificates with Storm. nixpkgs has Storm but not stormpy, so this takes PyPI's wheel of
# the version in tools/storm-requirements.txt, which bundles its own Storm, and patches its
# libraries for Nix.
{
  perSystem =
    { lib, pkgs, ... }:
    let
      python = pkgs.python3;
      version = lib.removePrefix "stormpy==" (
        lib.trim (builtins.readFile ../../tools/storm-requirements.txt)
      );
      tag = "cp${lib.replaceStrings [ "." ] [ "" ] python.pythonVersion}";

      # The wheels' hashes are for this version and Python.
      wheels = {
        version = "1.14.0";
        python = "cp314";
        x86_64-linux = {
          platform = "manylinux_2_34_x86_64";
          hash = "sha256-LFQJqLA7o8JeiMgEemBWJ8Zl0g03+OfErS1KlwnR/vc=";
        };
        aarch64-linux = {
          platform = "manylinux_2_34_aarch64";
          hash = "sha256-EWiFfh0LTZzZ6Tz9OdtCeRbdh+XTkaZEs8U9NGD76Ew=";
        };
      };
      wheel = wheels.${pkgs.stdenv.hostPlatform.system} or null;

      stormpy =
        assert lib.assertMsg (version == wheels.version && tag == wheels.python) ''
          flake-modules/devshells/lean.nix has stormpy ${wheels.version} for ${wheels.python}, but
          tools/storm-requirements.txt asks for ${version} and nixpkgs' Python is ${tag}: update the
          wheels' version, Python tag and hashes (pypi.org/project/stormpy/${version}/#files).'';
        python.pkgs.buildPythonPackage {
          pname = "stormpy";
          inherit version;
          format = "wheel";
          src = python.pkgs.fetchPypi {
            pname = "stormpy";
            inherit version;
            format = "wheel";
            dist = tag;
            python = tag;
            abi = tag;
            inherit (wheel) platform hash;
          };
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          buildInputs = [
            pkgs.stdenv.cc.cc.lib
            pkgs.zlib
          ];
          dependencies = [ python.pkgs.deprecated ];
          pythonImportsCheck = [ "stormpy" ];
        };
    in
    {
      devShells.lean = pkgs.mkShell (
        {
          name = "determinize-lean";

          hardeningDisable = [ "bindnow" ];

          packages = [
            pkgs.elan
            pkgs.git
            pkgs.curl
            pkgs.python3
            pkgs.uv
          ];
        }
        // lib.optionalAttrs (wheel != null) {
          STORM_PYTHON = "${python.withPackages (_: [ stormpy ])}/bin/python3";
        }
      );
    };
}
