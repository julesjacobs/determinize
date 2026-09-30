#!/usr/bin/env bash
# Check the public Lean theorems with Lean FRO's comparator (https://github.com/leanprover/comparator),
# independently of the check at the end of lean/Determinize/Theorems.lean.
#
# lean/Determinize/Challenge.lean states every public theorem with `sorry` as its proof and imports
# only Spec. Comparator builds it and Determinize.Theorems in a landrun sandbox, exports both, and
# checks that every theorem of Challenge.lean is proved in Theorems.lean with the same statement
# (and the same definitions under it), using only propext, Classical.choice and Quot.sound. It then
# replays the proofs and everything they use through Lean's kernel.
#
# Run it as `comparator-check` from the lean devshell (`./check.sh comparator` does that), which
# provides landrun and the pinned comparator and lean4export sources. Needs Linux with Landlock and
# `systemd-run --user`. For an independent audit, run it in a fresh clone before building anything
# there, after `lake exe cache get`: comparator assumes the solution has not been compiled yet.
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LEAN="$ROOT/lean"
: "${COMPARATOR_SRC:?run this as comparator-check from the lean devshell (nix develop .#lean)}"
: "${LEAN4EXPORT_SRC:?run this as comparator-check from the lean devshell (nix develop .#lean)}"

# lean4export reads the project's .olean files, so both tools are built with the project's Lean.
# They live outside lean/.lake, which the sandboxed builds may write to.
toolchain="$(<"$LEAN/lean-toolchain")"
key="$(printf '%s\n' "$COMPARATOR_SRC" "$LEAN4EXPORT_SRC" "$toolchain" | sha256sum | cut -c1-16)"
tools="${XDG_CACHE_HOME:-$HOME/.cache}/determinize/comparator/$key"
comparator="$tools/comparator/.lake/build/bin/comparator"
lean4export="$tools/lean4export/.lake/build/bin/lean4export"
if [[ ! -x "$comparator" || ! -x "$lean4export" ]]; then
  echo "Building comparator and lean4export with $toolchain in $tools"
  rm -rf "$tools"
  mkdir -p "$tools"
  cp -r --no-preserve=mode "$COMPARATOR_SRC" "$tools/comparator"
  cp -r --no-preserve=mode "$LEAN4EXPORT_SRC" "$tools/lean4export"
  echo "$toolchain" >"$tools/comparator/lean-toolchain"
  echo "$toolchain" >"$tools/lean4export/lean-toolchain"
  # Take comparator's one dependency, lean4export, from the pinned source instead of cloning it.
  sed -i '/^\[\[require\]\]/,$d' "$tools/comparator/lakefile.toml"
  printf '[[require]]\nname = "lean4export"\npath = "../lean4export"\n' >>"$tools/comparator/lakefile.toml"
  rm -f "$tools/comparator/lake-manifest.json"
  (cd "$tools/lean4export" && lake build lean4export)
  (cd "$tools/comparator" && lake build comparator)
fi

# The nix store holds the interpreters of elan's wrapper scripts and of the C compiler lake calls;
# comparator lets its sandbox execute only the Lean toolchain and git.
landrun="$tools/landrun"
printf '#!%s\nexec %q --rox /nix/store "$@"\n' "$(command -v bash)" "$(command -v landrun)" >"$landrun"
chmod +x "$landrun"

# Every theorem declared in Challenge.lean is checked, and every proposition Spec defines must be
# the statement of one of them. Challenge.lean is part of what a reviewer reads, so building it
# outside the sandbox is fine.
config="$(mktemp "$tools/config.XXXXXX.json")"
trap 'rm -f "$config"' EXIT
(cd "$LEAN" && lake build Determinize.Challenge >/dev/null)
if ! (cd "$LEAN" && lake env lean --stdin) >"$config" <<'EOF'
import Determinize.Challenge
open Lean in
#eval show CoreM Unit from do
  let env ← getEnv
  let some challenge := env.getModuleIdx? `Determinize.Challenge
    | throwError "Determinize.Challenge is not loaded"
  let theorems := env.constants.map₁.fold (init := #[]) fun found name info =>
    if env.getModuleIdxFor? name == some challenge && info.isTheorem && !name.isInternal
    then found.push (name, info.type) else found
  for (name, info) in env.constants.map₁.toList do
    if (`Determinize.Spec).isPrefixOf name && info.isDefinition && info.type.isProp
        && !theorems.any (·.2 == .const name []) then
      throwError "{name} is stated in Spec but no theorem in Challenge.lean states it"
  IO.println <| Json.pretty <| Json.mkObj [
    ("challenge_module", "Determinize.Challenge"),
    ("solution_module", "Determinize.Theorems"),
    ("theorem_names", toJson ((theorems.map (·.1.toString)).qsort (· < ·))),
    ("permitted_axioms", toJson #["propext", "Classical.choice", "Quot.sound"]),
    ("enable_nanoda", false)]
EOF
then
  cat "$config" >&2
  exit 1
fi
count="$(grep -c '"Determinize\.Theorems\.' "$config")"
echo "Checking $count theorems of Determinize/Challenge.lean against Determinize/Theorems.lean"

# systemd-run keeps the sandbox from reaching Unix sockets, as comparator's README requires until
# landrun can restrict them itself (Linux 7.1).
systemd-run --user --quiet --wait --pipe --collect \
  --property=RestrictAddressFamilies=~AF_UNIX \
  --working-directory="$LEAN" \
  -E PATH="$PATH" -E COMPARATOR_LANDRUN="$landrun" -E COMPARATOR_LEAN4EXPORT="$lean4export" \
  ${ELAN_HOME:+-E ELAN_HOME="$ELAN_HOME"} \
  -- lake env "$comparator" "$config"
