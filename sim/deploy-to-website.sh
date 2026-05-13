#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WEBSITE_DIR="${WEBSITE_DIR:-"$HOME/git/julesjacobs.github.io"}"
TARGET_REL="${TARGET_REL:-misc/determinize}"
LEGACY_TARGET_REL="${LEGACY_TARGET_REL:-misc/the-terminals}"
TARGET_DIR="$WEBSITE_DIR/$TARGET_REL"
LEGACY_TARGET_DIR="$WEBSITE_DIR/$LEGACY_TARGET_REL"
COMMIT_MESSAGE="${COMMIT_MESSAGE:-Deploy determinization simulator}"

if [[ ! -d "$WEBSITE_DIR/.git" ]]; then
  echo "Website repo not found: $WEBSITE_DIR" >&2
  exit 1
fi

cd "$SCRIPT_DIR"
npm test
npm run build

rm -rf "$TARGET_DIR"
mkdir -p "$TARGET_DIR"
cp index.html styles.css app.bundle.js "$TARGET_DIR/"

if [[ "$LEGACY_TARGET_REL" != "$TARGET_REL" && -d "$LEGACY_TARGET_DIR" ]]; then
  rm -rf "$LEGACY_TARGET_DIR"
fi

cd "$WEBSITE_DIR"

if ! git diff --cached --quiet; then
  echo "Website repo already has staged changes. Unstage or commit them before deploying." >&2
  exit 1
fi

git add -A "$TARGET_REL"
if [[ "$LEGACY_TARGET_REL" != "$TARGET_REL" ]]; then
  git add -A "$LEGACY_TARGET_REL"
fi

if git diff --cached --quiet -- "$TARGET_REL" "$LEGACY_TARGET_REL"; then
  echo "No deployment changes for $TARGET_REL."
  exit 0
fi

git commit -m "$COMMIT_MESSAGE" -- "$TARGET_REL" "$LEGACY_TARGET_REL"
git push origin "$(git branch --show-current)"

echo "Deployed to $TARGET_REL"
