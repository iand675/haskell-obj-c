#!/usr/bin/env bash
#
# regen-frameworks.sh — Regenerate Haskell bindings for all macOS frameworks.
#
# Usage:
#   ./scripts/regen-frameworks.sh
#
# Prerequisites:
#   - Xcode (or the Command Line Tools) must be installed.
#   - The codegen must already be built:  stack build haskell-obj-c-codegen
#
# What it does:
#   1. Removes old generated packages.
#   2. Runs objc-codegen against every .framework in the macOS SDK.
#   3. Rewrites the generated-package entries in stack.yaml.
#
# ---------------------------------------------------------------------------
# NOTES ON GENERATED PACKAGE STRUCTURE
# ---------------------------------------------------------------------------
#
# Base packages (apple-<fw>-gen):
#   One per framework. Contains the class hierarchy, enum definitions, struct
#   types, and all methods that *originate* in that framework.
#
# Extension packages (apple-<catFw>-<classFw>-gen-ext):
#   Objective-C categories allow one framework to add methods to a class owned
#   by a different framework. For example, AppKit declares categories on
#   Foundation's NSString, and CoreImage declares categories on AVFoundation's
#   AVDepthData. If those category methods lived in the base package of the
#   framework that declares them, the two base packages would form a circular
#   dependency (framework A depends on framework B for the class type, while
#   framework B depends on A for the same reason in reverse).
#
#   Extension packages break this cycle. Each one depends on exactly the two
#   frameworks involved (the one declaring the category and the one owning the
#   class) and contains only the cross-framework category methods. This keeps
#   the base packages cycle-free while still exposing every method.
#
# Cycle-aware dependency computation:
#   Even within base packages, method return/parameter types can reference
#   classes from other frameworks. The codegen computes a dependency graph in
#   two passes:
#     1. Core (structural) deps — superclass and struct-field references.
#        These form a DAG by construction.
#     2. Method type deps — frameworks referenced by method signatures.
#        Each candidate edge is tested: if adding it would create a cycle
#        (checked via transitive reachability), it is dropped and the
#        affected methods fall back to the opaque RawId type.
# ---------------------------------------------------------------------------

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SDK_PATH="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null || true)"
if [ -z "$SDK_PATH" ] || [ ! -d "$SDK_PATH/System/Library/Frameworks" ]; then
  echo "Error: macOS SDK not found via xcrun. Check Xcode/Command Line Tools and xcode-select." >&2
  exit 1
fi
SDK_FRAMEWORKS="$SDK_PATH/System/Library/Frameworks"
OUTPUT_DIR="$REPO_ROOT/generated"
STACK_YAML="$REPO_ROOT/stack.yaml"
MARKER='# Regenerate with: stack exec objc-codegen -- -f AppKit -f WebKit ... -o generated'

if [ ! -d "$SDK_FRAMEWORKS" ]; then
  echo "Error: macOS SDK frameworks not found at $SDK_FRAMEWORKS" >&2
  echo "       Install Xcode or check the path." >&2
  exit 1
fi

# --- Step 1: Clean old generated packages -----------------------------------

echo "==> Removing old generated packages..."
rm -rf "$OUTPUT_DIR"/apple-*

# Strip generated entries from stack.yaml so `stack exec` doesn't choke on
# missing directories.
python3 -c "
lines = open('$STACK_YAML').readlines()
out = [l for l in lines if not l.startswith('- generated/apple-')]
open('$STACK_YAML', 'w').writelines(out)
"

# --- Step 2: Build the codegen (if needed) ----------------------------------

echo "==> Building codegen..."
stack build haskell-obj-c-codegen 2>&1 | tail -1

# --- Step 3: Run the codegen ------------------------------------------------

echo "==> Collecting frameworks from SDK..."
args=(-o "$OUTPUT_DIR")
for fw in "$SDK_FRAMEWORKS"/*.framework; do
  name=$(basename "$fw" .framework)
  args+=(-f "$name")
done
echo "    Found $(( (${#args[@]} - 2) / 2 )) frameworks."

echo "==> Running objc-codegen (this takes ~90s)..."
stack exec objc-codegen -- "${args[@]}"

# --- Step 4: Update stack.yaml ----------------------------------------------

echo "==> Updating stack.yaml..."
ENTRIES=$(ls -d "$OUTPUT_DIR"/apple-* | sort | sed "s|^$REPO_ROOT/||" | sed 's/^/- /')

python3 -c "
import re, sys

yaml = open('$STACK_YAML').read()
marker = '''$MARKER'''
idx = yaml.index(marker) + len(marker)

entries = '''$ENTRIES'''
new_yaml = yaml[:idx] + '\n' + entries + '\n' + yaml[idx:]
new_yaml = re.sub(r'\n{3,}', '\n\n', new_yaml)
open('$STACK_YAML', 'w').write(new_yaml)
"

pkg_count=$(echo "$ENTRIES" | wc -l | tr -d ' ')
echo "==> Done. $pkg_count packages registered in stack.yaml."
