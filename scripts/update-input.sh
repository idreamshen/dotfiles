#!/usr/bin/env bash
# Update a flake input. Accepts the name as $1 or via $INPUT env var.
# If no name is provided, presents an interactive picker.

set -eu

inputs=(
  emacs-overlay
  llm-agents
  nixpkgs
  home-manager
)

name="${1:-${INPUT:-}}"

if [ -z "$name" ]; then
  echo "Select a flake input to update:"
  PS3="> "
  select name in "${inputs[@]}"; do
    if [ -n "$name" ]; then
      break
    fi
    echo "Invalid selection." >&2
  done
fi

found=0
for i in "${inputs[@]}"; do
  if [ "$i" = "$name" ]; then
    found=1
    break
  fi
done

if [ "$found" -eq 0 ]; then
  echo "Error: '$name' is not a known flake input." >&2
  echo "Available inputs:" >&2
  printf '  - %s\n' "${inputs[@]}" >&2
  exit 1
fi

nix flake update "$name"
