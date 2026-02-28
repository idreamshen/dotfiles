---
name: update-flake-inputs
description: Update one or more flake inputs (nixpkgs, emacs-overlay, llm-agents) in home-manager configuration. Use when the user requests to update any flake input, refresh packages, or upgrade dependencies.
---

# Update Flake Inputs

Updates one or more flake inputs to their latest versions.

## Available Inputs

| Input | Source | Description |
|-------|--------|-------------|
| `nixpkgs` | github:nixos/nixpkgs/nixos-unstable | Main package repository |
| `emacs-overlay` | github:nix-community/emacs-overlay | Emacs packages overlay |
| `llm-agents` | github:numtide/llm-agents.nix | LLM agents framework |

## Workflow

1. **Determine which inputs to update**
   - If the user specifies input(s), update only those.
   - If the user says "update all" or doesn't specify, update all three.

2. **Run the update using Make targets**
   ```bash
   make update-nixpkgs          # update nixpkgs only
   make update-emacs-overlay    # update emacs-overlay only
   make update-llm-agents       # update llm-agents only
   ```

   Multiple inputs can be updated in one command:
   ```bash
   make update-nixpkgs update-emacs-overlay update-llm-agents
   ```

3. **Show what changed**
   ```bash
   git diff flake.lock
   ```

4. **Do NOT apply automatically** — let the user decide when to switch.

## Example Usage

- "update nixpkgs" → `make update-nixpkgs`
- "update emacs overlay" → `make update-emacs-overlay`
- "update all flake inputs" → `make update-nixpkgs update-emacs-overlay update-llm-agents`
- "update llm-agents and nixpkgs" → `make update-llm-agents update-nixpkgs`
