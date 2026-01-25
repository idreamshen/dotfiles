# Home Manager Dotfiles: Agent Guidance

This file documents how to work in this repository for automated or human agents.
Keep changes minimal, follow existing patterns, and prefer Nix/Home Manager idioms.

## Repository overview

- Root contains a Nix flake for Home Manager profiles.
- Primary modules live in `home.nix`, `common.nix`, and `osx.nix`.
- Emacs configuration lives in `emacs.el`.
- Helper scripts for agent-shell upgrades live under `.claude/skills/upgrade-agent-shell/scripts/`.

## Build / apply commands

- Apply a profile:
  - `home-manager switch --flake ".#company-mbp"`
  - `home-manager switch --flake ".#homelab-devbox"`
- Convenience alias (defined in `common.nix`):
  - `update-dotfiles` (prompts for a profile and runs `home-manager switch`)
- Update nixpkgs input:
  - `nix flake lock --update-input nixpkgs`

## Linting / formatting / tests

- No explicit linting or formatting commands are defined in this repo.
- No test runner is configured.
- There is no documented single-test command; add it here if tests are introduced.

## Cursor / Copilot rules

- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` were found.

## Nix style guidelines

- Indentation is two spaces.
- Attribute sets use `{ ... }` with opening brace on the same line.
- Prefer `let ... in` for local bindings, then return an attribute set.
- Use `inherit` for values pulled from `pkgs` or `pkgs.lib`.
- Group related attributes together (e.g., `programs.zsh`, `programs.git`).
- Use `lib.mkIf` for conditional blocks (e.g., platform-specific settings).
- Use explicit lists for imports:
  - `imports = [ ./common.nix ./osx.nix ];`
- Keep list formatting consistent:
  - One entry per line for longer lists.
  - Inline list for short lists (2-3 items).
- Prefer `home.*` and `programs.*` modules instead of ad-hoc shell scripts.
- Use `''` multiline strings for shell snippets and heredocs.
- Keep Nix strings double-quoted unless multiline.
- Avoid unused bindings in `let` blocks.
- Prefer `config.lib.dag.entryAfter` for activation order.

## Nix naming conventions

- Attribute names are lowerCamelCase, matching Home Manager module options.
- Variables in `let` follow lowerCamelCase (e.g., `llmAgentsPkgs`).
- Use descriptive names for profiles (`company-mbp`, `homelab-devbox`).

## Error handling (Nix + shell snippets)

- When writing shell snippets, return non-zero on error paths.
- Use explicit error messages for missing profiles or configuration.
- Guard platform-specific behavior with `lib.mkIf`.

## Python script conventions

- Scripts use a `#!/usr/bin/env python3` shebang.
- Use standard library modules only; no third-party deps are present.
- Type hints are used for function signatures where helpful.
- Error paths print to `stderr` and exit with non-zero status.
- Prefer `pathlib.Path` for file operations.
- Keep functions small and focused (single responsibility).
- Use `subprocess.run(..., check=True)` for commands and surface errors.

## Emacs Lisp conventions

- Uses `use-package` blocks for package configuration.
- Prefer `:init` for setup and `:config` for configuration.
- Use `setq`/`setopt` for configuration values.
- Keep `use-package` blocks focused on a single package.
- Use `:hook` for mode hooks instead of manual `add-hook` where possible.

## File organization

- `home.nix` should only import other modules.
- Shared configuration belongs in `common.nix`.
- Platform-specific configuration belongs in `osx.nix`.
- Keep large shell functions inside `programs.zsh.initContent` if needed.

## Working with profiles

- Profiles are defined in `flake.nix` under `homeConfigurations`.
- `username` and `homeDirectory` are passed via `extraSpecialArgs`.
- Avoid hardcoding paths outside those values unless necessary.

## Safe change guidance

- Keep changes minimal; avoid refactors when making small edits.
- Do not change `home.stateVersion` unless explicitly requested.
- When touching activation scripts, keep side effects idempotent.
- Do not introduce new dependencies without a clear need.

## Adding new packages

- Add packages under `home.packages` in `common.nix`.
- Prefer `pkgs.<name>`; use overlays only when required.
- Group related packages together and keep alphabetical ordering if feasible.

## Notes for agents

- This repo is a Nix flake; commands should run from repo root.
- The primary workflows are `home-manager switch` and `nix flake lock` updates.
- If you add tests or lint tooling, update this file with the exact commands.
