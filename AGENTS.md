# Home Manager Dotfiles: Agent Guidance

Keep changes minimal, follow existing patterns, and prefer Nix/Home Manager idioms.

## Repository overview

- Nix flake for Home Manager profiles.
- Profile files (`company-mbp.nix`, `home-mbp.nix`, `homelab-devbox.nix`, `homelab-openclaw.nix`) are thin wrappers that import modules.
- Shared modules live in `modules/`: `common.nix`, `osx.nix`, `emacs.nix`, `dev-machine.nix`.
- `modules/dev-machine.nix` imports `emacs.nix` and `sops-nix`; it manages dev tooling (openCode, Clau­de Code, Playwright, etc.) and secrets templates.
- Emacs configuration is `modules/emacs.el`.
- OpenCode config and encrypted secrets live in `modules/opencode/`.
- SOPS public keys are defined in `.sops.yaml`; `modules/opencode/secrets.yaml` is the encrypted secrets file.
- The Makefile uses `scripts/detect-profile.sh` to auto-select a profile by hostname/username/system/homeDirectory.

## Build / apply commands

```bash
make build              # Runs update-secrets → home-manager build for auto-detected profile
make switch             # Runs home-manager switch for auto-detected profile
make gc                 # Garbage-collect old Home Manager generations
make update-input       # Interactive flake input updater (or set INPUT=name)
make update-secrets     # Re-encrypt all secrets.yaml files with all .sops.yaml keys

home-manager switch --flake ".#company-mbp"
home-manager switch --flake ".#homelab-devbox"
nix flake lock --update-input nixpkgs
```

The shell alias `update-dotfiles` (defined as a zsh function in `modules/common.nix`) prompts for a profile, runs `git pull --rebase --autostash`, then `home-manager switch`.

## Architecture notes

### Module hierarchy

```
flake.nix                  →  inputs, profiles, overlay wiring (emacs-overlay, go-overlay)
company-mbp.nix            ─┐
home-mbp.nix               ─┤ thin wrappers, differ in system/os-arch
homelab-devbox.nix         ─┤   └─ imports: [ ./modules/common.nix ./modules/dev-machine.nix ] (and ./modules/osx.nix for macOS)
homelab-openclaw.nix       ─┘       homelab-openclaw also imports ./modules/emacs.nix explicitly (cloneEmacsFiles = true)
modules/common.nix         →  home.username, home.packages (core packages), zsh, git, tmux, direnv, bash, ssh, starship
modules/osx.nix            →  macOS-only (iterm2, iTerm2 shell integration) — guarded by lib.mkIf stdenv.isDarwin
modules/emacs.nix          →  Emacs package, emacs.el config, rime, optional cloneEmacsFiles activation
modules/dev-machine.nix    →  LLM agents (openCode, Clau­de Code, etc.), Playwright, SOPS secrets, opencode-web service
```

### SOPS / secrets

- `modules/dev-machine.nix` imports `inputs.sops-nix.homeManagerModules.sops`.
- `modules/opencode/secrets.yaml` is the encrypted secrets file.
- Secrets are exposed via `config.sops.placeholder.<key>` and rendered into templates (e.g., `~/.config/opencode/opencode.json`).
- `make build` runs `make update-secrets` first, which calls `sops updatekeys -y` on every `secrets.yaml`.
- When adding a new SOPS-encrypted secret, register it under `sops.secrets` in `modules/dev-machine.nix` and add the corresponding encrypted value to `modules/opencode/secrets.yaml`.

### Flake inputs

Additional flake inputs beyond nixpkgs/home-manager:

| Input | Purpose |
|-------|---------|
| `emacs-overlay` | Emacs packages overlay |
| `llm-agents` | LLM agent tools (openCode, Claude Code, etc.) |
| `go-overlay` | Go toolchain overlay |
| `sops-nix` | SOPS secrets integration |

`make update-input` knows about: `emacs-overlay`, `llm-agents`, `nixpkgs`, `home-manager`.

### cloneEmacsFiles

Only `homelab-openclaw` sets `cloneEmacsFiles = true`. It triggers a post-writeBoundary activation hook in `modules/emacs.nix` that clones/pulls the `emacs-files` repo into `$HOME/emacs-files`.

## Nix style guidelines

- Indentation is two spaces.
- Use `lib.mkIf` for conditional blocks (platform checks, `cloneEmacsFiles`).
- Guard platform-specific behavior with `lib.mkIf stdenv.isDarwin` / `lib.mkIf (!stdenv.isDarwin)`.
- Use `config.lib.dag.entryAfter` for activation script ordering.
- Use `''` multiline strings for shell snippets and heredocs.
- Use `with pkgs; [...]` for package lists. Group related packages together.
- Keep profile files minimal — they should only specify imports.
- Larger shell functions go in `programs.zsh.initContent`.

## Safe change guidance

- Do not change `home.stateVersion` unless explicitly requested.
- When touching activation scripts, keep side effects idempotent.
- Do not introduce new dependencies without a clear need.
