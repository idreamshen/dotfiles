# Dotfiles (Home Manager)

Home Manager configuration managed as a Nix flake with separate profiles for each machine.

## Profiles
- `homelab-devbox` — Linux
- `company-mbp` — macOS

## Prerequisites
- Nix with flakes enabled
- Home Manager (e.g., `nix run home-manager/master -- init` if not installed)

## Usage
```bash
# Apply a profile (from repo root)
home-manager switch --flake ".#company-mbp"
# or use the helper alias defined in zsh to pick a profile interactively
update-dotfiles
```

## Structure
- `flake.nix` — inputs and per-profile outputs
- `home.nix` — main Home Manager module
- `emacs.el` — Emacs configuration loaded via use-package
