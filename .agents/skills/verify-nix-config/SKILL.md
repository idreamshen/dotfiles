---
name: verify-nix-config
description: Compile-only validation for Home Manager Nix/Emacs config changes. Use after editing any `*.nix` or `emacs.el` file to ensure the flake builds without switching profiles.
---

# Verify Nix Config (Compile Only)

Run a build-only check after changes to Nix or Emacs configuration to catch errors without activating a profile.

## Trigger

Use this workflow whenever editing:
- Any `*.nix` file (e.g., `common.nix`, `home.nix`, `osx.nix`, `flake.nix`)
- `emacs.el`

## Workflow

1. **Build the current machine profile (no switch)**
   ```bash
   make build
   ```

   This resolves the local Home Manager profile from host metadata and builds it without applying it.

2. **If you need to verify a specific profile explicitly**
   ```bash
   make build PROFILE=company-mbp
   ```

## Notes

- Do not run `home-manager switch` for verification.
- `make build` is the default verification path for this repo.
- Use `PROFILE=<name>` when validating a profile other than the one inferred for the current machine.
- If the build fails, report the error output and stop before further edits.
