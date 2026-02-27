.PHONY: update-nixpkgs update-emacs-overlay update-llm-agents \
        build-homelab-openclaw build-company-mbp \
        switch-homelab-openclaw switch-company-mbp

update-nixpkgs:
	nix flake lock --update-input nixpkgs

update-emacs-overlay:
	nix flake lock --update-input emacs-overlay

update-llm-agents:
	nix flake lock --update-input llm-agents

build-homelab-openclaw:
	home-manager build --flake ".#homelab-openclaw"

build-company-mbp:
	home-manager build --flake ".#company-mbp"

switch-homelab-openclaw:
	home-manager switch --flake ".#homelab-openclaw"

switch-company-mbp:
	home-manager switch --flake ".#company-mbp"
