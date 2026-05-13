.PHONY: update-input build switch gc update-secrets

PROFILE ?=
INPUT ?=
AGE ?= -3 days

update-input:
	@INPUT='$(INPUT)' scripts/update-input.sh

build:
	@profile="$$( PROFILE='$(PROFILE)' scripts/detect-profile.sh )"; \
	echo "Building Home Manager profile: $$profile"; \
	home-manager build --flake ".#$$profile"

switch:
	@profile="$$( PROFILE='$(PROFILE)' scripts/detect-profile.sh )"; \
	echo "Switching Home Manager profile: $$profile"; \
	home-manager switch --flake ".#$$profile"

gc:
	home-manager expire-generations "$(AGE)"
	nix-collect-garbage

update-secrets:
	@echo "Updating secrets.yaml files to include all recipients..."
	@find . -name secrets.yaml -not -path './.git/*' | while read -r f; do \
		echo "Updating keys for $$f"; \
		SOPS_AGE_KEY_FILE="$${HOME}/.config/sops/age/keys.txt" sops updatekeys -y "$$f"; \
	done
	@echo "Done."
