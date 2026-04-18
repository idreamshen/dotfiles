.PHONY: update-input build switch gc

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
