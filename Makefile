.PHONY: update-input build switch

PROFILE ?=
INPUT ?=

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
