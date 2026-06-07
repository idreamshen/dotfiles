.PHONY: update-input build switch gc update-secrets restart-emacs-daemon

PROFILE ?=
INPUT ?=
AGE ?= -3 days

update-input:
	@INPUT='$(INPUT)' scripts/update-input.sh

build: update-secrets
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

restart-emacs-daemon:
	@case "$$(uname -s)" in \
		Darwin) \
			echo "Restarting Emacs daemon via launchd..."; \
			launchctl unload "$${HOME}/Library/LaunchAgents/org.nix-community.home.emacs.plist" 2>/dev/null || true; \
			launchctl load "$${HOME}/Library/LaunchAgents/org.nix-community.home.emacs.plist"; \
			echo "Waiting for Emacs daemon socket..."; \
			for i in $$(seq 1 10); do \
				emacsclient -e '(server-running-p)' 2>/dev/null && break; \
				sleep 1; \
			done; \
			;; \
		Linux) \
			echo "Restarting Emacs daemon via systemd user service..."; \
			systemctl --user restart emacs.service; \
			;; \
		*) \
			echo "Unsupported OS: $$(uname -s)" >&2; \
			exit 1; \
			;; \
	esac
