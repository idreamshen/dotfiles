.PHONY: update-nixpkgs update-emacs-overlay update-llm-agents \
        print-profile build switch \
        build-company-mbp build-home-mbp build-homelab-devbox build-homelab-openclaw \
        switch-company-mbp switch-home-mbp switch-homelab-devbox switch-homelab-openclaw

PROFILE ?=

define PROFILE_METADATA_CMD
nix eval .#homeConfigurations --apply 'attrs: \
  let names = builtins.attrNames attrs; \
  in builtins.concatStringsSep "\n" (builtins.map (name: \
    let cfg = attrs.$${name}; \
    in "$${name}\t$${cfg.config.home.username}\t$${cfg.pkgs.stdenv.hostPlatform.system}\t$${cfg.config.home.homeDirectory}" \
  ) names)' --raw 2>/dev/null
endef

update-nixpkgs:
	nix flake lock --update-input nixpkgs

update-emacs-overlay:
	nix flake lock --update-input emacs-overlay

update-llm-agents:
	nix flake lock --update-input llm-agents

print-profile:
	@profile="$(PROFILE)"; \
	if [ -z "$$profile" ]; then \
	  username="$$(id -un)"; \
	  hostname_short="$$(hostname 2>/dev/null | sed 's/\..*//')"; \
	  hostname_lc="$$(printf '%s' "$$hostname_short" | tr '[:upper:]' '[:lower:]')"; \
	  os="$$(uname -s | tr '[:upper:]' '[:lower:]')"; \
	  arch="$$(uname -m)"; \
	  home_dir="$$HOME"; \
	  case "$$arch" in \
	    arm64) arch="aarch64" ;; \
	    amd64) arch="x86_64" ;; \
	  esac; \
	  system="$$arch-$$os"; \
	  profiles="$$( $(PROFILE_METADATA_CMD) )"; \
	  if [ -z "$$profiles" ]; then \
	    echo "Error: failed to read homeConfigurations from flake.nix." >&2; \
	    exit 1; \
	  fi; \
	  profile="$$(printf '%s\n' "$$profiles" | awk -F '\t' \
	    -v username="$$username" \
	    -v hostname="$$hostname_lc" \
	    -v target_system="$$system" \
	    -v home_dir="$$home_dir" \
	    ' \
	      { \
	        candidate = $$1; \
	        candidateUser = $$2; \
	        candidateSystem = $$3; \
	        candidateHome = $$4; \
	        candidateLc = tolower(candidate); \
	        if (candidateLc == hostname) { \
	          exactHost = candidate; \
	        } else if (hostMatch == "" && candidateLc ~ ("(^|-)" hostname "($$|-)")) { \
	          hostMatch = candidate; \
	        } \
	        if (candidateUser == username && candidateSystem == target_system) { \
	          metaMatch = candidate; \
	          metaCount++; \
	          if (candidateHome == home_dir) { \
	            homeMatch = candidate; \
	            homeCount++; \
	          } \
	        } \
	      } \
	      END { \
	        if (exactHost != "") { \
	          print exactHost; \
	        } else if (hostMatch != "") { \
	          print hostMatch; \
	        } else if (homeCount == 1) { \
	          print homeMatch; \
	        } else if (metaCount == 1) { \
	          print metaMatch; \
	        } \
	      }' \
	  )"; \
	  if [ -z "$$profile" ]; then \
	    echo "Error: could not determine a unique Home Manager profile." >&2; \
	    echo "Detected username=$$username hostname=$$hostname_short system=$$system home=$$home_dir" >&2; \
	    echo "Available profiles:" >&2; \
	    printf '%s\n' "$$profiles" | cut -f1 | sed 's/^/  - /' >&2; \
	    echo "Set PROFILE=<name> to override." >&2; \
	    exit 1; \
	  fi; \
	fi; \
	printf '%s\n' "$$profile"

build:
	@profile="$$( $(MAKE) --no-print-directory -s print-profile PROFILE='$(PROFILE)' )"; \
	echo "Building Home Manager profile: $$profile"; \
	home-manager build --flake ".#$$profile"

switch:
	@profile="$$( $(MAKE) --no-print-directory -s print-profile PROFILE='$(PROFILE)' )"; \
	echo "Switching Home Manager profile: $$profile"; \
	home-manager switch --flake ".#$$profile"

build-company-mbp:
	@$(MAKE) --no-print-directory build PROFILE=company-mbp

build-home-mbp:
	@$(MAKE) --no-print-directory build PROFILE=home-mbp

build-homelab-devbox:
	@$(MAKE) --no-print-directory build PROFILE=homelab-devbox

build-homelab-openclaw:
	@$(MAKE) --no-print-directory build PROFILE=homelab-openclaw

switch-company-mbp:
	@$(MAKE) --no-print-directory switch PROFILE=company-mbp

switch-home-mbp:
	@$(MAKE) --no-print-directory switch PROFILE=home-mbp

switch-homelab-devbox:
	@$(MAKE) --no-print-directory switch PROFILE=homelab-devbox

switch-homelab-openclaw:
	@$(MAKE) --no-print-directory switch PROFILE=homelab-openclaw
