{ config, pkgs, lib, llmAgents, ... }:

let
  cfg = config.programs.pi;
  llmAgentsPkgs = llmAgents.packages.${pkgs.system};

  piAcp = pkgs.buildNpmPackage {
    pname = "pi-acp";
    version = "0.0.31";

    src = pkgs.fetchFromGitHub {
      owner = "svkozak";
      repo = "pi-acp";
      rev = "v0.0.31";
      hash = "sha256-bM3V/3fxkY2Ib+OyfT82StIIRSLXGDuYUbt1CZKpTuo=";
    };

    patches = [
      ./patches/pi-acp-usage-update.patch
    ];

    npmDepsHash = "sha256-qN+b/tMbnJLkWjotl3XrA0nfZ3KT/mT6gM+n3Qiz8Wk=";
  };

  extensionsDir = ".pi/agent/extensions";
  # Extension files previously managed here that must be pruned after renames.
  removedPiExtensions = [
    "anthropic-provider.ts"
  ];

  # All `.ts` files in ./extensions/ are pi extensions. A file that contains a
  # `@sops:<secret>@` marker is secret-bearing and rendered via a sops template;
  # everything else is linked directly from the Nix store.
  tsExtensions = lib.filterAttrs
    (name: type: type == "regular" && lib.hasSuffix ".ts" name)
    (builtins.readDir ./extensions);
  hasSecretMarker = name: lib.hasInfix "@sops:" (builtins.readFile ./extensions/${name});

  # Static (secret-free) extensions: linked straight into `~/.pi/agent/extensions/`.
  staticExtensions = lib.mapAttrs'
    (name: _: lib.nameValuePair "${extensionsDir}/${name}" {
      source = ./extensions/${name};
    })
    (lib.filterAttrs (name: _: !hasSecretMarker name) tsExtensions);

  # Secret-bearing extensions: `@sops:<secret>@` markers are substituted with sops
  # placeholders and the file is rendered via a sops template into
  # `~/.pi/agent/extensions/<name>` so secret values never hit the Nix store.
  # The referenced secrets must be declared under `sops.secrets` below.
  secretPlaceholders = lib.mapAttrs' (name: _: lib.nameValuePair "@sops:${name}@" name)
    config.sops.secrets;
  renderExtension = content: builtins.replaceStrings
    (builtins.attrNames secretPlaceholders)
    (map (name: config.sops.placeholder.${name}) (builtins.attrValues secretPlaceholders))
    content;
  templatedExtensions = lib.mapAttrs'
    (name: _: lib.nameValuePair "pi-extension-${lib.removeSuffix ".ts" name}" {
      path = "${config.home.homeDirectory}/${extensionsDir}/${name}";
      mode = "0600";
      content = renderExtension (builtins.readFile ./extensions/${name});
    })
    (lib.filterAttrs (name: _: hasSecretMarker name) tsExtensions);

  # Project-local CPA credential overrides. Each entry renders the shared
  # `templates/cpa-override.ts` into a project's `.pi/extensions/`, substituting
  # `@cpa_base_url@` / `@cpa_api_key@` with that project's sops secret
  # placeholders. The override merges into the global `cpa` provider (see the
  # extension for details), so only base URL + token change per project.
  cpaOverrideTemplate = builtins.readFile ./templates/cpa-override.ts;
  renderCpaOverride = baseUrlSecret: apiKeySecret: builtins.replaceStrings
    [ "@cpa_base_url@" "@cpa_api_key@" ]
    [ config.sops.placeholder.${baseUrlSecret} config.sops.placeholder.${apiKeySecret} ]
    cpaOverrideTemplate;
  cpaProjectOverrides = {
    feedme = {
      projectDir = "projects/feedme";
      baseUrlSecret = "pi_cpa_feedme_base_url";
      apiKeySecret = "pi_cpa_feedme_api_key";
    };
    home-manager = {
      projectDir = ".config/home-manager";
      baseUrlSecret = "pi_cpa_home_manager_base_url";
      apiKeySecret = "pi_cpa_home_manager_api_key";
    };
  };
  cpaProjectSecrets = lib.optionalAttrs cfg.projectScopedCpa.enable (
    lib.foldl' (acc: o: acc // { ${o.baseUrlSecret} = {}; ${o.apiKeySecret} = {}; })
      {}
      (lib.attrValues cpaProjectOverrides));
  cpaProjectTemplates = lib.optionalAttrs cfg.projectScopedCpa.enable (
    lib.mapAttrs'
      (name: o: lib.nameValuePair "pi-extension-cpa-override-${name}" {
        path = "${config.home.homeDirectory}/${o.projectDir}/.pi/extensions/cpa-override.ts";
        mode = "0600";
        content = renderCpaOverride o.baseUrlSecret o.apiKeySecret;
      })
      cpaProjectOverrides);
in {
  options.programs.pi.projectScopedCpa.enable = lib.mkEnableOption
    "per-project CPA base URL + token overrides via project-local pi extensions";

  config = {
    home.packages = [
      llmAgentsPkgs.pi
      piAcp
    ];

    home.file = staticExtensions // {
      ".pi/agent/settings.json" = {
        source = ./settings.json;
        force = true;
      };
    };

    home.activation.pruneRemovedPiExtensions = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      extensions="${config.home.homeDirectory}/${extensionsDir}"
      for extension in ${lib.escapeShellArgs removedPiExtensions}; do
        ${pkgs.coreutils}/bin/rm -f "$extensions/$extension"
      done
    '';


    sops.secrets = {
      pi_cpa_base_url = {};
      pi_cpa_api_key = {};
    } // cpaProjectSecrets;
    sops.templates = templatedExtensions // cpaProjectTemplates;
  };
}
