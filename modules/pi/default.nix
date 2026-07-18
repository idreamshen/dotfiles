{ config, pkgs, lib, llmAgents, ... }:

let
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

    npmDepsHash = "sha256-qN+b/tMbnJLkWjotl3XrA0nfZ3KT/mT6gM+n3Qiz8Wk=";
  };

  extensionsDir = ".pi/agent/extensions";

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
in {
  config = {
    home.packages = [
      llmAgentsPkgs.pi
      piAcp
    ];

    home.file = staticExtensions;

    home.sessionVariables.HYPA_PI_MODE = "replace";

    home.activation.ensurePiHypaPackage = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      settings="${config.home.homeDirectory}/.pi/agent/settings.json"
      mkdir -p "$(dirname "$settings")"

      if [ ! -f "$settings" ]; then
        printf '{}\n' > "$settings"
      fi

      tmp="$(${pkgs.coreutils}/bin/mktemp)"
      ${pkgs.jq}/bin/jq '
        .packages = ((.packages // []) as $packages |
          if any($packages[]?; . == "npm:@hypabolic/pi-hypa") then
            $packages
          else
            $packages + ["npm:@hypabolic/pi-hypa"]
          end)
      ' "$settings" > "$tmp"
      ${pkgs.coreutils}/bin/mv "$tmp" "$settings"
    '';

    sops.secrets.pi_anthropic_base_url = {};
    sops.secrets.pi_anthropic_api_key = {};
    sops.templates = templatedExtensions;
  };
}
