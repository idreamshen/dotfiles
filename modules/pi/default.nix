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

  extensionFiles = builtins.readDir ./extensions;

  # Static (secret-free) extensions: drop any `.ts` file into ./extensions/ and it
  # is linked into `~/.pi/agent/extensions/` automatically.
  staticExtensions = lib.mapAttrs'
    (name: _: lib.nameValuePair "${extensionsDir}/${name}" {
      source = ./extensions/${name};
    })
    (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".ts" name)
      extensionFiles);

  # Secret-bearing extensions: drop a `<name>.ts.tmpl` file into ./extensions/ using
  # `@sops:<secret>@` markers. Each is rendered via a sops template into
  # `~/.pi/agent/extensions/<name>.ts` so secret values never hit the Nix store.
  # The referenced secrets must be declared under `sops.secrets` below.
  secretPlaceholders = lib.mapAttrs' (name: _: lib.nameValuePair "@sops:${name}@" name)
    config.sops.secrets;
  renderExtension = content: builtins.replaceStrings
    (builtins.attrNames secretPlaceholders)
    (map (name: config.sops.placeholder.${name}) (builtins.attrValues secretPlaceholders))
    content;
  templatedExtensions = lib.mapAttrs'
    (name: _: lib.nameValuePair "pi-extension-${lib.removeSuffix ".ts.tmpl" name}" {
      path = "${config.home.homeDirectory}/${extensionsDir}/${lib.removeSuffix ".tmpl" name}";
      mode = "0600";
      content = renderExtension (builtins.readFile ./extensions/${name});
    })
    (lib.filterAttrs (name: type: type == "regular" && lib.hasSuffix ".ts.tmpl" name)
      extensionFiles);
in {
  config = {
    home.packages = [
      llmAgentsPkgs.pi
      piAcp
    ];

    home.file = staticExtensions;

    sops.secrets.pi_anthropic_base_url = {};
    sops.secrets.pi_anthropic_api_key = {};
    sops.templates = templatedExtensions;
  };
}
