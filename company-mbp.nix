{ ... }:

{
  imports = [
    ./modules/common.nix
    ./modules/dev-machine.nix
    ./modules/osx.nix
  ];

  programs.opencode.anthropicApiKeySecret = "sub2api_company_anthropic_api_key";
}
