{ pkgs, lib, ... }:

let
  inherit (pkgs) stdenv;
in {
  home.packages = lib.mkIf stdenv.isDarwin [
    pkgs.iterm2
  ];

  home.file.".iterm2_shell_integration.zsh" = lib.mkIf stdenv.isDarwin {
    source = "${pkgs.iterm2}/Applications/iTerm2.app/Contents/Resources/iterm2_shell_integration.zsh";
  };

  programs.zsh.initContent = lib.mkIf stdenv.isDarwin (lib.mkAfter ''
    # iTerm2 shell integration
    if [ "$TERM_PROGRAM" = "iTerm.app" ]; then
      export PATH="${pkgs.iterm2}/Applications/iTerm2.app/Contents/Resources:$PATH"
      source "$HOME/.iterm2_shell_integration.zsh"
    fi
  '');
}
