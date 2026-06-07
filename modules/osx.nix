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

    emux() {
      local session="emux"

      if ! tmux has-session -t "$session" 2>/dev/null; then
        tmux new-session -d -s "$session" -n "mac-emacs" 'TERM=xterm-direct emacsclient -nw -a ""'

        local remote_cmd=$'tmux new-session -A -s emacs "emacsclient -nw -a \"\""'
        tmux new-window -t "$session" -n "devbox-emacs" \
          "ssh -t devbox '$remote_cmd'"

        tmux new-window -t "$session" -n "devbox-shell" 'ssh devbox'
        tmux select-window -t "$session:mac-emacs"
      fi

      if [ -z "$TMUX" ]; then
        tmux attach-session -t "$session"
      else
        tmux switch-client -t "$session"
      fi
    }
  '');
}
