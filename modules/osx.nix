{ config, pkgs, lib, ... }:

let
  inherit (pkgs) stdenv;
  emacsPkg = config.programs.emacs.finalPackage;
in {
  home.packages = lib.mkIf stdenv.isDarwin [
    pkgs.iterm2
    pkgs.pngpaste
  ];

  # Clickable launcher that opens a GUI frame on the running Emacs daemon
  # (services.emacs) instead of starting a second, standalone Emacs.app.
  # Launch it from Spotlight/Dock; every frame shares the one daemon process.
  home.file = lib.mkIf stdenv.isDarwin {
    "Applications/EmacsClient.app/Contents/Info.plist".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>CFBundleName</key>
        <string>EmacsClient</string>
        <key>CFBundleDisplayName</key>
        <string>EmacsClient</string>
        <key>CFBundleIdentifier</key>
        <string>org.gnu.EmacsClient</string>
        <key>CFBundleExecutable</key>
        <string>EmacsClient</string>
        <key>CFBundlePackageType</key>
        <string>APPL</string>
        <key>CFBundleVersion</key>
        <string>1.0</string>
        <key>CFBundleIconFile</key>
        <string>Emacs.icns</string>
      </dict>
      </plist>
    '';

    "Applications/EmacsClient.app/Contents/MacOS/EmacsClient" = {
      executable = true;
      text = ''
        #!/bin/sh
        # -c new frame, -n return immediately, -a "" auto-start daemon if down.
        exec "${emacsPkg}/bin/emacsclient" -c -n -a "" "$@"
      '';
    };

    "Applications/EmacsClient.app/Contents/Resources/Emacs.icns".source =
      "${emacsPkg}/Applications/Emacs.app/Contents/Resources/Emacs.icns";

    ".iterm2_shell_integration.zsh".source =
      "${pkgs.iterm2}/Applications/iTerm2.app/Contents/Resources/iterm2_shell_integration.zsh";
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

        tmux new-window -t "$session" -n "dev-emacs" \
          'ssh -t devbox "emacsclient -nw -a \"\""'

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
