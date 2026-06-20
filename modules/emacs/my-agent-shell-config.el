;;; my-agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

(require 'cl-lib)

(use-package agent-shell-macext
  :if (eq system-type 'darwin)
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-notifications nil))

(use-package agent-shell
  :bind (   ("C-c s s" . my/agent-shell-opencode-start-or-switch)
         ("C-c s c" . agent-shell-anthropic-start-claude-code)
         ("C-c s g" . agent-shell-google-start-gemini)
         ("C-c s x" . agent-shell-openai-start-codex)
         ("C-c s o" . agent-shell-github-start-copilot))
  :init
  (setq agent-shell-opencode-default-model-id "openai/gpt-5.5/high"
        agent-shell-opencode-default-session-mode-id "plan"
        agent-shell-openai-default-model-id "gpt-5.5"
        agent-shell-openai-default-session-mode-id "full-access"
        ;agent-shell-anthropic-default-model-id "claude-opus-4-7"
        agent-shell-anthropic-default-session-mode-id "bypassPermissions"
        agent-shell-google-gemini-acp-command '("gemini" "--acp" "--approval-mode" "yolo")
        agent-shell-github-acp-command '("copilot" "--acp" "--allow-all"))
  :custom
  (agent-shell-file-completion-enabled t)
  :config
  (defun my/agent-shell--find-opencode-buffer ()
    (when (fboundp 'agent-shell-project-buffers)
      (seq-find
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (with-current-buffer buffer
                (derived-mode-p 'agent-shell-mode))
              (let ((config (ignore-errors (agent-shell-get-config buffer))))
                (eq (alist-get :identifier config) 'opencode))))
       (agent-shell-project-buffers))))

  (defun my/agent-shell-opencode-start-or-switch ()
    (interactive)
    (if-let ((buffer (my/agent-shell--find-opencode-buffer)))
        (progn
          (if (fboundp 'agent-shell--display-buffer)
              (agent-shell--display-buffer buffer)
            (switch-to-buffer buffer))
          (message "Switched to opencode session: %s" (buffer-name buffer)))
      (call-interactively #'agent-shell-opencode-start-agent)))

  (defun my/agent-shell--remote-command-available-p (command)
    (and (file-remote-p default-directory)
         (let ((process-environment
                (if (boundp 'my/remote-exec-path)
                    (cons (concat "PATH=" (mapconcat #'identity my/remote-exec-path ":"))
                          process-environment)
                  process-environment)))
           (zerop (process-file "sh" nil nil nil "-lc"
                                (format "command -v %s >/dev/null"
                                        (shell-quote-argument command)))))))

  (defun my/agent-shell--acp-start-client-with-remote-path (orig-fun &rest args)
    (if (file-remote-p default-directory)
        (let ((original-executable-find (symbol-function 'executable-find)))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (command &optional remote)
                       (or (funcall original-executable-find command remote)
                           (when (and remote
                                      (my/agent-shell--remote-command-available-p command))
                             command)))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-remove 'acp--start-client
                 #'my/agent-shell--acp-start-client-with-remote-path)
  (advice-add 'acp--start-client
              :around #'my/agent-shell--acp-start-client-with-remote-path)

  (defun my/agent-shell--model-config-id-p (config-id)
    (when-let ((option (agent-shell--config-option-by-category
                        (agent-shell--state) "model")))
      (equal config-id (map-elt option :id))))

  (defun my/agent-shell--set-thought-level-high ()
    (when-let* ((option (agent-shell--config-option-by-category
                         (agent-shell--state) "thought_level"))
                ((seq-find (lambda (value)
                             (equal (map-elt value :value) "high"))
                           (map-elt option :options))))
      (agent-shell--config-option-set-thought-level-id
       :thought-level-id "high")))

  (defun my/agent-shell--model-change-on-success (on-success)
    (when on-success
      (funcall on-success))
    (my/agent-shell--set-thought-level-high))

  (defun my/agent-shell--set-effort-high-after-model-change (orig-fun &rest args)
    (if (my/agent-shell--model-config-id-p (plist-get args :config-id))
        (apply orig-fun
               (plist-put (copy-sequence args)
                          :on-success
                          (apply-partially
                           #'my/agent-shell--model-change-on-success
                           (plist-get args :on-success))))
      (apply orig-fun args)))

  (advice-remove 'agent-shell--set-session-config-option
                 #'my/agent-shell--set-effort-high-after-model-change)
  (advice-add 'agent-shell--set-session-config-option
              :around #'my/agent-shell--set-effort-high-after-model-change))

(use-package agent-shell-tramp
  :after agent-shell
  :config
  (agent-shell-tramp-mode 1))

(use-package agent-shell-attention
  :after agent-shell
  :config
  (agent-shell-attention-mode))

(provide 'my-agent-shell-config)

;;; my-agent-shell-config.el ends here
