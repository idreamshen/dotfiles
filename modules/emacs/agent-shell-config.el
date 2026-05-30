;;; agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

(use-package agent-shell
  :bind (("C-c s s" . agent-shell-opencode-start-agent)
         ("C-c s c" . agent-shell-anthropic-start-claude-code)
         ("C-c s g" . agent-shell-google-start-gemini)
         ("C-c s x" . agent-shell-openai-start-codex)
         ("C-c s o" . agent-shell-github-start-copilot))
  :init
  (setq agent-shell-opencode-default-model-id "deepseek/deepseek-v4-flash/high"
        agent-shell-openai-default-model-id "gpt-5.5/high"
        agent-shell-openai-default-session-mode-id "full-access"
        ;agent-shell-anthropic-default-model-id "claude-opus-4-7"
        agent-shell-anthropic-default-session-mode-id "bypassPermissions"
        agent-shell-google-gemini-acp-command '("gemini" "--acp" "--approval-mode" "yolo")
        agent-shell-github-acp-command '("copilot" "--acp" "--allow-all"))
  :custom
  (agent-shell-file-completion-enabled t)
  :config
  (defun my/agent-shell--model-config-id-p (config-id)
    (when-let ((option (agent-shell--config-option-by-category
                        (agent-shell--state) "model")))
      (equal config-id (map-elt option :id))))

  (defun my/agent-shell--set-thought-level-high ()
    (when-let* ((option (agent-shell--config-option-by-category
                         (agent-shell--state) "thought_level"))
                ((not (equal (map-elt option :current-value) "high")))
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

(use-package agent-shell-attention
  :after agent-shell
  :config
  (agent-shell-attention-mode))

(provide 'agent-shell-config)

;;; agent-shell-config.el ends here
