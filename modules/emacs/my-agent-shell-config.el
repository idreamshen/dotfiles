;;; my-agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

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

  (defvar my/agent-shell-opencode-deepseek-response-grace 1.0
    "Seconds to keep DeepSeek/OpenCode prompt requests active after response.")

  (defun my/agent-shell--opencode-deepseek-prompt-request-p (state request)
    (and (equal (map-elt request :method) "session/prompt")
         (eq (map-nested-elt state '(:agent-config :identifier)) 'opencode)
         (let ((model-id (or (map-nested-elt state '(:session :model-id))
                             (when-let ((default-model-id (map-nested-elt state '(:agent-config :default-model-id))))
                               (funcall default-model-id)))))
           (and (stringp model-id)
                (string-prefix-p "deepseek/" model-id)))))

  (defun my/agent-shell--remove-active-request (state request)
    (map-put! state :active-requests
              (seq-remove (lambda (active-request)
                            (equal active-request request))
                          (map-elt state :active-requests))))

  (defun my/agent-shell--keep-active-request (state request)
    (my/agent-shell--remove-active-request state request)
    (map-put! state :active-requests
              (cons request (map-elt state :active-requests))))

  (defun my/agent-shell--delay-deepseek-prompt-completion (orig-fun &rest args)
    (let ((state (plist-get args :state))
          (request (plist-get args :request))
          (on-success (plist-get args :on-success))
          (buffer (plist-get args :buffer)))
      (if (my/agent-shell--opencode-deepseek-prompt-request-p state request)
          (apply orig-fun
                 (plist-put (copy-sequence args)
                            :on-success
                            (lambda (acp-response)
                              (my/agent-shell--keep-active-request state request)
                              (run-at-time my/agent-shell-opencode-deepseek-response-grace
                                           nil
                                           (lambda ()
                                             (when (buffer-live-p buffer)
                                               (with-current-buffer buffer
                                                 (when on-success
                                                   (funcall on-success acp-response))
                                                 (my/agent-shell--remove-active-request
                                                  state request))))))))
        (apply orig-fun args))))

  (advice-remove 'agent-shell--set-session-config-option
                 #'my/agent-shell--set-effort-high-after-model-change)
  (advice-add 'agent-shell--set-session-config-option
              :around #'my/agent-shell--set-effort-high-after-model-change)
  (advice-remove 'agent-shell--send-request
                 #'my/agent-shell--delay-deepseek-prompt-completion)
  (advice-add 'agent-shell--send-request
              :around #'my/agent-shell--delay-deepseek-prompt-completion))

(use-package agent-shell-attention
  :after agent-shell
  :config
  (agent-shell-attention-mode))

(provide 'my-agent-shell-config)

;;; my-agent-shell-config.el ends here
