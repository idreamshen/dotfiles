;;; my-agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

(use-package agent-shell
  :bind (   ("C-c s s" . my/agent-shell-opencode-start-or-switch)
         ("C-c s c" . agent-shell-anthropic-start-claude-code)
         ("C-c s g" . agent-shell-google-start-gemini)
         ("C-c s x" . agent-shell-openai-start-codex)
         ("C-c s o" . agent-shell-github-start-copilot))
  :init
  (setq agent-shell-opencode-default-model-id "openai/gpt-5.5/high"
        agent-shell-opencode-default-session-mode-id "plan"
        agent-shell-openai-default-model-id "gpt-5.5/high"
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
    "Seconds to keep DeepSeek/OpenCode prompt requests active after last update.")

  (defun my/agent-shell--state-put (state key value)
    "Set KEY to VALUE in STATE alist, growing it in place if needed.
`map-put!' can only mutate existing keys of an alist; adding a new
key signals `map-not-inplace'.  Mirror agent-shell's own migration
trick of `nconc'ing a placeholder cons so the alist head is preserved
and all references observe the new key."
    (unless (assq key state)
      (nconc state (list (cons key nil))))
    (map-put! state key value))

  (defun my/agent-shell--opencode-deepseek-prompt-request-p (state request)
    (and (equal (map-elt request :method) "session/prompt")
         (eq (map-nested-elt state '(:agent-config :identifier)) 'opencode)
         (let ((model-id (or (map-nested-elt state '(:session :model-id))
                             (when-let ((default-model-id (map-nested-elt state '(:agent-config :default-model-id))))
                               (funcall default-model-id)))))
           (and (stringp model-id)
                (string-prefix-p "deepseek/" model-id)))))

  (defun my/agent-shell--remove-active-request (state request)
    (my/agent-shell--state-put state :active-requests
              (seq-remove (lambda (active-request)
                            (equal active-request request))
                          (map-elt state :active-requests))))

  (defun my/agent-shell--keep-active-request (state request)
    (my/agent-shell--remove-active-request state request)
    (my/agent-shell--state-put state :active-requests
              (cons request (map-elt state :active-requests))))

  (defun my/agent-shell--run-delayed-prompt-completion (state request acp-response on-success buffer)
    (unwind-protect
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when on-success
              (funcall on-success acp-response))))
      (my/agent-shell--remove-active-request state request)
      (my/agent-shell--state-put state :my/deepseek-prompt-completion nil)))

  (defun my/agent-shell--schedule-delayed-prompt-completion (state request acp-response on-success buffer)
    (when-let ((timer (map-nested-elt state '(:my/deepseek-prompt-completion :timer))))
      (cancel-timer timer))
    (my/agent-shell--keep-active-request state request)
    (my/agent-shell--state-put state :my/deepseek-prompt-completion
              `((:request . ,request)
                (:acp-response . ,acp-response)
                (:on-success . ,on-success)
                (:buffer . ,buffer)
                (:timer . ,(run-at-time
                            my/agent-shell-opencode-deepseek-response-grace
                            nil
                            #'my/agent-shell--run-delayed-prompt-completion
                            state request acp-response on-success buffer)))))

  (defun my/agent-shell--reset-deepseek-prompt-completion-on-update (orig-fun &rest args)
    (let* ((state (plist-get args :state))
           (completion (map-elt state :my/deepseek-prompt-completion)))
      (when (and completion
                 (equal (map-elt (plist-get args :acp-notification) 'method)
                        "session/update"))
        (my/agent-shell--schedule-delayed-prompt-completion
         state
         (map-elt completion :request)
         (map-elt completion :acp-response)
         (map-elt completion :on-success)
         (map-elt completion :buffer)))
      (apply orig-fun args)))

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
                               (my/agent-shell--schedule-delayed-prompt-completion
                                state request acp-response on-success buffer))))
        (apply orig-fun args))))

  (advice-remove 'agent-shell--set-session-config-option
                 #'my/agent-shell--set-effort-high-after-model-change)
  (advice-add 'agent-shell--set-session-config-option
              :around #'my/agent-shell--set-effort-high-after-model-change)
  (advice-remove 'agent-shell--send-request
                  #'my/agent-shell--delay-deepseek-prompt-completion)
  (advice-add 'agent-shell--send-request
              :around #'my/agent-shell--delay-deepseek-prompt-completion)
  (advice-remove 'agent-shell--on-notification
                 #'my/agent-shell--reset-deepseek-prompt-completion-on-update)
  (advice-add 'agent-shell--on-notification
              :around #'my/agent-shell--reset-deepseek-prompt-completion-on-update))

(use-package agent-shell-attention
  :after agent-shell
  :config
  (agent-shell-attention-mode))

(provide 'my-agent-shell-config)

;;; my-agent-shell-config.el ends here
