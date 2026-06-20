;;; my-agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

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

;;; ACP elicitation support (enables the native AskUserQuestion tool).
;;
;; claude-agent-acp disables the AskUserQuestion tool unless the connected
;; client advertises `clientCapabilities.elicitation.form'; when present it
;; renders the tool's questions as an ACP `elicitation/create' form request.
;; acp.el/agent-shell implement neither, so Claude can only ask in prose here.
;; Below we (1) advertise the capability on `initialize' and (2) handle the
;; incoming `elicitation/create' request by rendering its JSON-Schema form with
;; `completing-read'.  This also covers MCP-server-originated elicitations.
;;
;; NOTE: ACP's elicitation surface is UNSTABLE; this advice is coupled to the
;; current shapes of claude-agent-acp 0.47.0 / acp.el 0.12.2.

(defun my/agent-shell--elicitation-add-capability (request)
  "Inject the elicitation client capability into initialize REQUEST.
REQUEST is the value returned by `acp-make-initialize-request'.

The ACP `ElicitationCapabilities.form' field is an OBJECT (a marker with an
optional `_meta'), not a boolean: claude-agent-acp validates it with zod and
silently drops the whole `elicitation' capability on a type mismatch.  We
therefore advertise `form' as an empty JSON object (a hash table serializes to
`{}'), which the agent reads as truthy and uses to enable AskUserQuestion."
  (let* ((params (alist-get :params request))
         (caps (and params (alist-get 'clientCapabilities params))))
    (when (and (listp caps) (null (alist-get 'elicitation caps)))
      (setf (alist-get 'elicitation caps)
            (list (cons 'form (make-hash-table :test 'equal))))
      (setf (alist-get 'clientCapabilities params) caps)))
  request)

(defconst my/agent-shell--elicitation-other-label
  "✎ Other (type a custom answer)"
  "Sentinel choice offered alongside enum options.")

(defun my/agent-shell--elicitation-enum-options (spec)
  "Return a list of (TITLE . CONST) for enum SPEC (with `oneOf' or `items.anyOf')."
  (let ((opts (or (alist-get 'oneOf spec)
                  (map-nested-elt spec '(items anyOf)))))
    (mapcar (lambda (opt)
              (cons (or (alist-get 'title opt) (alist-get 'const opt))
                    (alist-get 'const opt)))
            (append opts nil))))

(defun my/agent-shell--elicitation-read-field (key spec message single has-custom)
  "Read one elicitation field KEY described by SPEC.
MESSAGE is the form-level prompt, SINGLE non-nil when this is the only
question, HAS-CUSTOM non-nil when a sibling \"_custom\" field exists.
Return the chosen value, nil to skip, or (:custom . STRING) for a typed
custom answer."
  (let* ((type (alist-get 'type spec))
         (prompt (format "%s: "
                         (or (alist-get 'description spec)
                             (and single message)
                             (alist-get 'title spec)
                             (symbol-name key))))
         (enum (my/agent-shell--elicitation-enum-options spec)))
    (cond
     ;; Multi-select (array of enum).
     ((equal type "array")
      (let ((chosen (completing-read-multiple prompt (mapcar #'car enum) nil t)))
        (vconcat (delq nil (mapcar (lambda (tit) (alist-get tit enum nil nil #'equal))
                                   chosen)))))
     ;; Single-select (enum).
     (enum
      (let* ((titles (append (mapcar #'car enum)
                             (when has-custom
                               (list my/agent-shell--elicitation-other-label))))
             (choice (completing-read prompt titles nil t)))
        (if (equal choice my/agent-shell--elicitation-other-label)
            (cons :custom (read-string
                           (format "%s (custom): "
                                   (or (alist-get 'title spec) "Answer"))))
          (alist-get choice enum nil nil #'equal))))
     ;; Plain boolean.
     ((equal type "boolean")
      (if (y-or-n-p prompt) t :false))
     ;; Plain number / integer.
     ((member type '("number" "integer"))
      (let ((s (read-string prompt)))
        (unless (string-empty-p s) (string-to-number s))))
     ;; Plain string (free-form).
     (t
      (let ((s (read-string prompt)))
        (unless (string-empty-p s) s))))))

(defun my/agent-shell--render-elicitation (params)
  "Render the ACP elicitation form described by PARAMS.
Return a `CreateElicitationResponse' result alist."
  (let* ((message (alist-get 'message params))
         (props (map-nested-elt params '(requestedSchema properties)))
         (non-custom (seq-remove (lambda (kv)
                                   (string-suffix-p "_custom" (symbol-name (car kv))))
                                 props))
         (single (= (length non-custom) 1))
         (content '()))
    (when (and message (not single))
      (message "%s" message))
    (dolist (kv non-custom)
      (let* ((key (car kv))
             (spec (cdr kv))
             (custom-key (intern (concat (symbol-name key) "_custom")))
             (has-custom (and (assq custom-key props) t))
             (val (my/agent-shell--elicitation-read-field
                   key spec message single has-custom)))
        (cond
         ((and (consp val) (eq (car val) :custom))
          (unless (string-empty-p (cdr val))
            (push (cons custom-key (cdr val)) content)))
         ((null val) nil)
         ((and (vectorp val) (zerop (length val))) nil)
         (t (push (cons key val) content)))))
    (if content
        `((action . "accept") (content . ,(nreverse content)))
      '((action . "decline")))))

(cl-defun my/agent-shell--on-request-elicitation (orig-fun &rest args)
  "Around advice for `agent-shell--on-request' handling `elicitation/create'.
Falls through to ORIG-FUN for every other method."
  (let ((acp-request (plist-get args :acp-request))
        (state (plist-get args :state)))
    (if (equal (map-elt acp-request 'method) "elicitation/create")
        (let ((result (condition-case err
                          (my/agent-shell--render-elicitation
                           (map-elt acp-request 'params))
                        (quit '((action . "cancel")))
                        (error
                         (message "agent-shell elicitation error: %S" err)
                         '((action . "cancel"))))))
          (acp-send-response
           :client (map-elt state :client)
           :response `((:request-id . ,(map-elt acp-request 'id))
                       (:result . ,result))))
      (apply orig-fun args))))

(with-eval-after-load 'acp
  (advice-remove 'acp-make-initialize-request
                 #'my/agent-shell--elicitation-add-capability)
  (advice-add 'acp-make-initialize-request :filter-return
              #'my/agent-shell--elicitation-add-capability))

(with-eval-after-load 'agent-shell
  (advice-remove 'agent-shell--on-request
                 #'my/agent-shell--on-request-elicitation)
  (advice-add 'agent-shell--on-request :around
              #'my/agent-shell--on-request-elicitation))

(provide 'my-agent-shell-config)

;;; my-agent-shell-config.el ends here
