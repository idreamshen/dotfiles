;;; my-agent-shell-config.el --- Agent shell configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defcustom my/agent-shell-render-markdown-during-streaming nil
  "Whether to render agent response Markdown while chunks are streaming.
When nil, agent response chunks are inserted as raw Markdown and rendered once
when the surrounding `session/prompt' or `session/load' request completes.  This
avoids running agent-shell's Markdown renderer for every streamed response
chunk."
  :type 'boolean
  :group 'agent-shell)

(defvar my/agent-shell--suppress-markdown-render nil
  "Dynamically bound non-nil to skip `agent-shell--render-markdown'.")

(defcustom my/agent-shell-heartbeat-beats-per-second 2
  "Heartbeat frequency (ticks per second) for agent-shell sessions.
agent-shell hardcodes this to 10, which redraws the header/mode-line every
100ms while the session is `busy', pinning a CPU core for the whole run.
Lowering it (e.g. 2 = every 500ms) cuts that idle-render cost; the spinner
animation just advances more slowly.  nil leaves the upstream default (10)."
  :type '(choice (const :tag "Upstream default (10)" nil) number)
  :group 'agent-shell)

(defun my/agent-shell--throttle-heartbeat (args)
  "Override :beats-per-second in ARGS for `agent-shell-heartbeat-make'.
ARGS is the keyword argument list; leaves it untouched when
`my/agent-shell-heartbeat-beats-per-second' is nil."
  (if my/agent-shell-heartbeat-beats-per-second
      (plist-put (copy-sequence args)
                 :beats-per-second
                 my/agent-shell-heartbeat-beats-per-second)
    args))

(with-eval-after-load 'agent-shell-heartbeat
  (advice-remove 'agent-shell-heartbeat-make
                 #'my/agent-shell--throttle-heartbeat)
  (advice-add 'agent-shell-heartbeat-make :filter-args
              #'my/agent-shell--throttle-heartbeat))

(defvar-local my/agent-shell--deferred-markdown-fragments nil
  "Qualified agent response fragment ids whose Markdown render is deferred.")

(use-package agent-shell-macext
  :if (eq system-type 'darwin)
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-notifications nil)
  :config
  (defun my/agent-shell-macext-yank-tramp-safe (orig-fun &optional arg)
    "Run `agent-shell-macext-yank' with a local `default-directory' when remote.
`agent-shell-macext-yank' calls `file-exists-p' on the trimmed clipboard text to
decide whether to attach it as a file.  When a multiline block (e.g. an org
`*** AI Context' subtree) is pasted into a remote agent-shell buffer, that probe
expands the text against the remote cwd and signals \"Not a Tramp file name\".
Rebinding `default-directory' to a local path keeps the probe on the local
file-name handler (a real local/remote absolute path still resolves correctly),
without redefining the `file-exists-p' primitive (which warns under native
compilation)."
    (if (my/agent-shell--remote-session-p)
        (let ((default-directory temporary-file-directory))
          (funcall orig-fun arg))
      (funcall orig-fun arg)))

  (advice-remove 'agent-shell-macext-yank
                 #'my/agent-shell-macext-yank-tramp-safe)
  (advice-add 'agent-shell-macext-yank
              :around #'my/agent-shell-macext-yank-tramp-safe))

(use-package agent-shell
  :bind (("C-c s c" . agent-shell-anthropic-start-claude-code)
         ("C-c s g" . agent-shell-google-start-gemini)
         ("C-c s x" . agent-shell-openai-start-codex)
         ("C-c s o" . agent-shell-github-start-copilot))
  :init
  (setq agent-shell-openai-default-model-id "gpt-5.5"
        agent-shell-openai-default-session-mode-id "full-access"
        agent-shell-anthropic-default-model-id "opus"
        agent-shell-anthropic-default-session-mode-id "bypassPermissions"
        agent-shell-session-restore-verbosity 'full
        agent-shell-highlight-blocks nil
        agent-shell-google-gemini-acp-command '("gemini" "--acp" "--approval-mode" "yolo")
        agent-shell-github-acp-command '("copilot" "--acp" "--allow-all"))
  :custom
  (agent-shell-file-completion-enabled t)
  :config
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

  (defun my/agent-shell--file-remote-p-safe (path)
    "Return non-nil when PATH is remote, ignoring malformed values."
    (and (stringp path)
         (ignore-errors (file-remote-p path))))

  (defun my/agent-shell--cwd-safe ()
    "Return `agent-shell-cwd' when available, ignoring errors."
    (when (fboundp 'agent-shell-cwd)
      (ignore-errors (agent-shell-cwd))))

  (defun my/agent-shell--remote-session-p ()
    "Return non-nil when the current agent-shell session is remote."
    (or (my/agent-shell--file-remote-p-safe default-directory)
        (my/agent-shell--file-remote-p-safe (my/agent-shell--cwd-safe))))

  (defvar my/agent-shell-anthropic-thought-level "high"
    "Preferred thought level for Anthropic (Claude Code) agent-shell sessions.
Re-applied after the model is (re)selected, since selecting a model resets the
session thought level.  Must be a value the agent advertises for the
`thought_level' config option (e.g. \"low\" \"medium\" \"high\" \"xhigh\" \"max\");
nil leaves the agent default untouched.")

  (defun my/agent-shell--model-config-id-p (config-id)
    (when-let ((option (agent-shell--config-option-by-category
                        (agent-shell--state) "model")))
      (equal config-id (map-elt option :id))))

  (defun my/agent-shell--anthropic-session-p ()
    "Return non-nil when the current agent-shell session is Claude Code."
    (eq (map-elt (map-elt (agent-shell--state) :agent-config) :identifier)
        'claude-code))

  (defun my/agent-shell--apply-anthropic-thought-level ()
    "Apply `my/agent-shell-anthropic-thought-level' to the current session.
No-op unless the session is Anthropic (Claude Code) and the agent advertises
the configured level."
    (when (and my/agent-shell-anthropic-thought-level
               (my/agent-shell--anthropic-session-p)
               (seq-find (lambda (value)
                           (equal (map-elt value :value)
                                  my/agent-shell-anthropic-thought-level))
                         (agent-shell--get-available-thought-levels
                          (agent-shell--state))))
      (agent-shell--config-option-set-thought-level-id
       :thought-level-id my/agent-shell-anthropic-thought-level)))

  (defun my/agent-shell--model-change-on-success (on-success)
    (when on-success
      (funcall on-success))
    (my/agent-shell--apply-anthropic-thought-level))

  (defun my/agent-shell--apply-thought-level-after-model-change (orig-fun &rest args)
    (if (my/agent-shell--model-config-id-p (plist-get args :config-id))
        (apply orig-fun
               (plist-put (copy-sequence args)
                          :on-success
                          (apply-partially
                           #'my/agent-shell--model-change-on-success
                           (plist-get args :on-success))))
      (apply orig-fun args)))

  (defun my/agent-shell--request-method (request)
    "Return ACP REQUEST method, accepting keyword or symbol alist keys."
    (or (map-elt request :method)
        (map-elt request 'method)))

  (defun my/agent-shell--active-request-method-p (state methods)
    "Return non-nil when STATE has an active request whose method is in METHODS."
    (seq-find (lambda (request)
                (member (my/agent-shell--request-method request) methods))
              (map-elt state :active-requests)))

  (defun my/agent-shell--defer-markdown-fragment-p (args)
    "Return non-nil when ARGS describe a streaming fragment body.
This covers agent response chunks, thinking chunks, and tool-call bodies.  It
intentionally skips permission prompt fragments because those contain
interactive buttons/properties rather than plain Markdown output."
    (let* ((state (plist-get args :state))
           (block-id (plist-get args :block-id)))
      (and (not my/agent-shell-render-markdown-during-streaming)
           state
           (plist-get args :body)
           (stringp block-id)
           (not (string-prefix-p "permission-" block-id))
           (or (string-suffix-p "agent_message_chunk" block-id)
               (string-suffix-p "agent_thought_chunk" block-id)
               (map-nested-elt state (list :tool-calls block-id)))
           (my/agent-shell--active-request-method-p
            state '("session/prompt" "session/load")))))

  (defun my/agent-shell--record-deferred-markdown-fragment (state namespace-id block-id)
    "Record fragment NAMESPACE-ID/BLOCK-ID for final Markdown render."
    (when-let ((buffer (map-elt state :buffer)))
      (with-current-buffer buffer
        (let ((qualified-id (format "%s-%s"
                                    (or namespace-id
                                        (map-elt state :request-count))
                                    block-id)))
          (cl-pushnew qualified-id
                      my/agent-shell--deferred-markdown-fragments
                      :test #'equal)))))

  (defun my/agent-shell--maybe-suppress-render-markdown (orig-fun &rest args)
    "Skip Markdown rendering while `my/agent-shell--suppress-markdown-render' is set."
    (unless my/agent-shell--suppress-markdown-render
      (apply orig-fun args)))

  (defun my/agent-shell--update-fragment-defer-markdown (orig-fun &rest args)
    "Around advice for `agent-shell--update-fragment'.
During agent response streaming, insert raw Markdown and defer rendering."
    (if (my/agent-shell--defer-markdown-fragment-p args)
        (let ((state (plist-get args :state))
              (namespace-id (plist-get args :namespace-id))
              (block-id (plist-get args :block-id)))
          (my/agent-shell--record-deferred-markdown-fragment
           state namespace-id block-id)
          ;; Use a global set/unwind-protect instead of `let' because this
          ;; file uses lexical binding and this advice lives inside
          ;; `use-package' expansion; relying on a dynamic `let' here can leave
          ;; `agent-shell--render-markdown' seeing the old global value.
          (let ((old-suppress my/agent-shell--suppress-markdown-render))
            (unwind-protect
                (progn
                  (setq my/agent-shell--suppress-markdown-render t)
                  (apply orig-fun args))
              (setq my/agent-shell--suppress-markdown-render old-suppress))))
      (apply orig-fun args)))

  (defun my/agent-shell--fragment-body-range (qualified-id)
    "Return body range for fragment QUALIFIED-ID in current buffer."
    (save-excursion
      (goto-char (point-max))
      (when-let* ((match
                   (text-property-search-backward
                    'agent-shell-ui-state nil
                    (lambda (_ state)
                      (equal (map-elt state :qualified-id) qualified-id))
                    t))
                  (block-range
                   (agent-shell-ui--block-range
                    :position (prop-match-beginning match))))
        (agent-shell-ui--nearest-range-matching-property
         :property 'agent-shell-ui-section
         :value 'body
         :from (map-elt block-range :start)
         :to (map-elt block-range :end)))))

  (defun my/agent-shell--render-deferred-markdown-in-buffer (qualified-id)
    "Render deferred Markdown for QUALIFIED-ID in current buffer."
    (when-let* ((range (my/agent-shell--fragment-body-range qualified-id))
                (start (map-elt range :start))
                (end (map-elt range :end)))
      (save-excursion
        (save-restriction
          (let ((inhibit-read-only t)
                (my/agent-shell--suppress-markdown-render nil))
            (narrow-to-region start end)
            ;; If collapsed, keep it raw; agent-shell renders collapsed bodies
            ;; when they are expanded via `agent-shell-ui-post-expand-fragment-at-point-hook'.
            (unless (agent-shell-ui--body-invisible-p (point-min) (point-max))
              (agent-shell--render-markdown :render-images t)))))))

  (defun my/agent-shell--render-deferred-markdown-fragments (state)
    "Render all deferred Markdown fragments for STATE."
    (condition-case err
        (when-let ((buffer (map-elt state :buffer)))
          (let (fragments)
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (setq fragments my/agent-shell--deferred-markdown-fragments)
                (setq my/agent-shell--deferred-markdown-fragments nil)))
            (dolist (qualified-id fragments)
              ;; Shell buffer.
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (derived-mode-p 'agent-shell-mode)
                    (my/agent-shell--render-deferred-markdown-in-buffer qualified-id))))
              ;; Viewport buffer, if present.
              (when-let ((viewport-buffer
                          (and (fboundp 'agent-shell-viewport--buffer)
                               (agent-shell-viewport--buffer
                                :shell-buffer buffer
                                :existing-only t))))
                (when (buffer-live-p viewport-buffer)
                  (with-current-buffer viewport-buffer
                    (my/agent-shell--render-deferred-markdown-in-buffer qualified-id)))))))
      (error
       (message "agent-shell deferred markdown render error: %S" err))))

  (defun my/agent-shell--send-request-render-deferred-markdown (orig-fun &rest args)
    "Around advice for `agent-shell--send-request'.
After `session/prompt' or `session/load' completes, render deferred agent response Markdown."
    (let* ((state (plist-get args :state))
           (request (plist-get args :request))
           (method (my/agent-shell--request-method request)))
      (if (not (member method '("session/prompt" "session/load")))
          (apply orig-fun args)
        (let* ((orig-success (plist-get args :on-success))
               (orig-failure (plist-get args :on-failure))
               (new-args (copy-sequence args)))
          (setq new-args
                (plist-put
                 new-args :on-success
                 (lambda (acp-response)
                   (unwind-protect
                       (when orig-success
                         (funcall orig-success acp-response))
                     (my/agent-shell--render-deferred-markdown-fragments state)))))
          (setq new-args
                (plist-put
                 new-args :on-failure
                 (lambda (acp-error raw-message)
                   (unwind-protect
                       (when orig-failure
                         (funcall orig-failure acp-error raw-message))
                     ;; Also render partial output after interrupt/failure.
                     (my/agent-shell--render-deferred-markdown-fragments state)))))
          (apply orig-fun new-args)))))

  (advice-remove 'agent-shell--render-markdown
                 #'my/agent-shell--maybe-suppress-render-markdown)
  (advice-add 'agent-shell--render-markdown
              :around #'my/agent-shell--maybe-suppress-render-markdown)

  (advice-remove 'agent-shell--update-fragment
                 #'my/agent-shell--update-fragment-defer-markdown)
  (advice-add 'agent-shell--update-fragment
              :around #'my/agent-shell--update-fragment-defer-markdown)

  (advice-remove 'agent-shell--send-request
                 #'my/agent-shell--send-request-render-deferred-markdown)
  (advice-add 'agent-shell--send-request
              :around #'my/agent-shell--send-request-render-deferred-markdown)

  (advice-remove 'agent-shell--set-session-config-option
                 #'my/agent-shell--apply-thought-level-after-model-change)
  (advice-add 'agent-shell--set-session-config-option
              :around #'my/agent-shell--apply-thought-level-after-model-change))

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
