;;; agent-shell-discord.el --- Bridge agent-shell sessions to Discord -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A Discord bridge for `agent-shell', modelled on ElleNajt/agent-shell-to-go
;; (which does the same over Slack).  It runs inside the local Emacs daemon and:
;;
;;   * mirrors each project to a Discord text channel and each agent-shell
;;     session to a thread inside that channel (session sync, like `agent-hub');
;;   * relays a Discord message posted in a session thread (by an authorized
;;     user) into that session as an agent prompt, and posts the agent's reply
;;     back into the thread (bidirectional chat).
;;
;; Transport: the Discord Gateway (WebSocket, via `websocket') for inbound
;; events, and the Discord REST API (via a LOCAL `curl') for posting.  The bot
;; token is read from `auth-source' (rendered from SOPS by Home Manager); the
;; guild id and the authorized-user allowlist are set from `local.el'.
;;
;; TRAMP safety: the bridge operates on local agent-shell BUFFERS, never on the
;; remote agent process, so remote (TRAMP) sessions work like local ones.  No
;; gateway/heartbeat/message handler performs synchronous remote I/O; REST calls
;; force a local `default-directory' so `curl' always runs locally.
;;
;; Security: any message accepted from a thread drives an agent that this machine
;; runs with bypassed permissions, so `agent-shell-discord-authorized-users' is
;; the primary trust boundary and is enforced on every inbound message.
;;
;; Permission approvals (approve/deny tool calls from Discord) are intentionally
;; out of scope for this first version.
;;
;; NOTE: coupled to agent-shell's current API, verified against the loaded
;; package (agent-shell 20260709.611):
;;   (agent-shell-buffers)                                    ; all shell buffers
;;   (agent-shell-status :shell-buffer BUF)                   ; busy/blocked/ready
;;   (agent-shell-submit :input STR :on-output FN :on-finished FN)
;;   (agent-shell-subscribe-to :shell-buffer BUF :event SYM :on-event FN)
;;   (agent-shell-unsubscribe :subscription TOKEN)
;; If agent-shell changes, re-derive via
;;   emacsclient -e '(find-library-name "agent-shell")'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'auth-source)
(require 'agent-hub nil t)

(declare-function websocket-open "websocket")
(declare-function websocket-send-text "websocket")
(declare-function websocket-close "websocket")
(declare-function websocket-frame-text "websocket")
(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-submit "agent-shell")
(declare-function agent-shell-subscribe-to "agent-shell")
(declare-function agent-shell-unsubscribe "agent-shell")
(declare-function agent-hub--buffer-session-id "agent-hub")
(declare-function agent-hub--buffer-status "agent-hub")
(declare-function agent-hub--buffer-cwd "agent-hub")
(declare-function agent-hub--buffer-agent-id "agent-hub")
(declare-function agent-hub--agent-buffers "agent-hub")
(declare-function agent-hub--slug "agent-hub")
(declare-function agent-hub--workspaces "agent-hub")
(declare-function agent-hub--workspace-for-cwd "agent-hub")

;; agent-shell writes every assistant/user message, thought, and tool-call
;; result into this per-buffer Markdown transcript.  It is our only source of
;; assistant message text (no agent-shell event carries it), so on turn-complete
;; we tail the file for the delta.  The read is guarded to local transcripts, so
;; a remote (TRAMP) session never triggers synchronous remote I/O.
(defvar agent-shell--transcript-file)

(defgroup agent-shell-discord nil
  "Bridge agent-shell sessions to Discord."
  :group 'tools)

;;;; Configuration

(defcustom agent-shell-discord-guild-id nil
  "Discord guild (server) id the bridge manages channels in.
A string of digits.  When nil, `agent-shell-discord-start' is a no-op."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-authorized-users nil
  "List of Discord user-id strings allowed to drive agents.
This is the security allowlist: a message from any other author is ignored.
A single entry of \"*\" allows EVERY user.  Warning: this machine runs agents
with bypassed permissions, so \"*\" means anyone who can post in a session
thread can execute arbitrary actions here -- only use it on a private, fully
trusted server.  Empty means nobody is authorized (inbound chat disabled)."
  :type '(repeat string)
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-category-id nil
  "Optional Discord category (parent) id for auto-created project channels.
A string of digits, or nil to create channels at the guild root."
  :type '(choice (const :tag "Guild root" nil) string)
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-authinfo-file
  (expand-file-name "~/.config/agent-shell-discord/authinfo")
  "Authinfo-style file holding the Discord bot token.
Expected line: `machine discord.com login bot password <TOKEN>'.
Rendered from SOPS by Home Manager."
  :type 'file
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-channel-map-file
  (locate-user-emacs-file "agent-shell-discord-channels.eld")
  "File persisting the project-root -> Discord channel-id mapping."
  :type 'file
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-thread-map-file
  (locate-user-emacs-file "agent-shell-discord-threads.eld")
  "File persisting the session-id -> Discord thread-id mapping."
  :type 'file
  :group 'agent-shell-discord)

(defcustom agent-shell-discord-auto-start nil
  "When non-nil, start the bridge automatically after Emacs startup.
Set from `local.el' on the profile that enables the bridge."
  :type 'boolean
  :group 'agent-shell-discord)

(defconst agent-shell-discord--api-base "https://discord.com/api/v10"
  "Base URL for the Discord REST API.")

(defconst agent-shell-discord--gateway-url
  "wss://gateway.discord.gg/?v=10&encoding=json"
  "Discord Gateway WebSocket URL.")

(defconst agent-shell-discord--intents 34304
  "Gateway intents bitmask.
GUILD_MESSAGES (512) + GUILD_MESSAGE_REACTIONS (1024) + MESSAGE_CONTENT (32768).
MESSAGE_CONTENT is privileged and must be enabled in the Developer Portal.")

(defconst agent-shell-discord--message-limit 1900
  "Max characters per posted Discord message chunk (Discord hard limit is 2000).")

;;;; Internal state

(defvar agent-shell-discord--ws nil
  "The active `websocket' connection, or nil.")

(defvar agent-shell-discord--heartbeat-timer nil
  "Repeating heartbeat timer for the gateway, or nil.")

(defvar agent-shell-discord--reconnect-timer nil
  "Pending reconnect timer, or nil.")

(defvar agent-shell-discord--seq nil
  "Last received dispatch sequence number, for heartbeats and RESUME.")

(defvar agent-shell-discord--gateway-session-id nil
  "Gateway session id from READY, for RESUME.")

(defvar agent-shell-discord--heartbeat-acked t
  "Whether the last heartbeat was acknowledged (zombie-connection guard).")

(defvar agent-shell-discord--bot-user-id nil
  "The bot's own Discord user id (from READY), used to ignore self-messages.")

(defvar agent-shell-discord--reconnect-delay 1
  "Current reconnect backoff in seconds; doubles up to 60, resets on READY.")

(defvar agent-shell-discord--stopping nil
  "Non-nil while intentionally shutting down, to suppress reconnects.")

(defvar agent-shell-discord--token nil
  "Cached bot token string, or nil until read from auth-source.")

(defvar agent-shell-discord--channel-map nil
  "Hash table: project-root string -> Discord channel-id string.")

(defvar agent-shell-discord--thread-map nil
  "Hash table: agent-shell session-id string -> Discord thread-id string.")

(defvar agent-shell-discord--buffers (make-hash-table :test #'eq)
  "Hash table: agent-shell buffer -> plist (:subscription TOKEN :session-id ID).")

(defvar agent-shell-discord--pending-threads (make-hash-table :test #'equal)
  "Hash table: session-id -> list of pending message strings awaiting a thread.")

(defvar agent-shell-discord--post-queues (make-hash-table :test #'equal)
  "Hash table: channel-id -> plist (:busy BOOL :queue ITEMS).
Serializes REST posts per Discord channel/thread so transcript blocks keep file
order even when several posts are emitted in one Emacs event.")

;;;; Logging

(defun agent-shell-discord--log (format-string &rest args)
  "Append a timestamped line to the `*agent-shell-discord*' log buffer."
  (let ((buffer (get-buffer-create "*agent-shell-discord*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (format-time-string "[%H:%M:%S] ")
                (apply #'format format-string args)
                "\n")))))

;;;; Token / persistence

(defun agent-shell-discord--read-token ()
  "Return the bot token from auth-source, or nil.
Cached in `agent-shell-discord--token' after the first successful read."
  (or agent-shell-discord--token
      (let* ((auth-sources (list agent-shell-discord-authinfo-file))
             (found (car (auth-source-search :host "discord.com"
                                             :user "bot" :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (setq agent-shell-discord--token
                  (if (functionp secret) (funcall secret) secret)))))))

(defun agent-shell-discord--load-map (file)
  "Load a hash table persisted to FILE, or return a fresh one."
  (or (ignore-errors
        (when (file-readable-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((table (make-hash-table :test #'equal)))
              (dolist (cell (read (current-buffer)) table)
                (when (and (consp cell) (stringp (car cell)) (stringp (cdr cell)))
                  (puthash (car cell) (cdr cell) table)))))))
      (make-hash-table :test #'equal)))

(defun agent-shell-discord--save-map (table file)
  "Persist hash TABLE to FILE as an alist of string pairs."
  (ignore-errors
    (let (alist)
      (maphash (lambda (k v) (push (cons k v) alist)) table)
      (with-temp-file file
        (let ((print-length nil) (print-level nil))
          (prin1 alist (current-buffer)))))))

(defun agent-shell-discord--save-maps ()
  "Persist both mapping tables to disk."
  (when agent-shell-discord--channel-map
    (agent-shell-discord--save-map agent-shell-discord--channel-map
                                   agent-shell-discord-channel-map-file))
  (when agent-shell-discord--thread-map
    (agent-shell-discord--save-map agent-shell-discord--thread-map
                                   agent-shell-discord-thread-map-file)))

;;;; REST client (always local curl)

(defun agent-shell-discord--api (method path payload callback &optional retries)
  "Call the Discord REST API asynchronously and pass the result to CALLBACK.
METHOD is an HTTP method string; PATH is appended to the API base; PAYLOAD is an
alist serialized to a JSON body (nil for none).  CALLBACK is called with
\(OK PARSED-JSON-OR-NIL): OK is non-nil on a 2xx response.  The auth header and
JSON body are passed via a temporary `curl' config file (mode 0600) so the token
never appears in the process argument list, and the call always runs with a
local `default-directory' so `curl' is never dispatched through TRAMP.  RETRIES
guards a single 429 retry."
  (let ((token (agent-shell-discord--read-token)))
    (if (not token)
        (progn
          (agent-shell-discord--log "REST %s %s skipped: no token" method path)
          (funcall callback nil nil))
      (let* ((retries (or retries 0))
             (url (concat agent-shell-discord--api-base path))
             (body-file (and payload (make-temp-file "asd-body-" nil ".json")))
             (config-file (make-temp-file "asd-curl-" nil ".conf"))
             (default-directory temporary-file-directory)
             (process-connection-type nil)
             (out-buffer (generate-new-buffer " *asd-curl*")))
        ;; Bind the write coding system explicitly: payloads carry non-ASCII
        ;; (emoji, CJK session titles), and without this `write-region' would
        ;; call `select-safe-coding-system' interactively -- a blocking prompt
        ;; that hangs the headless daemon.
        (let ((coding-system-for-write 'utf-8-unix))
          (when body-file
            (with-temp-file body-file
              (insert (json-serialize payload)))
            (set-file-modes body-file #o600))
          (with-temp-file config-file
            (insert (format "url = \"%s\"\n" url))
            (insert (format "request = \"%s\"\n" method))
            (insert (format "header = \"Authorization: Bot %s\"\n" token))
            (insert "header = \"Content-Type: application/json\"\n")
            (insert "header = \"User-Agent: agent-shell-discord (emacs, 0.1)\"\n")
            (when body-file
              (insert (format "data = \"@%s\"\n" body-file))))
          (set-file-modes config-file #o600))
        (let ((process (start-process
                        "asd-curl" out-buffer "curl" "-sS"
                        "-K" config-file "-w" "\n%{http_code}")))
          (set-process-query-on-exit-flag process nil)
          (set-process-sentinel
           process
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((raw (with-current-buffer (process-buffer proc)
                             (buffer-string)))
                      (nl (and raw (string-match "\n\\([0-9]+\\)\\s-*\\'" raw)))
                      (status (and nl (string-to-number (match-string 1 raw))))
                      (body (if nl (substring raw 0 nl) raw))
                      (parsed (ignore-errors
                                (json-parse-string
                                 (string-trim (or body ""))
                                 :object-type 'alist :array-type 'list
                                 :null-object nil :false-object nil))))
                 (ignore-errors (kill-buffer (process-buffer proc)))
                 (ignore-errors (delete-file config-file))
                 (when body-file (ignore-errors (delete-file body-file)))
                 (cond
                  ((and status (= status 429) (< retries 2))
                   (let ((wait (or (and (listp parsed)
                                        (alist-get 'retry_after parsed))
                                   1)))
                     (agent-shell-discord--log
                      "REST %s %s -> 429, retrying in %ss" method path wait)
                     (run-at-time (+ 0.1 (float wait)) nil
                                  #'agent-shell-discord--api
                                  method path payload callback (1+ retries))))
                  ((and status (>= status 200) (< status 300))
                   (funcall callback t parsed))
                  (t
                   (agent-shell-discord--log "REST %s %s -> %s %s"
                                             method path (or status "?")
                                             (or body ""))
                   (funcall callback nil parsed))))))))))))

(defun agent-shell-discord--post-message (channel-id text callback)
  "Post TEXT to CHANNEL-ID, chunked to Discord's length limit.
CALLBACK receives (OK JSON) for the FIRST posted chunk (enough to spawn a
thread from a starter message)."
  (let ((chunks (agent-shell-discord--split-message text))
        (first t))
    (agent-shell-discord--enqueue-message
     channel-id
     (mapcar (lambda (chunk)
               (prog1 (list :content chunk :callback (and first callback))
                 (setq first nil)))
             chunks))))

(defun agent-shell-discord--enqueue-message (channel-id items)
  "Queue ITEMS for CHANNEL-ID and start posting if idle."
  (let* ((entry (or (gethash channel-id agent-shell-discord--post-queues) '()))
         (queue (append (plist-get entry :queue) items)))
    (puthash channel-id (plist-put entry :queue queue)
             agent-shell-discord--post-queues)
    (agent-shell-discord--drain-message-queue channel-id)))

(defun agent-shell-discord--drain-message-queue (channel-id)
  "Post the next queued Discord message for CHANNEL-ID, preserving order."
  (let* ((entry (gethash channel-id agent-shell-discord--post-queues))
         (busy (plist-get entry :busy))
         (queue (plist-get entry :queue)))
    (when (and queue (not busy))
      (let ((item (car queue)))
        (setq entry (plist-put entry :busy t))
        (setq entry (plist-put entry :queue (cdr queue)))
        (puthash channel-id entry agent-shell-discord--post-queues)
        (agent-shell-discord--api
         "POST" (format "/channels/%s/messages" channel-id)
         `((content . ,(plist-get item :content)))
         (lambda (ok json)
           (when-let ((callback (plist-get item :callback)))
             (funcall callback ok json))
           (let ((entry (gethash channel-id agent-shell-discord--post-queues)))
             (setq entry (plist-put entry :busy nil))
             (puthash channel-id entry agent-shell-discord--post-queues))
           (agent-shell-discord--drain-message-queue channel-id)))))))

(defun agent-shell-discord--split-message (text)
  "Split TEXT into a list of chunks no longer than the message limit."
  (let ((text (or text ""))
        (limit agent-shell-discord--message-limit)
        chunks)
    (if (<= (length text) limit)
        (list (if (string-empty-p text) "​" text))
      (while (> (length text) limit)
        ;; Prefer to break on a newline within the window.
        (let* ((window (substring text 0 limit))
               (nl (or (cl-position ?\n window :from-end t) limit)))
          (when (< nl (/ limit 2)) (setq nl limit))
          (push (substring text 0 nl) chunks)
          (setq text (string-trim-left (substring text nl)))))
      (when (> (length text) 0) (push text chunks))
      (nreverse chunks))))

;;;; Project / channel / thread mapping

(defun agent-shell-discord--project-root (buffer)
  "Return the workspace root for agent-shell BUFFER (may be a TRAMP path)."
  (let ((cwd (ignore-errors (agent-hub--buffer-cwd buffer))))
    (or (and cwd (fboundp 'agent-hub--workspaces)
             (ignore-errors
               (agent-hub--workspace-for-cwd cwd (agent-hub--workspaces))))
        cwd
        (directory-file-name (expand-file-name default-directory)))))

(defun agent-shell-discord--channel-name (root)
  "Return a Discord-safe channel name for workspace ROOT.
Derived from ROOT's localname basename (never the raw TRAMP path); a remote
root is prefixed with its host to disambiguate same-named projects."
  (let* ((localname (or (file-remote-p root 'localname) root))
         (base (file-name-nondirectory (directory-file-name localname)))
         (slug (if (fboundp 'agent-hub--slug)
                   (agent-hub--slug base)
                 (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" base))))
         (host (when (file-remote-p root)
                 (agent-hub--slug (or (file-remote-p root 'host) "remote")))))
    (setq slug (if (string-empty-p slug) "session" slug))
    (let ((name (if host (concat host "-" slug) slug)))
      (substring name 0 (min (length name) 90)))))

(defun agent-shell-discord--ensure-channel (root callback)
  "Ensure a Discord channel exists for workspace ROOT; pass its id to CALLBACK.
Uses the persisted channel map; creates a text channel when absent."
  (let ((existing (gethash root agent-shell-discord--channel-map)))
    (if existing
        (funcall callback existing)
      (let ((payload `((name . ,(agent-shell-discord--channel-name root))
                       (type . 0))))
        (when agent-shell-discord-category-id
          (setq payload (append payload
                                `((parent_id . ,agent-shell-discord-category-id)))))
        (agent-shell-discord--api
         "POST" (format "/guilds/%s/channels" agent-shell-discord-guild-id)
         payload
         (lambda (ok json)
           (if (and ok (alist-get 'id json))
               (let ((id (alist-get 'id json)))
                 (puthash root id agent-shell-discord--channel-map)
                 (agent-shell-discord--save-maps)
                 (agent-shell-discord--log "Created channel %s for %s" id root)
                 (funcall callback id))
             (agent-shell-discord--log "Failed to create channel for %s" root)
             (funcall callback nil))))))))

(defun agent-shell-discord--ensure-thread (buffer session-id callback)
  "Ensure a Discord thread exists for SESSION-ID in BUFFER's channel.
Passes the thread-id to CALLBACK.  Creates the channel and a starter message,
then a thread off that message, when the session has no thread yet."
  (let ((existing (gethash session-id agent-shell-discord--thread-map)))
    (if existing
        (funcall callback existing)
      (let* ((root (agent-shell-discord--project-root buffer))
             (title (agent-shell-discord--session-title buffer))
             (agent (or (ignore-errors (agent-hub--buffer-agent-id buffer)) "agent"))
             (starter (format "\U0001F7E2 Session started: %s  [%s]" title agent)))
        (agent-shell-discord--ensure-channel
         root
         (lambda (channel-id)
           (if (not channel-id)
               (funcall callback nil)
             (agent-shell-discord--api
              "POST" (format "/channels/%s/messages" channel-id)
              `((content . ,starter))
              (lambda (ok json)
                (if (not (and ok (alist-get 'id json)))
                    (funcall callback nil)
                  (let ((message-id (alist-get 'id json)))
                    (agent-shell-discord--api
                     "POST" (format "/channels/%s/messages/%s/threads"
                                    channel-id message-id)
                     `((name . ,(substring title 0 (min (length title) 90)))
                       (auto_archive_duration . 1440))
                     (lambda (ok2 json2)
                       (if (and ok2 (alist-get 'id json2))
                           (let ((thread-id (alist-get 'id json2)))
                             (puthash session-id thread-id
                                      agent-shell-discord--thread-map)
                             (agent-shell-discord--save-maps)
                             (agent-shell-discord--log
                              "Created thread %s for session %s"
                              thread-id session-id)
                             (agent-shell-discord--flush-pending session-id thread-id)
                             (funcall callback thread-id))
                         (funcall callback nil)))))))))))))))

(defun agent-shell-discord--flush-pending (session-id thread-id)
  "Post any messages queued for SESSION-ID before its THREAD-ID existed."
  (let ((pending (nreverse (gethash session-id agent-shell-discord--pending-threads))))
    (remhash session-id agent-shell-discord--pending-threads)
    (dolist (text pending)
      (agent-shell-discord--post-message thread-id text #'ignore))))

(defun agent-shell-discord--session-title (buffer)
  "Return a short display title for agent-shell BUFFER."
  (or (ignore-errors
        (let ((name (buffer-name buffer)))
          (and (stringp name) (string-trim name))))
      "session"))

(defun agent-shell-discord--thread-for-session (session-id)
  "Return the thread-id mapped to SESSION-ID, or nil."
  (and session-id (gethash session-id agent-shell-discord--thread-map)))

(defun agent-shell-discord--session-for-thread (thread-id)
  "Return the session-id whose thread is THREAD-ID, or nil."
  (let (result)
    (when agent-shell-discord--thread-map
      (maphash (lambda (session tid)
                 (when (equal tid thread-id) (setq result session)))
               agent-shell-discord--thread-map))
    result))

(defun agent-shell-discord--buffer-for-session (session-id)
  "Return the live agent-shell BUFFER for SESSION-ID, or nil."
  (seq-find (lambda (buffer)
              (and (buffer-live-p buffer)
                   (equal session-id
                          (ignore-errors (agent-hub--buffer-session-id buffer)))))
            (agent-hub--agent-buffers)))

;;;; Outbound: post session activity to Discord

(defun agent-shell-discord--post-to-session (session-id text)
  "Post TEXT to SESSION-ID's Discord thread, queueing until it exists."
  (when session-id
    (let ((thread-id (agent-shell-discord--thread-for-session session-id)))
      (if thread-id
          (agent-shell-discord--post-message thread-id text #'ignore)
        (push text (gethash session-id agent-shell-discord--pending-threads))))))

;;;; Transcript relay (assistant/user text only, no tool calls)
;;
;; agent-shell has no event carrying assistant message text; it only writes text
;; to a per-buffer Markdown transcript, tagging sections with `## Agent (..)',
;; `## User (..)', `## Agent's Thoughts (..)', and `### Tool Call [..]:' headers.
;; On `turn-complete' we tail the transcript file for the delta appended since
;; the last relay and post only the Agent + User prose, dropping thoughts and
;; tool-call blocks (issues: agent output must show, tool output must not).
;;
;; We read the file rather than advise `agent-shell--append-transcript' because
;; agent-shell may redefine that internal function, silently dropping any advice.
;; The read is guarded to LOCAL transcripts only, so a remote (TRAMP) session
;; never triggers synchronous remote I/O in the event handler.

(defun agent-shell-discord--entry-put (buffer key value)
  "Set KEY to VALUE in BUFFER's registry plist, creating the entry if needed."
  (puthash buffer
           (plist-put (or (gethash buffer agent-shell-discord--buffers) '())
                      key value)
           agent-shell-discord--buffers))

(defun agent-shell-discord--transcript-file (buffer)
  "Return BUFFER's LOCAL transcript file path if readable, else nil."
  (when-let* (((buffer-live-p buffer))
              (file (buffer-local-value 'agent-shell--transcript-file buffer))
              ((not (file-remote-p file)))
              ((file-readable-p file)))
    file))

(defun agent-shell-discord--transcript-size (file)
  "Return the character length of transcript FILE (decoded as UTF-8), or 0."
  (or (ignore-errors
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8))
            (insert-file-contents file))
          (buffer-size)))
      0))

(defun agent-shell-discord--init-transcript-pos (buffer)
  "Seed BUFFER's relay position to the current transcript end.
So only content appended after the bridge attaches is relayed (not the whole
back-history of a resumed session)."
  (when-let ((file (agent-shell-discord--transcript-file buffer)))
    (unless (plist-get (gethash buffer agent-shell-discord--buffers) :xpos)
      (agent-shell-discord--entry-put
       buffer :xpos (agent-shell-discord--transcript-size file)))))

(defun agent-shell-discord--parse-transcript-region (text)
  "Parse transcript REGION TEXT into an ordered list of (TYPE . PROSE).
TYPE is `agent' or `user'; `## Agent's Thoughts' and `### Tool Call' sections
are dropped.  Only the four headers agent-shell itself writes switch sections,
so a Markdown heading inside assistant prose is treated as content, not a
boundary."
  (let ((section nil) (buf "") (blocks nil))
    (cl-flet ((flush ()
                (when (and (memq section '(agent user))
                           (> (length (string-trim buf)) 0))
                  (push (cons section (string-trim buf)) blocks))
                (setq buf "")))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-match-p "\\`[ \t]*## Agent's Thoughts (" line)
          (flush) (setq section 'thoughts))
         ((string-match-p "\\`[ \t]*## Agent (" line)
          (flush) (setq section 'agent))
         ((string-match-p "\\`[ \t]*## User (" line)
          (flush) (setq section 'user))
         ((string-match-p "\\`[ \t]*### Tool Call \\[" line)
          (flush) (setq section 'tool))
         (t
          (when (memq section '(agent user))
            (setq buf (concat buf
                              (if (eq section 'user)
                                  (replace-regexp-in-string "\\`> ?" "" line)
                                line)
                              "\n"))))))
      (flush)
      (nreverse blocks))))

(defun agent-shell-discord--relay-transcript (buffer)
  "Post the transcript delta for BUFFER (since last relay) to its Discord thread.
Relays assistant and user prose; a user prompt that echoes one submitted from
Discord is suppressed (already visible in the thread)."
  (let* ((entry (gethash buffer agent-shell-discord--buffers))
         (session-id (plist-get entry :session-id))
         (file (agent-shell-discord--transcript-file buffer)))
    (when (and session-id file)
      (let ((pos (or (plist-get entry :xpos) 0))
            region size)
        (ignore-errors
          (with-temp-buffer
            (let ((coding-system-for-read 'utf-8))
              (insert-file-contents file))
            (setq size (buffer-size))
            (when (> size pos)
              (setq region (buffer-substring-no-properties
                            (1+ pos) (point-max))))))
        (when size
          (agent-shell-discord--entry-put buffer :xpos size))
        (when region
          (let ((discord-input (plist-get entry :discord-input)))
            (dolist (block (agent-shell-discord--parse-transcript-region region))
              (pcase (car block)
                ('user
                 (if (and discord-input
                          (string= (string-trim (cdr block))
                                   (string-trim discord-input)))
                     (agent-shell-discord--entry-put buffer :discord-input nil)
                   (agent-shell-discord--post-to-session
                    session-id (concat "\U0001F9D1 " (cdr block)))))
                ('agent
                 (agent-shell-discord--post-to-session
                  session-id (cdr block)))))))))))

(defun agent-shell-discord--handle-event (buffer event)
  "Handle an agent-shell EVENT from BUFFER; mirror it into Discord."
  (let* ((type (map-elt event :event))
         (data (map-elt event :data))
         (session-id (or (plist-get (gethash buffer agent-shell-discord--buffers)
                                    :session-id)
                         (ignore-errors (agent-hub--buffer-session-id buffer)))))
    ;; Once a session id materializes, make sure a thread exists.
    (when (and session-id
               (not (plist-get (gethash buffer agent-shell-discord--buffers)
                               :session-id)))
      (agent-shell-discord--attach-session buffer session-id))
    (agent-shell-discord--init-transcript-pos buffer)
    (pcase type
      ('turn-complete
       ;; Post the turn's user prompt + assistant text from the transcript
       ;; delta, dropping tool-call and thought sections.
       (agent-shell-discord--relay-transcript buffer))
      ('permission-request
       (agent-shell-discord--post-to-session
        session-id
        "⚠️ agent is waiting for a permission response (approve in Emacs)"))
      ('error
       (agent-shell-discord--post-to-session
        session-id
        (format "❌ error: %s" (or (map-elt data :message) "unknown"))))
      ('session-title-changed
       ;; Name the thread once, from the first meaningful title (derived from the
       ;; opening prompt); ignore later title churn so the thread name is stable.
       (let ((entry (gethash buffer agent-shell-discord--buffers)))
         (when (and session-id (not (plist-get entry :thread-named)))
           (when-let ((thread-id (agent-shell-discord--thread-for-session session-id))
                      (title (map-elt data :title)))
             (agent-shell-discord--entry-put buffer :thread-named t)
             (agent-shell-discord--api
              "PATCH" (format "/channels/%s" thread-id)
              `((name . ,(substring title 0 (min (length title) 90))))
              #'ignore)))))
      ('clean-up
       (agent-shell-discord--detach-buffer buffer)))))

(defun agent-shell-discord--attach-session (buffer session-id)
  "Record SESSION-ID for BUFFER and ensure its Discord thread exists."
  (agent-shell-discord--entry-put buffer :session-id session-id)
  (agent-shell-discord--init-transcript-pos buffer)
  (agent-shell-discord--ensure-thread buffer session-id #'ignore))

(defun agent-shell-discord--register-buffer (&optional buffer)
  "Subscribe the bridge to agent-shell BUFFER (default current buffer)."
  (let ((buffer (or buffer (current-buffer))))
    (when (and (buffer-live-p buffer)
               (fboundp 'agent-shell-subscribe-to)
               (not (gethash buffer agent-shell-discord--buffers)))
      (let ((token (ignore-errors
                     (agent-shell-subscribe-to
                      :shell-buffer buffer
                      :on-event (lambda (event)
                                  (ignore-errors
                                    (agent-shell-discord--handle-event
                                     buffer event)))))))
        (puthash buffer (list :subscription token) agent-shell-discord--buffers)
        (agent-shell-discord--init-transcript-pos buffer)
        (agent-shell-discord--log "Registered buffer %s" (buffer-name buffer))
        ;; Session id may already exist for a resumed session.
        (when-let ((session-id (ignore-errors
                                 (agent-hub--buffer-session-id buffer))))
          (agent-shell-discord--attach-session buffer session-id))))))

(defun agent-shell-discord--detach-buffer (buffer)
  "Unsubscribe and forget BUFFER."
  (when-let ((entry (gethash buffer agent-shell-discord--buffers)))
    (when-let ((token (plist-get entry :subscription)))
      (ignore-errors (agent-shell-unsubscribe :subscription token)))
    (remhash buffer agent-shell-discord--buffers)))

(defun agent-shell-discord-sync ()
  "Register every live agent-shell buffer with the bridge."
  (interactive)
  (when (fboundp 'agent-hub--agent-buffers)
    (dolist (buffer (agent-hub--agent-buffers))
      (agent-shell-discord--register-buffer buffer))))

;;;; Inbound: Discord message -> agent prompt

(defun agent-shell-discord--authorized-p (author-id)
  "Return non-nil when AUTHOR-ID may drive agents.
True when the allowlist contains AUTHOR-ID, or the wildcard entry \"*\"."
  (and author-id
       (or (member "*" agent-shell-discord-authorized-users)
           (member author-id agent-shell-discord-authorized-users))))

(defun agent-shell-discord--handle-message-create (data)
  "Handle a MESSAGE_CREATE gateway dispatch DATA alist."
  (let* ((author-id (map-nested-elt data '(author id)))
         (is-bot (map-nested-elt data '(author bot)))
         (channel-id (alist-get 'channel_id data))
         (content (alist-get 'content data)))
    (cond
     (is-bot nil)
     ((equal author-id agent-shell-discord--bot-user-id) nil)
     ((not (agent-shell-discord--authorized-p author-id))
      (agent-shell-discord--log "Ignored message from unauthorized user %s"
                                author-id))
     ((or (null content) (string-empty-p (string-trim content))) nil)
     (t
      (let* ((session-id (agent-shell-discord--session-for-thread channel-id))
             (buffer (and session-id
                          (agent-shell-discord--buffer-for-session session-id))))
        (if (not (buffer-live-p buffer))
            (agent-shell-discord--log
             "No live session for thread %s" channel-id)
          (agent-shell-discord--submit-prompt buffer session-id
                                              (string-trim content))))))))

(defun agent-shell-discord--submit-prompt (buffer session-id text)
  "Submit TEXT to agent-shell BUFFER; the agent's reply is relayed via transcript.
Records TEXT so the transcript echo of this prompt is suppressed (the user's own
Discord message already appears in the thread).  Only failures are posted here."
  (agent-shell-discord--log "Prompt -> %s: %s" (buffer-name buffer) text)
  (agent-shell-discord--entry-put buffer :discord-input (string-trim text))
  (with-current-buffer buffer
    (ignore-errors
      (agent-shell-submit
       :input text
       :on-finished
       (lambda (_input output success)
         (unless success
           (agent-shell-discord--post-to-session
            session-id (format "❌ failed: %s" (or output "")))))))))

;;;; Gateway (WebSocket)

(defun agent-shell-discord--send (payload)
  "Serialize PAYLOAD to JSON and send it over the gateway."
  (when agent-shell-discord--ws
    (ignore-errors
      (websocket-send-text agent-shell-discord--ws (json-serialize payload)))))

(defun agent-shell-discord--send-heartbeat ()
  "Send a gateway heartbeat, reconnecting if the previous one was not ACKed."
  (if (not agent-shell-discord--heartbeat-acked)
      (progn
        (agent-shell-discord--log "Heartbeat not ACKed; reconnecting")
        (agent-shell-discord--reconnect))
    (setq agent-shell-discord--heartbeat-acked nil)
    (agent-shell-discord--send
     `((op . 1) (d . ,(or agent-shell-discord--seq :null))))))

(defun agent-shell-discord--start-heartbeat (interval-ms)
  "Start the heartbeat timer firing every INTERVAL-MS milliseconds."
  (when agent-shell-discord--heartbeat-timer
    (cancel-timer agent-shell-discord--heartbeat-timer))
  (setq agent-shell-discord--heartbeat-acked t)
  (let ((interval (/ interval-ms 1000.0)))
    (setq agent-shell-discord--heartbeat-timer
          (run-at-time interval interval #'agent-shell-discord--send-heartbeat))))

(defun agent-shell-discord--identify ()
  "Send the gateway Identify payload."
  (agent-shell-discord--send
   `((op . 2)
     (d . ((token . ,(agent-shell-discord--read-token))
           (intents . ,agent-shell-discord--intents)
           (properties . ((os . "darwin")
                          (browser . "emacs")
                          (device . "emacs"))))))))

(defun agent-shell-discord--on-message (_ws frame)
  "Handle an incoming gateway FRAME."
  (let* ((text (ignore-errors (websocket-frame-text frame)))
         (payload (and text (ignore-errors
                              (json-parse-string
                               text :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil))))
         (op (and payload (alist-get 'op payload)))
         (seq (and payload (alist-get 's payload)))
         (data (and payload (alist-get 'd payload))))
    (when seq (setq agent-shell-discord--seq seq))
    (pcase op
      (10 ;; Hello
       (agent-shell-discord--start-heartbeat
        (or (alist-get 'heartbeat_interval data) 41250))
       (agent-shell-discord--identify))
      (11 ;; Heartbeat ACK
       (setq agent-shell-discord--heartbeat-acked t))
      (1 ;; Heartbeat request
       (agent-shell-discord--send-heartbeat))
      ((or 7 9) ;; Reconnect / Invalid Session
       (agent-shell-discord--log "Gateway op %s; reconnecting" op)
       (agent-shell-discord--reconnect))
      (0 ;; Dispatch
       (agent-shell-discord--handle-dispatch (alist-get 't payload) data)))))

(defun agent-shell-discord--handle-dispatch (type data)
  "Handle a gateway dispatch of TYPE with DATA."
  (pcase type
    ("READY"
     (setq agent-shell-discord--gateway-session-id (alist-get 'session_id data)
           agent-shell-discord--bot-user-id (map-nested-elt data '(user id))
           agent-shell-discord--reconnect-delay 1)
     (agent-shell-discord--log "Gateway READY as user %s"
                               agent-shell-discord--bot-user-id))
    ("MESSAGE_CREATE"
     (ignore-errors (agent-shell-discord--handle-message-create data)))))

(defun agent-shell-discord--on-close (&rest _)
  "Handle gateway socket close."
  (agent-shell-discord--log "Gateway closed")
  (unless agent-shell-discord--stopping
    (agent-shell-discord--reconnect)))

(defun agent-shell-discord--reconnect ()
  "Schedule a gateway reconnect with exponential backoff."
  (when agent-shell-discord--heartbeat-timer
    (cancel-timer agent-shell-discord--heartbeat-timer)
    (setq agent-shell-discord--heartbeat-timer nil))
  (when agent-shell-discord--ws
    (ignore-errors (websocket-close agent-shell-discord--ws))
    (setq agent-shell-discord--ws nil))
  (unless (or agent-shell-discord--stopping agent-shell-discord--reconnect-timer)
    (let ((delay agent-shell-discord--reconnect-delay))
      (setq agent-shell-discord--reconnect-delay (min 60 (* 2 delay)))
      (agent-shell-discord--log "Reconnecting in %ss" delay)
      (setq agent-shell-discord--reconnect-timer
            (run-at-time delay nil #'agent-shell-discord--connect)))))

(defun agent-shell-discord--connect ()
  "Open the gateway WebSocket connection."
  (setq agent-shell-discord--reconnect-timer nil)
  (if (not (require 'websocket nil t))
      (agent-shell-discord--log "websocket package unavailable; cannot connect")
    (condition-case err
        (setq agent-shell-discord--ws
              (websocket-open
               agent-shell-discord--gateway-url
               :on-message #'agent-shell-discord--on-message
               :on-close #'agent-shell-discord--on-close
               :on-error (lambda (_ws type err)
                           (agent-shell-discord--log
                            "Gateway error (%s): %S" type err))))
      (error
       (agent-shell-discord--log "Connect failed: %S" err)
       (agent-shell-discord--reconnect)))))

;;;; Entry points

;;;###autoload
(defun agent-shell-discord-start ()
  "Start the Discord bridge: connect the gateway and mirror agent-shell sessions.
A no-op (with a log line) when the guild id or token is missing, so a profile
without Discord configured is unaffected."
  (interactive)
  (cond
   ((not agent-shell-discord-guild-id)
    (agent-shell-discord--log "Not starting: `agent-shell-discord-guild-id' unset"))
   ((not (agent-shell-discord--read-token))
    (agent-shell-discord--log "Not starting: no bot token in %s"
                              agent-shell-discord-authinfo-file))
   (t
    (setq agent-shell-discord--stopping nil
          agent-shell-discord--channel-map
          (or agent-shell-discord--channel-map
              (agent-shell-discord--load-map agent-shell-discord-channel-map-file))
          agent-shell-discord--thread-map
          (or agent-shell-discord--thread-map
              (agent-shell-discord--load-map agent-shell-discord-thread-map-file)))
    (add-hook 'agent-shell-mode-hook #'agent-shell-discord--register-buffer)
    (add-hook 'kill-emacs-hook #'agent-shell-discord--save-maps)
    (agent-shell-discord-sync)
    (agent-shell-discord--connect)
    (agent-shell-discord--log "Bridge started")
    (message "agent-shell-discord: started"))))

;;;###autoload
(defun agent-shell-discord-stop ()
  "Stop the Discord bridge and release all resources."
  (interactive)
  (setq agent-shell-discord--stopping t)
  (remove-hook 'agent-shell-mode-hook #'agent-shell-discord--register-buffer)
  (when agent-shell-discord--heartbeat-timer
    (cancel-timer agent-shell-discord--heartbeat-timer)
    (setq agent-shell-discord--heartbeat-timer nil))
  (when agent-shell-discord--reconnect-timer
    (cancel-timer agent-shell-discord--reconnect-timer)
    (setq agent-shell-discord--reconnect-timer nil))
  (when agent-shell-discord--ws
    (ignore-errors (websocket-close agent-shell-discord--ws))
    (setq agent-shell-discord--ws nil))
  (maphash (lambda (buffer _entry) (agent-shell-discord--detach-buffer buffer))
           (copy-hash-table agent-shell-discord--buffers))
  (agent-shell-discord--save-maps)
  (agent-shell-discord--log "Bridge stopped")
  (message "agent-shell-discord: stopped"))

;;;###autoload
(defun agent-shell-discord-status ()
  "Report the bridge connection state and show its log buffer."
  (interactive)
  (message "agent-shell-discord: gateway %s, %d session(s) tracked"
           (if agent-shell-discord--ws "connected" "disconnected")
           (hash-table-count agent-shell-discord--buffers))
  (display-buffer (get-buffer-create "*agent-shell-discord*")))

(provide 'agent-shell-discord)

;;; agent-shell-discord.el ends here
