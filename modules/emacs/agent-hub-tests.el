;;; agent-hub-tests.el --- Tests for agent-hub -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; ERT suite for `agent-hub'.  Three groups:
;;
;;   Group A  end-to-end performance against the real remote workspace already
;;            registered in the user's project list (measures wall-clock render
;;            latency; the regression gate for "reopen after 60s is slow").
;;   Group B  end-to-end functional -- a local git+session fixture rendered
;;            through the real magit-section insert path, driven by a synchronous
;;            async-runner so no network or waiting is involved.
;;   Group C  deterministic logic -- pure parsers and the stale-while-revalidate
;;            state machine, with the clock, timer, and subprocess runner mocked.
;;
;; How to run:
;;
;;   Live daemon (real project list + warm remote connection -> all groups):
;;     emacsclient -e '(progn
;;       (load "~/.config/emacs/lisp/agent-hub.el")
;;       (load "<repo>/modules/emacs/agent-hub-tests.el")
;;       (ert-run-tests-batch "agent-hub")
;;       nil)'
;;   Use `ert-run-tests-batch', NOT `...-and-exit', so the daemon survives.  Return
;;   nil afterward: returning the raw ERT stats object makes `emacsclient' spend a
;;   long time pretty-printing it.  Or run interactively with
;;   `M-x ert RET agent-hub RET'.
;;
;;   Batch, host-independent (Group C + local-fixture parts of B; A and the
;;   remote parts of B self-skip):
;;     emacs -Q --batch -l ert \
;;       -l <repo>/modules/emacs/agent-hub-tests.el \
;;       -f ert-run-tests-batch-and-exit

;;; Code:

(eval-and-compile
  (require 'ert)
  (require 'cl-lib)
  (require 'benchmark))

;; `agent-hub' requires `magit-section'.  When it is unavailable (bare `emacs -Q'
;; on a host without magit) provide just enough of a stub for the file to LOAD,
;; so Group C still runs; render tests gate on `agent-hub-test--magit-real'.
(eval-and-compile
  (defvar agent-hub-test--magit-real (require 'magit-section nil t)
    "Non-nil when the real `magit-section' is available (render tests need it).")

  (unless agent-hub-test--magit-real
    (defmacro magit-insert-section (_spec &rest body) `(progn ,@body))
    (defmacro magit-insert-section-body (&rest body) `(progn ,@body))
    (defun magit-insert-heading (&rest args)
      (when args (insert (mapconcat (lambda (x) (format "%s" x)) args "") "\n")))
    (defun magit-current-section () nil)
    (defun magit-section-ident (_section) nil)
    (defun magit-get-section (_ident) nil)
    (defun magit-section-toggle (_section) nil)
    (define-derived-mode magit-section-mode special-mode "MagitSection")
    (provide 'magit-section))

  ;; Load agent-hub.el from the same directory as this test file (unless already
  ;; loaded into a running Emacs, in which case that copy is used).
  (unless (featurep 'agent-hub)
    (load (expand-file-name
           "agent-hub.el"
           (file-name-directory (or load-file-name buffer-file-name default-directory)))
          nil t)))

(defvar agent-hub-test-render-budget 1.5
  "Upper bound (seconds) for a non-blocking render in the performance tests.
Generous vs. the pre-change baseline (~6.7s stale-cache reopen) so the gate is
robust across hosts while still catching a regression to synchronous fetching.")

(defmacro agent-hub-test--with-clean-state (&rest body)
  "Run BODY with fresh, isolated agent-hub caches and title state.
Rebinds every cache to a private hash table and the title cache to a temp file,
so a test never reads or mutates the live daemon's caches."
  (declare (indent 0) (debug t))
  `(let ((agent-hub--git-info-cache (make-hash-table :test #'equal))
         (agent-hub--inflight (make-hash-table :test #'equal))
         (agent-hub--projects-dir-cache (make-hash-table :test #'equal))
         (agent-hub--title-cache nil)
         (agent-hub--title-cache-dirty nil)
         (agent-hub--rerender-timer nil)
         (agent-hub-title-cache-file (make-temp-file "agent-hub-titles" nil ".eld")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file agent-hub-title-cache-file)))))

;;;; Group C -- pure parsers

(ert-deftest agent-hub-test-parse-numstat ()
  (should (equal (agent-hub--parse-numstat "3\t4\tfoo.el\n10\t0\tbar.el")
                 '(:add 13 :del 4)))
  ;; binary rows ("-\t-\t...") and blank lines contribute nothing
  (should (equal (agent-hub--parse-numstat "-\t-\tbin.png\n2\t1\tx") '(:add 2 :del 1)))
  (should (equal (agent-hub--parse-numstat "") '(:add 0 :del 0)))
  (should (equal (agent-hub--parse-numstat nil) '(:add 0 :del 0))))

(ert-deftest agent-hub-test-parse-default-branch ()
  (should (equal (agent-hub--parse-default-branch "origin/main") "main"))
  (should (equal (agent-hub--parse-default-branch "origin/develop") "develop"))
  (should (equal (agent-hub--parse-default-branch "trunk") "trunk"))
  (should (equal (agent-hub--parse-default-branch "") "main"))
  (should (equal (agent-hub--parse-default-branch nil) "main")))

(ert-deftest agent-hub-test-parse-current-branch ()
  (should (equal (agent-hub--parse-current-branch "feature/x") "feature/x"))
  (should (null (agent-hub--parse-current-branch "")))   ; detached HEAD
  (should (null (agent-hub--parse-current-branch nil))))

(ert-deftest agent-hub-test-parse-workspace-info ()
  (let* ((sep agent-hub--workspace-info-separator)
         (text (concat "origin/main\n" sep "\nfeat/x\n" sep "\n"
                       "worktree /repo\nHEAD abc\nbranch refs/heads/feat/x\n"))
         (info (agent-hub--parse-workspace-info "/repo" text)))
    (should (equal (plist-get info :default-branch) "main"))
    ;; current branch of the root is injected into :cwd-branches
    (should (equal (cdr (assoc "/repo" (plist-get info :cwd-branches))) "feat/x")))
  ;; no origin/HEAD -> default falls back to main; detached root -> no injection
  (let* ((sep agent-hub--workspace-info-separator)
         (info (agent-hub--parse-workspace-info "/repo" (concat "\n" sep "\n\n" sep "\n"))))
    (should (equal (plist-get info :default-branch) "main"))
    (should (null (plist-get info :cwd-branches)))))

(ert-deftest agent-hub-test-parse-pr ()
  (let ((view "{\"number\":7,\"state\":\"open\",\"isDraft\":false,\"url\":\"u\"}"))
    (should (equal (plist-get (agent-hub--parse-pr view) :number) 7))
    (should (equal (plist-get (agent-hub--parse-pr view) :state) "OPEN")))
  ;; list form: choose the most recently updated
  (let ((lst (concat "[{\"number\":1,\"state\":\"closed\",\"updatedAt\":\"2020\"},"
                     "{\"number\":2,\"state\":\"open\",\"updatedAt\":\"2026\"}]")))
    (should (equal (plist-get (agent-hub--parse-pr lst) :number) 2)))
  ;; draft overrides state
  (let ((draft "{\"number\":9,\"state\":\"open\",\"isDraft\":true}"))
    (should (equal (plist-get (agent-hub--parse-pr draft) :state) "DRAFT")))
  (should (null (agent-hub--parse-pr "")))
  (should (null (agent-hub--parse-pr "not json"))))

(defconst agent-hub-test--transcript
  (concat "# Agent Shell Transcript\r\n\r\n"
          "**Agent:** Claude\r\n"
          "**Started:** 2026-07-19 09:00:00\r\n"
          "**Working Directory:** /repo\r\n"
          "**Session ID:** session-1\r\n\r\n---\r\n\r\n"
          "## User (2026-07-19 09:01:00)\r\n\r\n"
          "Fix   login\r\n\r\n### ordinary heading\r\nmore details\r\n\r\n"
          "## Agent's Thoughts (2026-07-19 09:10:00)\r\n\r\nthink\r\n\r\n"
          "### Tool Call [2026-07-19 09:11:00]\r\n\r\ntool\r\n\r\n"
          "## Agent (2026-07-19 09:02:00)\r\n\r\ndone\r\n")
  "Sanitized synthetic agent-shell transcript used by tests.")

(ert-deftest agent-hub-test-parse-transcript-metadata ()
  (let ((m (agent-hub--parse-transcript-metadata
            agent-hub-test--transcript "/tmp/session.md")))
    (should (eq (plist-get m :agent) 'claude-code))
    (should (equal (plist-get m :id) "session-1"))
    (should (equal (plist-get m :cwd) "/repo"))
    (should (string-prefix-p "Fix login" (plist-get m :title)))
    ;; Thoughts and tool timestamps are excluded; last actual message is 09:02.
    (should (equal (format-time-string "%F %T" (plist-get m :last-message-at))
                   "2026-07-19 09:02:00"))))

(ert-deftest agent-hub-test-parse-transcript-malformed-and-unknown ()
  (let ((m (agent-hub--parse-transcript-metadata
            "**Agent:** Custom Bot\n---\n## User (bad)\nhello\n"
            "/tmp/bad.md" "/fallback")))
    (should (equal (plist-get m :agent) "custom bot"))
    (should-not (plist-get m :id))
    (should (equal (plist-get m :cwd) "/fallback"))
    (should (equal (plist-get m :title) "hello"))
    (should-not (plist-get m :time))))

(ert-deftest agent-hub-test-parse-transcript-preserves-remote-cwd ()
  (let ((m (agent-hub--parse-transcript-metadata
            (concat "**Agent:** Pi\n**Started:** 2026-07-19 09:00:00\n"
                    "**Working Directory:** /home/u/repo\n"
                    "**Session ID:** remote-1\n---\n")
            "/ssh:devbox:/home/u/repo/.agent-shell/transcripts/x.md"
            "/ssh:devbox:/home/u/repo")))
    (should (equal (plist-get m :cwd) "/ssh:devbox:/home/u/repo"))))

(ert-deftest agent-hub-test-parse-find-transcripts ()
  (let ((r (agent-hub--parse-find-transcripts
            "1783046848.98 123 /home/u/repo/.agent-shell/transcripts/a.md\r\n"
            "/ssh:h:/home/u/repo" "/ssh:h:")))
    (should (= (length r) 1))
    (should (equal (plist-get (car r) :file)
                   "/ssh:h:/home/u/repo/.agent-shell/transcripts/a.md"))
    (should (= (plist-get (car r) :size) 123))))

(ert-deftest agent-hub-test-parse-find-dirs ()
  (should (equal (agent-hub--parse-find-dirs "/home/u/repo/.agent-shell/worktrees/a\n/home/u/repo/.agent-shell/worktrees/b/\r\n"
                                             "/ssh:h:")
                 '("/ssh:h:/home/u/repo/.agent-shell/worktrees/a"
                   "/ssh:h:/home/u/repo/.agent-shell/worktrees/b")))
  (should (null (agent-hub--parse-find-dirs "" "/ssh:h:"))))

(ert-deftest agent-hub-test-github-repo-from-url ()
  (should (equal (agent-hub--github-repo-from-url "git@github.com:owner/repo.git") "owner/repo"))
  (should (equal (agent-hub--github-repo-from-url "https://github.com/owner/repo") "owner/repo"))
  (should (equal (agent-hub--github-repo-from-url "https://github.com/owner/repo.git") "owner/repo"))
  (should (null (agent-hub--github-repo-from-url "https://example.com/x/y"))))

;;;; Group C -- stale-while-revalidate state machine

(ert-deftest agent-hub-test-swr-stale-value-served ()
  (agent-hub-test--with-clean-state
    (cl-letf* ((clock 1000.0)
               ((symbol-function 'float-time) (lambda (&rest _) clock)))
      (agent-hub--cache-put "k" 'v)
      (should (agent-hub--cache-fresh-p (agent-hub--cache-entry "k")))
      (setq clock (+ 1000.0 agent-hub-git-info-cache-ttl 1))
      (should-not (agent-hub--cache-fresh-p (agent-hub--cache-entry "k")))
      (should (equal (agent-hub--cached "k") 'v))))) ; stale value still served

(ert-deftest agent-hub-test-needs-fetch-p ()
  (agent-hub-test--with-clean-state
    (cl-letf* ((clock 1000.0)
               ((symbol-function 'float-time) (lambda (&rest _) clock)))
      (should (agent-hub--needs-fetch-p "k"))          ; absent
      (agent-hub--cache-put "k" 'v)
      (should-not (agent-hub--needs-fetch-p "k"))       ; fresh
      (setq clock (+ 1000.0 agent-hub-git-info-cache-ttl 1))
      (should (agent-hub--needs-fetch-p "k"))           ; stale
      (puthash "k" t agent-hub--inflight)
      (should-not (agent-hub--needs-fetch-p "k")))))    ; stale but in-flight

(ert-deftest agent-hub-test-invalidate-all-keeps-values ()
  (agent-hub-test--with-clean-state
    (cl-letf* ((clock 1000.0)
               ((symbol-function 'float-time) (lambda (&rest _) clock)))
      (agent-hub--cache-put "k" 'v)
      (agent-hub--invalidate-all)
      (should (equal (agent-hub--cached "k") 'v))       ; value preserved
      (should (agent-hub--needs-fetch-p "k")))))        ; but marked stale

(ert-deftest agent-hub-test-swr-dedup ()
  (agent-hub-test--with-clean-state
    (let* ((calls 0)
           ;; runner that never calls back -> key stays in-flight
           (agent-hub--async-runner (lambda (&rest _) (setq calls (1+ calls)))))
      (agent-hub--swr "k" nil #'identity "true")
      (agent-hub--swr "k" nil #'identity "true")
      (should (= calls 1)))))

(ert-deftest agent-hub-test-fetch-complete-updates-and-repaints ()
  (agent-hub-test--with-clean-state
    (let ((renders 0))
      (cl-letf (((symbol-function 'agent-hub--schedule-rerender)
                 (lambda () (setq renders (1+ renders)))))
        (let ((agent-hub--async-runner
               (lambda (_k _d _p _a cb) (funcall cb t "value"))))
          (should (equal (agent-hub--swr "k" nil #'identity "x") "value"))
          (should-not (gethash "k" agent-hub--inflight))
          (should (= renders 1)))))))

(ert-deftest agent-hub-test-fetch-complete-storm-guard ()
  (agent-hub-test--with-clean-state
    (let ((renders 0))
      (cl-letf (((symbol-function 'agent-hub--schedule-rerender)
                 (lambda () (setq renders (1+ renders)))))
        (agent-hub--cache-put "k" "same")
        (agent-hub--invalidate-all)          ; stale but value "same"
        (let ((agent-hub--async-runner
               (lambda (_k _d _p _a cb) (funcall cb t "same"))))
          (agent-hub--swr "k" nil #'identity "x")
          (should (equal (agent-hub--cached "k") "same"))
          (should (= renders 0)))))))        ; unchanged -> no repaint

(ert-deftest agent-hub-test-fetch-failure-negative-cache ()
  (agent-hub-test--with-clean-state
    (cl-letf* ((clock 1000.0)
               ((symbol-function 'float-time) (lambda (&rest _) clock))
               ((symbol-function 'agent-hub--schedule-rerender) #'ignore))
      (let ((agent-hub--async-runner
             (lambda (_k _d _p _a cb) (funcall cb nil "")))) ; failure
        (agent-hub--swr "k" nil #'identity "x")
        (should-not (agent-hub--cached "k"))
        (should-not (gethash "k" agent-hub--inflight))
        (should-not (agent-hub--needs-fetch-p "k")))))) ; fresh negative, no refetch loop

(ert-deftest agent-hub-test-swr-convergence ()
  "A completed fetch unblocks its dependents; fresh entries never refetch."
  (agent-hub-test--with-clean-state
    (cl-letf (((symbol-function 'agent-hub--schedule-rerender) #'ignore))
      (let* ((spawns nil)
             ;; runner records the key and leaves it in-flight (we complete by hand)
             (agent-hub--async-runner (lambda (key &rest _) (push key spawns))))
        ;; wave 1: workspace-info unknown -> fetched
        (should (null (agent-hub--git-workspace-info "/x")))
        (should (member "git:/x" spawns))
        (agent-hub--fetch-complete
         "git:/x" '(:default-branch "main" :cwd-branches (("/x" . "feat"))))
        ;; wave 2: branch now known -> branch-diff becomes computable and is fetched
        (setq spawns nil)
        (should (null (agent-hub--git-branch-diff "/x" "feat" "main")))
        (should (member "branch-diff:/x:main:feat" spawns))
        (agent-hub--fetch-complete "branch-diff:/x:main:feat" '(:add 1 :del 2))
        ;; steady state: fresh entry served with no new spawn
        (setq spawns nil)
        (should (equal (agent-hub--git-branch-diff "/x" "feat" "main") '(:add 1 :del 2)))
        (should (null spawns))))))

;;;; Group C -- recent-session merge/filter logic

(ert-deftest agent-hub-test-workspace-for-cwd-most-specific ()
  (should (equal (agent-hub--workspace-for-cwd
                  "/repo/sub/worktree" '("/repo" "/repo/sub"))
                 "/repo/sub"))
  (should (equal (agent-hub--workspace-for-cwd
                  "/ssh:host:/repo/.agent-shell/worktrees/feat"
                  '("/ssh:host:/repo" "/local/repo"))
                 "/ssh:host:/repo"))
  (should-not (agent-hub--workspace-for-cwd "/other" '("/repo"))))

(ert-deftest agent-hub-test-merge-recent-dedup-and-order ()
  (let* ((agent-hub-recent-session-limit 5)
         (agent-hub-recent-session-max-age-days nil)
         (older (seconds-to-time 100))
         (newer (seconds-to-time 200))
         (persisted (list (list :id "same" :agent 'claude-code :time older
                                :title "Disk title" :cwd "/repo" :root "/repo")
                          (list :id "same" :agent 'codex :time older
                                :title "Codex" :cwd "/repo" :root "/repo")
                          (list :id "new" :agent 'claude-code :time newer
                                :title "New" :cwd "/repo" :root "/repo")))
         (live (list (list :id "same" :agent 'claude-code
                           :title "Buffer title" :cwd "/repo"
                           :buffer 'same-buffer :root "/repo")
                     (list :id nil :agent 'pi :title "Initializing" :cwd "/repo"
                           :buffer 'init-buffer :root "/repo")))
         (result (agent-hub--merge-recent-sessions persisted live
                                                    (seconds-to-time 300))))
    (should (= (length result) 4))
    (should-not (plist-get (car result) :id))
    (should (= (length (seq-filter (lambda (s) (equal (plist-get s :id) "same"))
                                   result))
               2))
    (let ((same (seq-find (lambda (s) (and (equal (plist-get s :id) "same")
                                            (eq (plist-get s :agent) 'claude-code)))
                          result)))
      (should (eq (plist-get same :buffer) 'same-buffer))
      (should (equal (plist-get same :title) "Disk title")))))

(ert-deftest agent-hub-test-merge-recent-live-bypasses-filters ()
  (let* ((agent-hub-recent-session-limit 2)
         (agent-hub-recent-session-max-age-days 1)
         (now (seconds-to-time (* 10 86400)))
         (old (seconds-to-time 0))
         (fresh (seconds-to-time (- (* 10 86400) 60)))
         (persisted (list (list :id "old" :time old)
                          (list :id "fresh-1" :time fresh)
                          (list :id "fresh-2" :time (time-subtract fresh (seconds-to-time 1)))))
         (live (list (list :id "live-1" :time old :buffer 'one)
                     (list :id "live-2" :time old :buffer 'two)
                     (list :id "live-3" :time nil :buffer 'three)))
         (result (agent-hub--merge-recent-sessions persisted live now)))
    (should (= (length result) 3))
    (should (equal (sort (delq nil (mapcar (lambda (s) (plist-get s :id)) result))
                         #'string-lessp)
                   '("live-1" "live-2" "live-3")))))

(ert-deftest agent-hub-test-merge-recent-fills-limit-with-persisted ()
  (let* ((agent-hub-recent-session-limit 3)
         (agent-hub-recent-session-max-age-days nil)
         (persisted (list (list :id "new" :time (seconds-to-time 30))
                          (list :id "middle" :time (seconds-to-time 20))
                          (list :id "old" :time (seconds-to-time 10))))
         (live (list (list :id "live" :time (seconds-to-time 15) :buffer 'live)))
         (result (agent-hub--merge-recent-sessions
                  persisted live (seconds-to-time 40))))
    (should (equal (mapcar (lambda (s) (plist-get s :id)) result)
                   '("new" "middle" "live")))))

(ert-deftest agent-hub-test-persisted-session-preserves-tramp-paths ()
  (let* ((file "/ssh:devbox:/home/u/repo/.agent-shell/transcripts/sess.md")
         (cwd "/ssh:devbox:/home/u/repo")
         (session (agent-hub--persisted-session
                   cwd (list :id "sess" :agent 'pi :file file
                             :time (seconds-to-time 1) :cwd cwd))))
    (should (equal (plist-get session :id) "sess"))
    (should (equal (plist-get session :file) file))
    (should (equal (plist-get session :cwd) cwd))
    (should (equal (plist-get session :root) cwd))))

;;;; Group C -- deletion cleanup helpers

(ert-deftest agent-hub-test-protected-branch-p ()
  (should (agent-hub--protected-branch-p nil "trunk"))
  (should (agent-hub--protected-branch-p "" "trunk"))
  (should (agent-hub--protected-branch-p "main" "trunk"))
  (should (agent-hub--protected-branch-p "trunk" "trunk"))
  (should-not (agent-hub--protected-branch-p "feature/x" "main")))

(ert-deftest agent-hub-test-agent-worktree-cwd-p ()
  (cl-letf (((symbol-function 'agent-hub--worktrees-dir)
             (lambda (root) (expand-file-name ".agent-shell/worktrees/" root))))
    (should-not (agent-hub--agent-worktree-cwd-p "/repo" "/repo"))
    (should (agent-hub--agent-worktree-cwd-p
             "/repo" "/repo/.agent-shell/worktrees/feat"))
    (should-not (agent-hub--agent-worktree-cwd-p
                 "/repo" "/repo/.agent-shell/worktrees/feat/sub"))
    (should-not (agent-hub--agent-worktree-cwd-p "/repo" "/other/feat"))))

(ert-deftest agent-hub-test-worktree-branch-for-cwd ()
  (cl-letf (((symbol-function 'agent-hub--git)
             (lambda (_root &rest _args)
               "worktree /repo\nHEAD abc\nbranch refs/heads/main\n\nworktree /repo/.agent-shell/worktrees/feat\nHEAD def\nbranch refs/heads/feature/x\n")))
    (should (equal (agent-hub--worktree-branch-for-cwd
                    "/repo" "/repo/.agent-shell/worktrees/feat")
                   "feature/x"))))

;;;; Group B -- end-to-end functional (real magit-section, local fixture)

(defun agent-hub-test--sync-runner (_key dir program args callback)
  "Synchronous stand-in for `agent-hub--async-runner' used in fixture tests.
Runs PROGRAM ARGS in DIR via `process-file' and invokes CALLBACK immediately."
  (with-temp-buffer
    (let ((default-directory (or dir default-directory))
          (process-connection-type nil))
      (let ((code (apply #'process-file program nil t nil args)))
        (funcall callback (and (integerp code) (zerop code)) (buffer-string))))))

(defmacro agent-hub-test--in-fixture (vars &rest body)
  "Build a temp HOME with a git repo and synthetic transcript, then run BODY.
VARS is (HOME REPO) bound to the temp home and repo paths."
  (declare (indent 1) (debug t))
  (let ((home (nth 0 vars)) (repo (nth 1 vars)))
    `(let* ((,home (make-temp-file "agent-hub-home" t))
            (,repo (expand-file-name "myrepo" ,home))
            (process-environment (cons (concat "HOME=" ,home) process-environment)))
       (unwind-protect
           (progn
             (make-directory ,repo t)
             (let ((default-directory ,repo))
               (call-process "git" nil nil nil "init")
               (call-process "git" nil nil nil "config" "user.email" "t@example.com")
               (call-process "git" nil nil nil "config" "user.name" "Test")
               (write-region "hi\n" nil (expand-file-name "README" ,repo))
               (call-process "git" nil nil nil "add" "-A")
               (call-process "git" nil nil nil "commit" "-m" "init")
               (call-process "git" nil nil nil "branch" "-M" "main"))
             (agent-hub-test--write-session ,home ,repo "sess1" "Fix the login bug")
             ,@body)
         (ignore-errors (delete-directory ,home t))))))

(defun agent-hub-test--goto-line (pred)
  "Move point to the first dashboard line satisfying PRED."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (funcall pred (line-beginning-position))
        (throw 'found t))
      (forward-line 1))
    nil))

(defun agent-hub-test--goto-line-type (type)
  "Move point to the first dashboard line carrying TYPE."
  (agent-hub-test--goto-line
   (lambda (bol) (eq (get-text-property bol 'agent-hub-type) type))))

(defun agent-hub-test--session-file (_home cwd id)
  "Return synthetic transcript file for CWD and ID."
  (let ((dir (expand-file-name ".agent-shell/transcripts/" cwd)))
    (make-directory dir t)
    (expand-file-name (concat id ".md") dir)))

(defun agent-hub-test--write-session (home cwd id title &optional agent)
  "Write a sanitized transcript TITLE for CWD and return its file."
  (let ((file (agent-hub-test--session-file home cwd id)))
    (write-region
     (format (concat "# Agent Shell Transcript\n\n**Agent:** %s\n"
                     "**Started:** 2026-07-19 09:00:00\n"
                     "**Working Directory:** %s\n**Session ID:** %s\n\n---\n\n"
                     "## User (2026-07-19 09:01:00)\n\n%s\n\n"
                     "## Agent (2026-07-19 09:02:00)\n\nDone\n")
             (or agent "Claude") cwd id title)
     nil file)
    file))

(defun agent-hub-test--make-worktree (repo name branch)
  "Create a clean git worktree NAME on BRANCH under REPO."
  (let ((path (expand-file-name name (expand-file-name ".agent-shell/worktrees" repo))))
    (make-directory (file-name-directory path) t)
    (let ((default-directory repo))
      (call-process "git" nil nil nil "worktree" "add" "-b" branch path "HEAD"))
    path))

(defun agent-hub-test--branch-exists-p (repo branch)
  "Return non-nil when BRANCH exists in REPO."
  (let ((default-directory repo))
    (eq 0 (call-process "git" nil nil nil "rev-parse" "--verify" "--quiet"
                        (concat "refs/heads/" branch)))))

(defun agent-hub-test--delete-sessions (sessions)
  "Delete SESSIONS without interactive prompts and return the result plist."
  (cl-letf (((symbol-function 'agent-hub--agent-buffers) (lambda () nil)))
    (agent-hub--apply-delete-plan (agent-hub--delete-plan sessions))))

(defun agent-hub-test--render-expand (repo)
  "Render the dashboard in the current buffer and expand REPO's workspace."
  (agent-hub-mode)
  (let ((inhibit-read-only t)) (agent-hub--render))
  (when (agent-hub-test--goto-line
         (lambda (bol)
           (and (eq (get-text-property bol 'agent-hub-type) 'workspace)
                (equal (get-text-property bol 'agent-hub-root) repo))))
    (magit-section-toggle (magit-current-section))))

(ert-deftest agent-hub-test-local-render-e2e ()
  (skip-unless agent-hub-test--magit-real)
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (cl-letf (((symbol-function 'project-known-project-roots) (lambda () (list repo)))
                ((symbol-function 'agent-hub--agent-buffers) (lambda () nil))
                (agent-hub--async-runner #'agent-hub-test--sync-runner))
        (with-temp-buffer
          (agent-hub-test--render-expand repo)
          (let ((text (buffer-string)))
            (should (string-match-p "Recent Sessions (1)" text))
            (should (string-match-p (file-name-nondirectory repo) text))
            (should (string-match-p "Fix the login bug" text))
            (should (string-match-p "1 session" text))))))))

(ert-deftest agent-hub-test-recent-session-resume-e2e ()
  (skip-unless agent-hub-test--magit-real)
  (dolist (case '(("Claude" claude-code agent-shell-anthropic-make-claude-code-config)
                  ("Codex" codex agent-shell-openai-make-codex-config)
                  ("Pi" pi agent-shell-pi-make-agent-config)))
    (agent-hub-test--with-clean-state
      (agent-hub-test--in-fixture (home repo)
        (agent-hub-test--write-session home repo "sess1" "Resume me" (car case))
        (let (start-args resumed-directory resumed-transcript)
          (cl-letf (((symbol-function 'project-known-project-roots) (lambda () (list repo)))
                    ((symbol-function 'agent-hub--agent-buffers) (lambda () nil))
                    ((symbol-function 'require) (lambda (&rest _) t))
                    ((symbol-function (nth 2 case)) (lambda () 'test-config))
                    ((symbol-function 'agent-shell-start)
                     (lambda (&rest args)
                       (setq start-args args
                             resumed-directory default-directory
                             resumed-transcript
                             (and (functionp agent-shell-transcript-file-path-function)
                                  (funcall agent-shell-transcript-file-path-function)))))
                    (agent-hub--async-runner #'agent-hub-test--sync-runner))
            (with-temp-buffer
              (agent-hub-mode)
              (let ((inhibit-read-only t)) (agent-hub--render))
              (should (agent-hub-test--goto-line-type 'session))
              (agent-hub-visit))
            (should (equal (plist-get start-args :session-id) "sess1"))
            (should (equal (plist-get start-args :config) 'test-config))
            (should (equal resumed-directory (file-name-as-directory repo)))
            ;; Resume reuses the selected transcript rather than minting a new
            ;; timestamped file.
            (should (stringp resumed-transcript))
            (should (string-suffix-p ".md" resumed-transcript))
            (should (file-exists-p resumed-transcript))))))))

(ert-deftest agent-hub-test-mark-session-advances-point ()
  (skip-unless agent-hub-test--magit-real)
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (agent-hub-test--write-session home repo "sess2" "Add the logout button")
      (cl-letf (((symbol-function 'project-known-project-roots) (lambda () (list repo)))
                ((symbol-function 'agent-hub--agent-buffers) (lambda () nil))
                (agent-hub--async-runner #'agent-hub-test--sync-runner))
        (with-temp-buffer
          (agent-hub-mode)
          (let ((inhibit-read-only t)) (agent-hub--render))
          (should (agent-hub-test--goto-line-type 'session))
          (let ((first-file (agent-hub--session-file
                             (agent-hub--session-at-point))))
            (agent-hub-mark-session)
            ;; Point lands on a session line, and it is a different session
            ;; than the one we just marked -- i.e. it advanced.
            (should (eq (agent-hub--type-at-point) 'session))
            (should-not (equal first-file
                               (agent-hub--session-file
                                (agent-hub--session-at-point))))
            ;; The advanced-to session is unmarked, and unmarking advances too.
            (should-not (agent-hub--session-marked-p
                         (agent-hub--session-at-point)))))))))

(ert-deftest agent-hub-test-title-cache-persistence ()
  (skip-unless agent-hub-test--magit-real)
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (cl-letf* (((symbol-function 'project-known-project-roots) (lambda () (list repo)))
                 (agent-hub--async-runner #'agent-hub-test--sync-runner)
                 (parses 0)
                 (orig (symbol-function 'agent-hub--parse-transcript-metadata))
                 ((symbol-function 'agent-hub--parse-transcript-metadata)
                  (lambda (&rest args)
                    (setq parses (1+ parses))
                    (apply orig args))))
        (with-temp-buffer (agent-hub-test--render-expand repo))
        (should (>= parses 1))
        (should (file-exists-p agent-hub-title-cache-file))
        (setq parses 0)
        (with-temp-buffer (agent-hub-test--render-expand repo))
        (should (= parses 0))))))

(ert-deftest agent-hub-test-local-transcript-cold-miss-is-async ()
  (agent-hub-test--with-clean-state
    (let* ((file (make-temp-file "agent-hub-transcript" nil ".md"
                                 agent-hub-test--transcript))
           (attrs (file-attributes file))
           (entry (list :file file :cwd "/repo"
                        :mtime (file-attribute-modification-time attrs)
                        :size (file-attribute-size attrs)))
           (spawns 0)
           (agent-hub--async-runner
            (lambda (&rest _args) (setq spawns (1+ spawns)))))
      (unwind-protect
          (cl-letf (((symbol-function 'insert-file-contents)
                     (lambda (&rest _) (ert-fail "render opened transcript"))))
            (let ((metadata (agent-hub--transcript-metadata-cached entry)))
              (should (= spawns 1))
              (should (equal (plist-get metadata :file) file))))
        (delete-file file)))))

(ert-deftest agent-hub-test-delete-root-session-keeps-main-worktree ()
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (let* ((file (agent-hub-test--session-file home repo "sess1"))
             (session (list :id "sess1" :file file :cwd repo :root repo))
             (result (agent-hub-test--delete-sessions (list session))))
        (should-not (file-exists-p file))
        (should (file-directory-p repo))
        (should (agent-hub-test--branch-exists-p repo "main"))
        (should (= (plist-get result :worktrees-removed) 0))
        (should (= (plist-get result :branches-deleted) 0))))))

(ert-deftest agent-hub-test-delete-shared-worktree-keeps-worktree-and-branch ()
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (let* ((worktree (agent-hub-test--make-worktree repo "feat" "feature/shared"))
             (file1 (agent-hub-test--write-session home worktree "sess-a" "A"))
             (file2 (agent-hub-test--write-session home worktree "sess-b" "B"))
             (session (list :id "sess-a" :file file1 :cwd worktree :root repo))
             (result (agent-hub-test--delete-sessions (list session))))
        (should-not (file-exists-p file1))
        (should (file-exists-p file2))
        (should (file-directory-p worktree))
        (should (agent-hub-test--branch-exists-p repo "feature/shared"))
        (should (= (plist-get result :worktrees-removed) 0))
        (should (= (plist-get result :branches-deleted) 0))))))

(ert-deftest agent-hub-test-delete-last-worktree-session-keeps-worktree-and-branch ()
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (let* ((worktree (agent-hub-test--make-worktree repo "feat" "feature/delete"))
             (file (agent-hub-test--write-session home worktree "sess-a" "A"))
             (session (list :id "sess-a" :file file :cwd worktree :root repo))
             (result (agent-hub-test--delete-sessions (list session))))
        (should-not (file-exists-p file))
        (should (file-exists-p worktree))
        (should (agent-hub-test--branch-exists-p repo "feature/delete"))
        (should (= (plist-get result :worktrees-removed) 0))
        (should (= (plist-get result :branches-deleted) 0))))))

(ert-deftest agent-hub-test-delete-batch-same-worktree-keeps-worktree ()
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (let* ((worktree (agent-hub-test--make-worktree repo "feat" "feature/batch"))
             (file1 (agent-hub-test--write-session home worktree "sess-a" "A"))
             (file2 (agent-hub-test--write-session home worktree "sess-b" "B"))
             (sessions (list (list :id "sess-a" :file file1 :cwd worktree :root repo)
                             (list :id "sess-b" :file file2 :cwd worktree :root repo)))
             (result (agent-hub-test--delete-sessions sessions)))
        (should-not (file-exists-p file1))
        (should-not (file-exists-p file2))
        (should (file-exists-p worktree))
        (should (agent-hub-test--branch-exists-p repo "feature/batch"))
        (should (= (plist-get result :worktrees-removed) 0))
        (should (= (plist-get result :branches-deleted) 0))))))

(ert-deftest agent-hub-test-delete-batch-mixed-root-and-worktree ()
  (agent-hub-test--with-clean-state
    (agent-hub-test--in-fixture (home repo)
      (let* ((worktree (agent-hub-test--make-worktree repo "feat" "feature/mixed"))
             (root-file (agent-hub-test--write-session home repo "root-sess" "Root"))
             (worktree-file (agent-hub-test--write-session home worktree "wt-sess" "WT"))
             (sessions (list (list :id "root-sess" :file root-file :cwd repo :root repo)
                             (list :id "wt-sess" :file worktree-file
                                   :cwd worktree :root repo)))
             (result (agent-hub-test--delete-sessions sessions)))
        (should-not (file-exists-p root-file))
        (should-not (file-exists-p worktree-file))
        (should (file-directory-p repo))
        (should (agent-hub-test--branch-exists-p repo "main"))
        (should (file-exists-p worktree))
        (should (agent-hub-test--branch-exists-p repo "feature/mixed"))
        (should (= (plist-get result :worktrees-removed) 0))
        (should (= (plist-get result :branches-deleted) 0))))))

;;;; Group A -- performance against the real remote workspace (live daemon)

(defun agent-hub-test--remote-root ()
  "Return a reachable remote workspace root, or nil.
Remote performance tests intentionally run only in the live daemon.  In
`emacs --batch' they are skipped: batch is for parser/SWR/local-fixture tests,
while the daemon carries the real project list and warm TRAMP connection."
  (and (not noninteractive)
       (let ((root (seq-find #'file-remote-p (agent-hub--workspaces))))
         (and root (ignore-errors (file-exists-p root)) root)))) ; warms connection

(defun agent-hub-test--goto-workspace (root)
  "Move point to ROOT's workspace heading in the current agent-hub buffer."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (and (eq (get-text-property (line-beginning-position) 'agent-hub-type)
                     'workspace)
                 (equal (get-text-property (line-beginning-position) 'agent-hub-root)
                        root))
        (throw 'found t))
      (forward-line 1))
    nil))

(ert-deftest agent-hub-test-perf-reopen-stale-cache ()
  "Reopening after the cache expired must not block (the 6.7s -> ~0.1s win)."
  (skip-unless agent-hub-test--magit-real)
  (let ((root (agent-hub-test--remote-root)))
    (skip-unless root)
    (save-window-excursion (agent-hub))
    (agent-hub-refresh)                 ; warm
    (sit-for 0.1)
    (agent-hub--invalidate-all)         ; simulate the 60s-later expiry
    (should (< (benchmark-elapse (agent-hub-refresh)) agent-hub-test-render-budget))))

(ert-deftest agent-hub-test-perf-expand-remote ()
  "Expanding a remote workspace must not block."
  (skip-unless agent-hub-test--magit-real)
  (let ((root (agent-hub-test--remote-root)))
    (skip-unless root)
    (save-window-excursion (agent-hub))
    (agent-hub-refresh)
    (with-current-buffer agent-hub-buffer-name
      (should (agent-hub-test--goto-workspace root))
      (should (< (benchmark-elapse (magit-section-toggle (magit-current-section)))
                 agent-hub-test-render-budget)))))

(ert-deftest agent-hub-test-perf-nonblocking-async ()
  "Render returns immediately while background fetches are still in flight."
  (skip-unless agent-hub-test--magit-real)
  (let ((root (agent-hub-test--remote-root)))
    (skip-unless root)
    (save-window-excursion (agent-hub))
    (agent-hub-refresh)
    (sit-for 0.1)
    (agent-hub--invalidate-all)
    (agent-hub-refresh)
    (with-current-buffer agent-hub-buffer-name
      (should (agent-hub-test--goto-workspace root))
      ;; Expanding the real remote workspace enqueues its async session/body
      ;; fetches; the toggle returns before those fetches complete.
      (magit-section-toggle (magit-current-section))
      (should (string-match-p (regexp-quote (agent-hub--workspace-name root))
                              (buffer-string))))
    ;; The render/toggle above already returned; revalidation is happening off
    ;; the main loop.
    (should (agent-hub--inflight-p))))

(provide 'agent-hub-tests)

;;; agent-hub-tests.el ends here
