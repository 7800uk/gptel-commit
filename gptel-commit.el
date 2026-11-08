;;; gptel-commit.el --- Generate commit message with gptel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Liu Bo

;; Author: Liu Bo <liubolovelife@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8"))
;; Keywords: vc, convenience
;; URL: https://github.com/lakkiy/gptel-commit

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides functions to generate Git commit messages using GPTel.
;; It also supports Claude Code as an alternative backend.
;; It analyzes staged changes and generates appropriate commit messages following
;; conventional Git commit formats.
;;
;; Main functions:
;; - `gptel-commit': Generate commit message directly
;; - `gptel-commit-rationale': Generate with optional context/rationale
;;
;; The package supports streaming responses and excludes specified file patterns
;; from diff analysis to focus on relevant changes.
;;
;; `gptel-commit-generate' is the same generation without a commit buffer:
;; it hands the message to a callback, for code that commits on its own
;; (an auto-commit on save, say).
;;
;; This fork (github.com/7800uk/gptel-commit) is kept current with Emacs
;; master (32.0.50) and the Emacs 31 release branch: it byte-compiles
;; without warnings on both, quotes file names it hands to git, and lets
;; the commit model differ from the chat model.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)

(defgroup gptel-commit nil
  "Generate commit messages with GPTel."
  :group 'vc
  :group 'gptel)

(defcustom gptel-commit-use-claude-code nil
  "Whether to use Claude Code CLI instead of GPTel for commit message generation."
  :type 'boolean
  :group 'gptel-commit)

(defcustom gptel-commit-claude-command "claude"
  "Command to run Claude Code CLI.
Can be a command name in PATH or absolute path to the executable."
  :type 'string
  :group 'gptel-commit)

(defcustom gptel-commit-stream t
  "Whether to stream commit message generation.
Set to nil if your backend doesn't support streaming."
  :type 'boolean
  :group 'gptel-commit)

(defcustom gptel-commit-claude-debug nil
  "Enable debug logging for Claude Code backend.
When enabled, Claude CLI commands and responses are shown in the
*gptel-commit-claude-debug* buffer.  For GPTel debugging, use
`gptel-log-level' instead."
  :type 'boolean
  :group 'gptel-commit)

(defcustom gptel-commit-loading-message "# Generating commit message with gptel..."
  "Message shown in the commit buffer while generating.
Set to nil to disable the loading indicator."
  :type '(choice string (const :tag "Disabled" nil))
  :group 'gptel-commit)

(defvar gptel-commit-prompt
  "You are an expert at writing Git commit messages.
Generate **only** the commit message, nothing else.

CRITICAL: OUTPUT PLAIN TEXT ONLY - NO markdown formatting, NO code
blocks, NO backticks, NO **bold** or *italic*. Just raw text.

DECISION PROCESS:
1. Count changed files
2. If 1 file: check if change is simple or complex
3. Apply the appropriate format

FORMAT RULES:

A. Single File + Simple Change (one clear purpose):
   * path/to/file: Description. (≤72 chars)

   NO subject line, NO blank lines, JUST this one line.

B. Single File + Complex Change (multiple purposes/major refactor):
   Subject line (≤50 chars, imperative mood, NO period)

   Optional body paragraph explaining why (wrap at 72 chars).

   * path/to/file (func1, func2): Description.

C. Multiple Files (2+ files changed):
   Subject line (≤50 chars, imperative mood, NO period)

   Optional body paragraph explaining why (wrap at 72 chars).

   * path/to/file1 (func1): Description.
   * path/to/file2 (func2): Another description.

D. Trivial Changes:
   Add `; ` prefix for typos/comments/docs.
   Example: `; * file: Fix typo.`

SIMPLE vs COMPLEX (single file):
- Simple: one function, one clear fix/addition
- Complex: multiple functions, refactoring, or architectural change"
  "A prompt adapted from Emacs.")

(defvar gptel-commit-after-insert-hook nil
  "Hook run when commit message is inserted (both GPTel and Claude Code).")

(defcustom gptel-commit-backend nil
  "The GPTel backend used for generating commit messages, or nil.
Nil means whatever `gptel-backend' is when a message is generated.
This can be set to a lightweight or free model (e.g., via OpenRouter),
so it won't interfere with your default `gptel' usage for general chat.
Only used when `gptel-commit-use-claude-code' is nil."
  :type '(choice (const :tag "Use `gptel-backend'" nil)
                 (restricted-sexp :tag "Backend" :match-alternatives (gptel-backend-p)))
  :group 'gptel-commit)

(defcustom gptel-commit-model nil
  "The model, a symbol, used for generating commit messages, or nil.
Nil means whatever `gptel-model' is when a message is generated.  A
model given here must be one `gptel-commit-backend' carries."
  :type '(choice (const :tag "Use `gptel-model'" nil) symbol)
  :group 'gptel-commit)

(defcustom gptel-commit-max-diff-chars 60000
  "Largest staged diff, in characters, sent to the model, or nil for no limit.
A longer diff is cut off at this length and the model is told so.  The
diffstat of every staged file is always sent in full ahead of the diff,
so a cut-off diff still names everything the commit touches."
  :type '(choice (const :tag "No limit" nil) natnum)
  :group 'gptel-commit)

(defvar gptel-commit-diff-excludes
  '("pnpm-lock.yaml"
    "*.lock"
    "ent/**/*.go")
  "List of file globs to exclude from commit diff analysis.")

(defvar gptel-commit--current-buffer nil
  "Buffer where commit message is being generated.")

(defvar gptel-commit-rationale-buffer "*GPTel Commit Rationale*"
  "Buffer name for entering rationale for commit message generation.")

(defvar gptel-commit-claude-debug-buffer "*gptel-commit-claude-debug*"
  "Buffer name for Claude Code debug output.")

(defvar gptel-commit--insert-position nil
  "Position where commit message should be inserted.")

(defvar gptel-commit--loading-beg nil
  "Start position of loading indicator.")

(defvar gptel-commit--loading-end nil
  "End position of loading indicator.")

(defvar gptel-commit--claude-process nil
  "Current Claude Code process.")

(defun gptel-commit--claude-debug-log (message &rest args)
  "Log MESSAGE with ARGS to Claude debug buffer if debug is enabled."
  (when gptel-commit-claude-debug
    (with-current-buffer (get-buffer-create gptel-commit-claude-debug-buffer)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (apply #'format message args))
      (insert "\n"))))

(defun gptel-commit--insert-loading ()
  "Insert loading indicator at `gptel-commit--insert-position'."
  (when gptel-commit-loading-message
    (gptel-commit--remove-loading)
    (with-current-buffer gptel-commit--current-buffer
      (save-excursion
        (goto-char gptel-commit--insert-position)
        (setq gptel-commit--loading-beg (point))
        (insert gptel-commit-loading-message "\n")
        (setq gptel-commit--loading-end (point))
        (setq gptel-commit--insert-position (point))))))

(defun gptel-commit--remove-loading ()
  "Remove loading indicator from the commit buffer."
  (when (and gptel-commit--loading-beg gptel-commit--loading-end)
    (when (buffer-live-p gptel-commit--current-buffer)
      (with-current-buffer gptel-commit--current-buffer
        (let ((beg gptel-commit--loading-beg)
              (end gptel-commit--loading-end))
          (when (and (<= beg (point-max)) (<= end (point-max)))
            (delete-region beg end))
          (when (= gptel-commit--insert-position end)
            (setq gptel-commit--insert-position beg)))))
    (setq gptel-commit--loading-beg nil
          gptel-commit--loading-end nil)))

(defun gptel-commit--wildcard-to-regexp (glob)
  "Convert shell glob GLOB to a regular expression."
  (let ((glob (replace-regexp-in-string "\\.\\*" ".*" glob)))
    (wildcard-to-regexp glob)))

(defun gptel-commit--excluded-file-p (filename)
  "Check if FILENAME matches any pattern in `gptel-commit-diff-excludes`."
  (cl-some (lambda (pat)
             (string-match-p (gptel-commit--wildcard-to-regexp pat) filename))
           gptel-commit-diff-excludes))

(defun gptel-commit--git (&rest args)
  "Run git ARGS in `default-directory' and return its output.
Arguments go to git directly, never through a shell, so a file name
with spaces or shell metacharacters in it arrives as one argument."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil args)
    (buffer-string)))

(defun gptel-commit--truncate (diff)
  "Cut DIFF down to `gptel-commit-max-diff-chars', saying so if it was."
  (if (and gptel-commit-max-diff-chars
           (> (length diff) gptel-commit-max-diff-chars))
      (concat (substring diff 0 gptel-commit-max-diff-chars)
              (format "\n\n[Diff truncated: %d of %d characters shown.  \
The diffstat above lists every staged file.]"
                      gptel-commit-max-diff-chars (length diff)))
    diff))

(defun gptel-commit--filtered-diff ()
  "Return a filtered diff string of staged changes, excluding patterns.
The diffstat of all staged changes comes first, then each included
file's diff, cut to `gptel-commit-max-diff-chars' overall."
  (let* ((files (split-string
                 (gptel-commit--git "diff" "--cached" "--name-only" "-z")
                 "\0" t))
         (included-files (cl-remove-if #'gptel-commit--excluded-file-p files))
         (diffs '()))
    (dolist (file included-files)
      (let ((diff (gptel-commit--git "diff" "--cached" "--" file)))
        (unless (string-empty-p diff)
          (push (format "===== %s =====\n%s" file diff) diffs))))
    (when diffs
      (concat (string-trim-right (gptel-commit--git "diff" "--cached" "--stat"))
              "\n\n"
              (gptel-commit--truncate (string-join (nreverse diffs) "\n\n"))))))

(defun gptel-commit--find-commit-buffer ()
  "Find the appropriate buffer for commit message."
  (or (get-buffer "COMMIT_EDITMSG")
      (and (derived-mode-p 'text-mode 'git-commit-mode) (current-buffer))
      (user-error "No commit message buffer found")))

(defvar gptel-commit--claude-json-buffer ""
  "Buffer to accumulate partial JSON from Claude Code streaming.")

(defun gptel-commit--claude-process-filter (_proc output)
  "Process filter for a Claude Code process, receiving OUTPUT."
  (gptel-commit--claude-debug-log "Claude raw output: %s" output)
  (when-let* ((buffer gptel-commit--current-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (gptel-commit--remove-loading)
        (save-excursion
          (goto-char gptel-commit--insert-position)
          ;; Parse stream-json format if streaming
          (if gptel-commit-stream
              (progn
                ;; Accumulate output in case JSON is split across chunks
                (setq gptel-commit--claude-json-buffer
                      (concat gptel-commit--claude-json-buffer output))
                (let ((lines (split-string gptel-commit--claude-json-buffer "\n" t)))
                  ;; Process complete JSON lines
                  (setq gptel-commit--claude-json-buffer "")
                  (dolist (line lines)
                    (when (and (string-prefix-p "{" line)
                               (string-suffix-p "}" line))
                      (condition-case err
                          (let* ((json-data (json-parse-string line :object-type 'plist))
                                 (type (plist-get json-data :type))
                                 (content (plist-get json-data :content))
                                 (result (plist-get json-data :result)))
                            ;; Handle different message types
                            (cond
                             ;; Assistant message with content (real streaming)
                             ((and (string= type "assistant") content)
                              (insert content)
                              (setq gptel-commit--insert-position (point)))
                             ;; Final result (fallback if no streaming content)
                             ((and (string= type "result") result)
                              (insert result)
                              (setq gptel-commit--insert-position (point)))))
                        (error (gptel-commit--claude-debug-log "JSON parse error: %s" err))))
                    ;; Save incomplete JSON line for next chunk
                    (when (and (string-prefix-p "{" line)
                               (not (string-suffix-p "}" line)))
                      (setq gptel-commit--claude-json-buffer line)))))
            (insert output)
            (setq gptel-commit--insert-position (point))))))))

(defun gptel-commit--claude-process-sentinel (_proc status)
  "Process sentinel for a Claude Code process, receiving STATUS."
  (gptel-commit--claude-debug-log "Claude process finished with status: %s" status)
  (setq gptel-commit--claude-process nil)
  (when (string-match-p "finished\\|exited" status)
    (run-hooks 'gptel-commit-after-insert-hook)))

(defun gptel-commit--generate-with-claude (prompt)
  "Generate commit message using Claude Code CLI with PROMPT."
  (gptel-commit--claude-debug-log "Using Claude Code backend")
  (let* ((buffer (gptel-commit--find-commit-buffer))
         (args (list "--print"
                     "--append-system-prompt" gptel-commit-prompt)))

    (when gptel-commit-stream
      (setq args (append args (list "--verbose" "--output-format" "stream-json"))))

    (setq args (append args (list prompt)))

    (let ((debug-args (list "--print")))
      (when gptel-commit-stream
        (setq debug-args (append debug-args (list "--verbose" "--output-format" "stream-json"))))
      (setq debug-args (append debug-args (list "--append-system-prompt" "[SYSTEM_PROMPT]" "[USER_PROMPT]")))
      (gptel-commit--claude-debug-log "Claude command: %s %s"
                                      gptel-commit-claude-command
                                      (mapconcat #'identity debug-args " ")))

    (setq gptel-commit--current-buffer buffer)
    (with-current-buffer buffer
      (setq gptel-commit--insert-position (point))
      (gptel-commit--insert-loading)

      (if gptel-commit-stream
          (progn
            (setq gptel-commit--claude-json-buffer "")  ; Reset JSON buffer
            (setq gptel-commit--claude-process
                  (make-process
                   :name "claude-commit"
                   :buffer nil
                   :command (cons gptel-commit-claude-command args)
                   :filter #'gptel-commit--claude-process-filter
                   :sentinel #'gptel-commit--claude-process-sentinel))
            (gptel-commit--claude-debug-log "Started streaming Claude process"))
        ;; Use make-process for non-streaming too for consistency
        (let ((output-accumulator ""))
          (setq gptel-commit--claude-process
                (make-process
                 :name "claude-commit-sync"
                 :buffer nil
                 :command (cons gptel-commit-claude-command args)
                 :filter (lambda (_proc output)
                           ;; Log raw output and accumulate
                           (gptel-commit--claude-debug-log "Claude raw output: %s" output)
                           (setq output-accumulator (concat output-accumulator output)))
                 :sentinel (lambda (_proc status)
                             (when (string-match-p "finished\\|exited" status)
                               ;; Remove trailing ANSI escape sequences ^[[?25h^[[?25h if present
                               (when (string-suffix-p "\x1b[?25h\x1b[?25h" output-accumulator)
                                 (setq output-accumulator (substring output-accumulator 0 -12)))
                               (gptel-commit--claude-debug-log "Claude non-streaming result: %s"
                                                               (if (> (length output-accumulator) 200)
                                                                   (concat (substring output-accumulator 0 200) "...")
                                                                 output-accumulator))
                               (gptel-commit--remove-loading)
                             (with-current-buffer buffer
                               (save-excursion
                                 (goto-char gptel-commit--insert-position)
                                 (insert output-accumulator)))
                             (run-hooks 'gptel-commit-after-insert-hook)))))
          (gptel-commit--claude-debug-log "Started non-streaming Claude process"))))))

(defun gptel-commit--setup-request-args ()
  "Setup request arguments based on gptel-commit configuration."
  (list :callback #'gptel-commit--handle-response))

(defun gptel-commit--handle-response (response info)
  "Handle the response from gptel.
RESPONSE is the generated commit message or chunk.
INFO is a plist with additional information."
  (when-let* ((buffer gptel-commit--current-buffer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (gptel-commit--remove-loading)
        (cond
         ;; A failed request: say why, and do not claim a message arrived.
         ((null response)
          (message "gptel-commit: request failed: %s" (plist-get info :status)))
         ((eq response 'abort)
          (message "gptel-commit: request aborted"))
         ;; Reasoning blocks and tool traffic are not commit message text.
         ((consp response))
         ((and gptel-commit-stream (stringp response))
          (save-excursion
            (goto-char gptel-commit--insert-position)
            (insert response)
            (setq gptel-commit--insert-position (point))))
         ((and (not gptel-commit-stream) (stringp response))
          (goto-char (point-min))
          (insert response))
         ((and gptel-commit-stream (not (stringp response)))
          (run-hooks 'gptel-commit-after-insert-hook))))))
  (when (and (not gptel-commit-stream) (stringp response))
    (run-hooks 'gptel-commit-after-insert-hook)))

(defmacro gptel-commit--with-model (&rest body)
  "Run BODY with the commit backend and model in force, and no tools."
  (declare (indent 0) (debug t))
  `(let ((gptel-backend (or gptel-commit-backend gptel-backend))
         (gptel-model (or gptel-commit-model gptel-model))
         (gptel-use-tools nil))
     ,@body))

(defun gptel-commit--prompt (rationale changes)
  "The user prompt for CHANGES, a diff, with RATIONALE if it is non-empty."
  (if (and rationale (not (string-empty-p rationale)))
      (format "Why this change was made: %s\n\nCode changes:\n%s" rationale changes)
    changes))

(defun gptel-commit--clean (message)
  "MESSAGE as a commit message: trimmed, and out of any code fence."
  (let ((message (string-trim message)))
    (if (string-match "\\````[^\n]*\n\\([^z-a]*?\\)\n```\\'" message)
        (string-trim (match-string 1 message))
      message)))

;;;###autoload
(cl-defun gptel-commit-generate (callback &key rationale directory)
  "Generate a commit message for the staged changes, without a commit buffer.
The changes are those staged in the repository at DIRECTORY, or at
`default-directory' if that is nil.  RATIONALE, a string, says why
they were made.  CALLBACK is called once, with the message as a
string, or with nil and a string saying what went wrong.

Always uses gptel, with `gptel-commit-backend' and `gptel-commit-model',
never Claude Code, and never streams.  Returns immediately."
  (let* ((default-directory (file-name-as-directory
                             (expand-file-name (or directory default-directory))))
         (changes (gptel-commit--filtered-diff)))
    (if (null changes)
        (funcall callback nil "nothing staged")
      (with-current-buffer (get-buffer-create " *gptel-commit-generate*")
        (setq default-directory (file-name-as-directory
                                 (expand-file-name (or directory default-directory))))
        (gptel-commit--with-model
          (gptel-request (gptel-commit--prompt rationale changes)
            :system gptel-commit-prompt
            :stream nil
            :callback
            (lambda (response info)
              (cond
               ((stringp response)
                (funcall callback (gptel-commit--clean response)))
               ((consp response))       ;reasoning or tool traffic
               (t (funcall callback nil
                           (if (eq response 'abort)
                               "aborted"
                             (format "%s" (plist-get info :status)))))))))))))

(defun gptel-commit--generate-message (rationale)
  "Generate a commit message based on staged changes and optional RATIONALE."
  (let* ((changes (or (gptel-commit--filtered-diff)
                      (user-error "No staged changes to describe")))
         (prompt (gptel-commit--prompt rationale changes)))

    (if gptel-commit-use-claude-code
        (progn
          (gptel-commit--claude-debug-log "Generating commit message with backend: claude-code")
          (gptel-commit--claude-debug-log "Prompt being sent: %s" (if (> (length prompt) 300)
                                                                      (concat (substring prompt 0 300) "...")
                                                                    prompt)))
      (gptel-commit--claude-debug-log "Generating commit message with backend: gptel"))

    (if gptel-commit-use-claude-code
        (gptel-commit--generate-with-claude prompt)
      (let ((buffer (gptel-commit--find-commit-buffer)))
        (setq gptel-commit--current-buffer buffer)
        (with-current-buffer buffer
          (setq gptel-commit--insert-position (point))
          (gptel-commit--insert-loading)
          (gptel-commit--with-model
            (gptel-request prompt
              :system gptel-commit-prompt
              :stream gptel-commit-stream
              :callback #'gptel-commit--handle-response)))))))

(defvar-keymap gptel-commit-rationale-mode-map
  :doc "Keymap for `gptel-commit-rationale-mode'."
  "C-c C-c" #'gptel-commit--submit-rationale
  "C-c C-k" #'gptel-commit--cancel-rationale)

(define-derived-mode gptel-commit-rationale-mode text-mode "GPTel-Commit-Rationale"
  "Mode for entering commit rationale before generating commit message.")

(defun gptel-commit--setup-rationale-buffer ()
  "Setup the rationale buffer with proper guidance."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert ";;; WHY are you making these changes? (optional)\n")
    (insert ";;; Press C-c C-c to generate commit message, C-c C-k to cancel\n")
    (insert ";;; Leave empty to generate without rationale\n")
    (insert ";;; ────────────────────────────────────────────────────────\n")
    (add-text-properties (point-min) (point)
                         '(face font-lock-comment-face read-only t))
    (insert "\n")
    (goto-char (point-max))))

(defun gptel-commit--submit-rationale ()
  "Submit the rationale buffer content and proceed with commit generation."
  (interactive)
  (let ((rationale (string-trim
                    (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (while (and (not (eobp))
                                   (get-text-property (point) 'read-only))
                         (forward-char))
                       (point))
                     (point-max)))))
    (quit-window t)
    (gptel-commit--generate-message rationale)))

(defun gptel-commit--cancel-rationale ()
  "Cancel rationale input and abort commit generation."
  (interactive)
  (quit-window t)
  (message "Commit generation canceled."))

;;;###autoload
(defun gptel-commit ()
  "Generate commit message with configured backend (GPTel or Claude Code)."
  (interactive)
  (gptel-commit--generate-message nil))

;;;###autoload
(defun gptel-commit-rationale ()
  "Prompt user for rationale and generate commit message with configured backend."
  (interactive)
  (when (or (get-buffer "COMMIT_EDITMSG")
            (derived-mode-p 'text-mode 'git-commit-mode))
    (setq gptel-commit--current-buffer (current-buffer)))
  (let ((buffer (get-buffer-create gptel-commit-rationale-buffer)))
    (with-current-buffer buffer
      (gptel-commit-rationale-mode)
      (gptel-commit--setup-rationale-buffer))
    (pop-to-buffer buffer)))

(provide 'gptel-commit)

;;; gptel-commit.el ends here
