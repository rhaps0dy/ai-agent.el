;;; ai-agent.el --- Functions for the AI Agent and Emacs to interact -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Adrià Garriga-Alonso
;;
;; Author: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Maintainer: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Created: January 06, 2025
;; Modified: January 06, 2025
;; Version: 0.0.1
;; Keywords: comm extensions
;; Homepage: https://github.com/rhaps0dy/ai-agent
;; Package-Requires: ((emacs "25.1") (python "0.28") (org "9.7"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Functions for the AI Agent and Emacs to interact
;;
;;; Code:
(require 'python)
(require 'org)
(require 'org-element)
(require 'diff-mode)

(defcustom ai-agent-default-model "gpt-4o"
  "The model to use by default when asking questions."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-model-provider-alist
  `((openai
     (url . "https://api.openai.com/v1")
     (key . ,(or (getenv "OPENAI_API_KEY") ""))
     (models . ((gpt-4o . "gpt-4o") (o1 . "o1")))
     (api-style . openai))
    (anthropic
     (url . "https://api.anthropic.com/v1")
     (key . ,(or (getenv "ANTHROPIC_API_KEY") ""))
     (models . ((claude-3-5-sonnet . "claude-3-5-sonnet-20241022")))
     (api-style . anthropic)))
  "Alist of model providers with their configurations."
  :type 'alist
  :group 'ai-agent)

(defvar ai-agent--current-model 'gpt-4o
  "The current AI model being used.")

(defvar ai-agent--current-provider 'openai
  "The current provider of the AI model being used.")

(defun ai-agent-select-model ()
  "Interactively select an AI model and set the current provider and model.
Errors if duplicate model symbols are found."
  (interactive)
  ;; Build a list of all model symbols, ensuring no duplicates
  (let ((model-choices '())
        (model-symbol-map (make-hash-table :test 'equal)))
    ;; Iterate over providers
    (dolist (provider ai-agent-model-provider-alist)
      (let ((provider-name (car provider))
            (models (cdr (assoc 'models (cdr provider)))))
        ;; Iterate over models for each provider
        (dolist (model models)
          (let ((model-symbol (car model)))
            (if (gethash model-symbol model-symbol-map)
                (error "Duplicate model symbol found: %s" model-symbol)
              (push model-symbol model-choices)
              (puthash model-symbol provider-name model-symbol-map))))))
    ;; Ask user to choose a model
    (let* ((choice (completing-read "Select AI model: " model-choices))
           (chosen-symbol (intern choice)))
      ;; Set the current model and provider based on choice
      (setq ai-agent--current-model chosen-symbol)
      (setq ai-agent--current-provider (gethash chosen-symbol model-symbol-map))
      (message "Selected model: %s, provider: %s"
               ai-agent--current-model ai-agent--current-provider))))

; we avoid putting #+end_src at the beginning of line in this file so it gets escaped.
(defcustom ai-agent-default-system-prompt "You are an AI agent which can interface with the world using Emacs. You are a master programmer. You are happy to help the user achieve what they want, answer any questions and write code.

1. Do not format your response in Markdown. You must not use ```python ... ``` or ```emacs-lisp ... ``` for code blocks. Use #+begin_src ... #+end_src for code blocks instead.
2. Format your response in org-mode instead. This includes *bold*, /italics/, _underline_, =verbatim=, * header1, ** header2, ...
3. To edit a code block, you can either write out the new version, or use the diff editor. For example, given an existing =hello.py= block that looks like this:
#+begin_src python :file hello.py
print(\"hello\")
for i in range(10):
    print(i)\n#+end_src

You can write out the new version directly:

#+begin_src python :file hello.py
print(\"hello\")
for j in range(20):
    print(j)\n#+end_src

You can also write a Git-style diff, with OLD_CODE first and NEW_CODE afterwards.  The OLD_CODE must be reproduced exactly, including whitespace, and must have sufficient context to be unique in the file. For example:
#+begin_src python
print(\"hello\")
<<<<<<<
for i in range(10):
    print(i)
=======
for j in range(20):
    print(j)
>>>>>>>\n#+end_src

You can also include several Git-style diffs in a single block. For example:
#+begin_src python
def impala_loss(
    params: Any,
    get_logits_and_value: GetLogitsAndValueFn,
    args: ImpalaLossConfig,
<<<<<<<
    batch: Rollout,
=======
    minibatch: Rollout,
>>>>>>>
) -> tuple[jax.Array, dict[str, jax.Array]]:
<<<<<<<
    done_t = batch.episode_starts_t[1:]
=======
    done_t = minibatch.episode_starts_t[1:]
>>>>>>>
    discount_t = (~done_t) * args.gamma\n#+end_src
"
  "Default system prompt.

It should describe the environment and how the AI can interact with it."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-user-name (car (split-string user-full-name " "))
  "Default name for the user."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-logging-directory (expand-file-name "~/ai-logs/")
  "Directory where conversation logs will be stored."
  :type 'directory
  :group 'ai-agent)

(defcustom ai-agent-defun-at-point-dispatch-alist
  '((python-mode . ai-agent-python--defun-at-point-region )
    (emacs-lisp-mode . ai-agent-emacs-lisp--defun-at-point-region ))
  "Alist mapping major modes to their respective defun-region functions."
  :type 'alist
  :group 'ai-agent)

(defcustom ai-agent-statement-at-point-dispatch-alist
  '((python-mode . ai-agent-python--statement-at-point-region )
    (emacs-lisp-mode . ai-agent-emacs-lisp--sexp-at-point-region ))
  "Alist mapping major modes to their respective statement-region functions."
  :type 'alist
  :group 'ai-agent)

(defun ai-agent-python--defun-at-point-region ()
  "Return the region for the Python function at point as a cons cell (START . END)."
  (interactive)
  (save-excursion
    (python-nav-beginning-of-defun)
    (let ((start (point)))
      (python-nav-end-of-defun)
      (cons start (point)))))

(defun ai-agent-emacs-lisp--defun-at-point-region ()
  "Return the region for the Emacs Lisp function at point as a cons cell (START . END)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (cons start (point)))))

(defun ai-agent-python--statement-at-point-region ()
  "Return the region for the Python statement at point as a cons cell (START . END)."
  (interactive)
  (save-excursion
    (python-nav-beginning-of-statement)
    (let ((start (point)))
      (python-nav-end-of-statement)
      (cons start (point)))))

(defun ai-agent-emacs-lisp--sexp-at-point-region ()
  "Return the region for the Emacs Lisp s-expression at point as a cons cell (START . END)."
  (interactive)
  (save-excursion
    (backward-sexp)
    (let ((start (point)))
      (forward-sexp)
      (cons start (point)))))


(defvar ai-agent-buffer-prefix "*AI agent*"
  "Prefix for all AI agent conversation buffers.")

(defvar ai-agent-mode-map (make-sparse-keymap)
  "Minor mode map for ai-agent.el.")

(define-minor-mode ai-agent-mode
  "Minor mode for AI Agent conversation buffers."
  :init-value nil
  :lighter " AI-Agent"
  (if ai-agent-mode
      (progn
        (setq truncate-lines nil)
        (message "ai-agent-mode enabled in %s" (buffer-name)))
    (message "ai-agent-mode disabled in %s" (buffer-name))))

(defvar-local ai-agent-conversation-marker nil
  "Where to append conversation at the end of the conversation buffer.")

(defun ai-agent-new-conversation (buffer &optional system-prompt)
  "If BUFFER is empty, start a new AI conversation with SYSTEM-PROMPT.

SYSTEM-PROMPT defaults to `ai-agent-default-system-prompt'."
  (setq system-prompt (or system-prompt ai-agent-default-system-prompt))
  (save-excursion
    (with-current-buffer buffer
      (org-mode)
      (ai-agent-mode 1)  ; activate mode if not activated yet
      (when (= (buffer-size) 0)
        (insert (format "* System\n%s\n* User\n" system-prompt))
        (setq-local ai-agent-conversation-marker (point-max-marker)))))
  buffer)

(defun ai-agent-rename-conversation-buffer (buffer new-name)
  "Rename BUFFER to NEW-NAME."
  (let* ((datetime (format-time-string "%04Y-%02m-%d %H:%M:%S"))
         (new-name (if new-name (format "%s %s" datetime new-name) datetime))
         (log-file (format "%s/%s.org" ai-agent-logging-directory new-name)))
    (make-directory ai-agent-logging-directory t)
    (with-current-buffer buffer
      (set-visited-file-name log-file nil t))))

;;;###autoload
(defun ai-agent-focus-new-conversation ()
  "Toggle the visibility of the sidebar. Return the focused or created buffer."
  (interactive)
  (let ((buffer (ai-agent-new-conversation (generate-new-buffer "*AI conversation*"))))
    ;; Now Open the buffer
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (progn
        (split-window-right)
        (other-window 1)
        (switch-to-buffer buffer)
        (other-window -1)))
    buffer))

(defun ai-agent-choose-target-buffer ()
  (let ((open-window (cl-find-if
                      (lambda (window)
                        (with-current-buffer (window-buffer window)
                          (bound-and-true-p ai-agent-mode)))
                      (window-list))))
    (if open-window (window-buffer open-window) (ai-agent-focus-new-conversation))))

(defun ai-agent--ensure-newline ()
  "If point is not in a fresh line, insert a line after the point."
  (unless (= (line-beginning-position) (point))
    (goto-char (line-end-position))
    (insert ?\n)))

(defun ai-agent-insert-code-region (start end &optional target-buffer)
  "Add the selected code region (START to END) as an Org code block.

If specified, add the region to the TARGET-BUFFER. If TARGET-BUFFER is nil, and
there is an AI-agent mode buffer visible, use that one; otherwise create a new
one."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (target-buffer (or target-buffer (ai-agent-choose-target-buffer)))
         (file-name (when buffer-file-name
                      (file-relative-name
                       buffer-file-name
                       (when (fboundp 'projectile-root) (projectile-root)))))
         (buffer-name (buffer-name (current-buffer)))
         (major-mode (buffer-local-value 'major-mode (current-buffer)))
         (code-language (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))))

    (with-current-buffer target-buffer
      (ai-agent--ensure-newline)
      (let ((drawer-marker (point-marker)))
        (insert (format "#+begin_src %s %s\n" code-language
                        (if file-name
                            (format ":file %s" file-name)
                          (format ":buffer %s" buffer-name))))
        (insert code)
        (ai-agent--ensure-newline)
        (insert "#+end_src\n")
        (save-excursion
          (goto-char drawer-marker)
          ;; Close the newly-inserted drawer
          (org-cycle))))
    (pulse-momentary-highlight-region start end)))

;;;###autoload
(defun ai-agent-insert-code-defun (&optional target-buffer)
  "Insert the current function as code in the AI agent session at TARGET-BUFFER."
  (interactive)
  (let ((region-function (cdr (assoc major-mode ai-agent-defun-at-point-dispatch-alist))))
    (if region-function
        (pcase-let ((`(,start ,end) (funcall region-function)))
          (ai-agent-insert-code-region start end target-buffer))
      (error (message "AI agent error: major-mode %s not present in `ai-agent-defun-at-point-dispatch-alist'" major-mode)))))

;;;###autoload
(defun ai-agent-insert-code-statement (&optional target-buffer)
  "Insert the current statement as code in the AI agent session at TARGET-BUFFER."
  (interactive)
  (let ((region-function (cdr (assoc major-mode ai-agent-statement-at-point-dispatch-alist))))
    (if region-function
        (pcase-let ((`(,start ,end) (funcall region-function)))
          (ai-agent-insert-code-region start end target-buffer))
      (error (message "AI agent error: major-mode %s not present in `ai-agent-statement-at-point-dispatch-alist'" major-mode)))))

;;;###autoload
(defun ai-agent-insert-code-buffer (&optional target-buffer)
  "Insert the current buffer as code in the AI agent session at TARGET-BUFFER."
  (interactive)
  (ai-agent-insert-code-region (point-min) (point-max) target-buffer))


(defun ai-agent-insert-buffer-contents-with-line-numbers (src-buffer &optional ai-agent-buffer)
  "Insert SRC-BUFFER contents as code into AI-AGENT-BUFFER and fold it.

If nil, insert to the last buffer in `ai-agent-mode' instead."
  (interactive "bSelect buffer: ")
  (let* ((ai-agent-buffer (or ai-agent-buffer (current-buffer)))
         (drawer-char (with-current-buffer ai-agent-buffer (point-max))))

    (with-current-buffer src-buffer
      (ai-agent-insert-code-buffer ai-agent-buffer))

    (with-current-buffer ai-agent-buffer
      (save-excursion
        (goto-char drawer-char)
        (org-cycle)))))

(defun ai-agent-kill-src-block-at-point ()
  "Copy the #+begin_src ... #+end_src block at current point."
  (interactive)
  (let* ((elem (org-element-at-point))
         (beg (save-excursion
                (goto-char (org-element-begin elem))
                (forward-line 1)
                (point)))
         (end (save-excursion
                (goto-char (org-element-end elem))
                (forward-line -1)
                (point))))
  (copy-region-as-kill beg end)
  (pulse-momentary-highlight-region beg end)))

(defun ai-agent-kill-select-block-at-point ()
  "Copy the #+begin_src ... #+end_src block at current point."
  (interactive)
  (let* ((elem (org-element-at-point)))
    (goto-char (org-element-end elem))
    (forward-line -1)
    (set-mark (point))
    (goto-char (org-element-begin elem))
    (forward-line 1)))


(defun ai-agent-interrupt ()
  "Stop the LLM process associated with the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (progn
          (delete-process proc)
          (message "LLM process stopped."))
      (message "No active LLM process to stop."))))


(defun ai-agent--insert-at-marker-in-current-buffer (marker content)
  "Insert CONTENT at MARKER in current buffer, moving point if necessary."
  (save-excursion
    (goto-char marker)
    (insert content)
    (set-marker marker (point))))

(defun ai-agent--get-json-items (obj &rest keys)
  "Gets keys or list indices KEYS from OBJ in sequence."
  (let ((output obj))
    (dolist (key keys)
      (setq output (if (numberp key)
                       (elt output key)
                     (cdr (assoc key output)))))
    output))


(defun ai-agent--append-streamed-conversation-filter (conv-buf)
  "Return a filter for to append streamed data to CONV-BUF, updating idle times."
  (lambda (proc string)
    ;; TODO: deal with data: {...} messages that split unicode characters in half.
    (dolist (line (split-string string "\n" t))
      (when (string-prefix-p "data: " line)
        (if (string-prefix-p "data: [DONE]" line)
            (progn
              ;; Close the process and kill the buffer
              (when (process-live-p proc)
                (delete-process proc))
              (when (buffer-live-p (process-buffer proc))
                (kill-buffer (process-buffer proc))))
          (let* ((json-data (json-read-from-string (substring line 6)))
                 (content (ai-agent--get-json-items json-data 'choices 0 'delta 'content)))
            ;; Insert the content if it exists
            (when content
              (with-current-buffer conv-buf
                (ai-agent--insert-at-marker-in-current-buffer ai-agent-conversation-marker
                                                              (decode-coding-string content 'utf-8))))))))
    ;; We also store the raw chunk in the response buffer itself
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (ai-agent--insert-at-marker-in-current-buffer (process-mark proc) string)))))

(defun ai-agent-collect-messages ()
  "Collect top-level headers and their contents to form messages."
  (apply #'vector
         (org-map-entries
          (lambda ()
            (let* ((header (nth 4 (org-heading-components)))
                   (prefix (cond
                            ((string-prefix-p "User" header) "user")
                            ((string-prefix-p "Assistant" header) "assistant")
                            ((string-prefix-p "System" header) "system")))
                   (content (org-get-entry)))
              `(("role" . ,prefix) ("content" . ,content))))
          "LEVEL=1" nil)))

(defun ai-agent-tell (&optional buffer)
  "Send to the AI agent the contents of BUFFER (defaults current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (bound-and-true-p ai-agent-mode)
      (signal 'ai-agent-no-mode (format "Buffer %s not in `ai-agent-mode'." buffer)))

    ;; Ensure last line is newline, then insert headings
    (goto-char (point-max))
    (unless (looking-back "\n" 1) (insert "\n"))
    (insert "* Assistant\n")
    (setq-local ai-agent-conversation-marker
                (or ai-agent-conversation-marker (point-marker)))
    (set-marker ai-agent-conversation-marker (point))
    ;; User always gets a newline to avoid being on the same line as the marker.
    ;; This way the user cursor will advance.
    (insert "\n* User\n")

    ;; Get provider details from the alist
    (let* ((provider-info (cdr (assoc ai-agent--current-provider ai-agent-model-provider-alist)))
           (models (assoc 'models provider-info))
           (model-name (cdr (assoc ai-agent--current-model models)))
           (url (concat (cdr (assoc 'url provider-info)) "/chat/completions"))
           (api-key (cdr (assoc 'key provider-info)))
           (api-style (cdr (assoc 'api-style provider-info)))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json; charset=utf-8")
              . ,(cond ((eq api-style 'openai)
                        `(("Authorization" . ,(format "Bearer %s" (encode-coding-string api-key 'ascii)))))
                       ((eq api-style 'anthropic)
                        `(("x-api-key" . (encode-coding-string api-key 'ascii))
                          ("anthropic-version" . "2023-06-01")))
                       (t (error "AI agent: unknown api-style `%s'" api-style)))))
           (messages (ai-agent-collect-messages))
           (url-request-data
            (encode-coding-string
             (json-encode
              `(("messages" . ,messages)
                ("stream" . t)
                ("model" . ,model-name)))
             'utf-8 t))
           (response-buffer (url-retrieve url
                                          (lambda (status)
                                            (when-let ((error (plist-get status :error)))
                                              (signal 'error (cdr error)))
                                            nil t t)))
           (proc (get-buffer-process response-buffer))
           (conv-buf (current-buffer)))
      (set-process-filter proc (ai-agent--append-streamed-conversation-filter conv-buf))

      ;; After the first message, rename conversation
      (when (or (string-prefix-p "*AI conversation*" (buffer-name))
                (< (length messages) 3))
        (ai-agent-rename-buffer-from-summary conv-buf messages))

      (pop-to-buffer (current-buffer)))))

(defun ai-agent-rename-buffer-from-summary (conv-buf messages)
  "Rename the CONV-BUF conversation based on the content of MESSAGES."
  (let* ((conversation
          (mapconcat
           (lambda (message)
             (let ((role (alist-get "role" message "" nil #'equal))
                   (content (alist-get "content" message "" nil #'equal)))
               (format "<%s>\n%s</%s>\n" role content role)))
           messages)))
    (ai-agent-chat
     "gpt-4o"
     `[(("role" . "system") ("content" . "Please describe the following conversation, in 1-5 words. If the conversation is about a request, please prioritize that in the summary. Otherwise just summarize whatever you think is most relevant."))
       (("role" . "user") ("content" . ,(format "The conversation: %s" conversation)))]
     (lambda (new-name)
       (ai-agent-rename-conversation-buffer
        conv-buf
        (replace-regexp-in-string "[.,/]" "" new-name))))))

(defun ai-agent-chat (model messages callback)
  "Make a request to the AI agent using MODEL and MESSAGES.
The CALLBACK is called with the buffer returned by `url-retrieve`, which is then automatically deleted."
  (let* ((url (concat ai-agent-openai-url "/chat/completions"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(format "Bearer %s" (encode-coding-string ai-agent-openai-key 'ascii)))))
         (url-request-data
          (encode-coding-string
           (json-encode
            `(("messages" . ,messages)
              ("model" . ,model)))
           'utf-8 t)))
    (url-retrieve
     url
     (lambda (status)
       (unwind-protect
           (if (plist-get status :error)
               (error "Error retrieving URL: %s"
                      (plist-get status :error))
             (goto-char (point-min))
             (re-search-forward "\n\n")
             (let* ((json-data (json-read-from-string
                                (buffer-substring-no-properties (point) (point-max))))
                    (content (ai-agent--get-json-items json-data 'choices 0 'message 'content)))
               (funcall callback (decode-coding-string content 'utf-8))))
         (kill-buffer (current-buffer)))))))

(defun ai-agent-count-lines-beginning-with-char (char start end)
  "Count lines beginning with CHAR between START and END."
  (interactive "cCharacter:\nr")
  (count-matches (format "^%c" char) start end))

(defun ai-agent-fix-diff-hunk-at-point ()
  "Process a diff hunk to gather information and prepare for application."
  (interactive)
  (let* ((start (diff-beginning-of-hunk t))
         (end (save-excursion
                (goto-char start)
                (forward-line 1)
                (re-search-forward "^\\(@@ \\|--- \\|#\\+end_src\\)" (point-max))))
         (target-line
          (save-excursion
            (goto-char start)
            (when (re-search-forward "^@@ -\\([0-9]+\\)[0-9,]* \\+[0-9,]* @@$" end t)
              (match-string 1))))
         (total-line-count (count-matches "^.*" start end))
         (add-line-count (count-matches "^\\+.*" start end))
         (del-line-count (count-matches "^\\-.*" start end))
         (before-line-count (- (- total-line-count add-line-count) 2))
         (after-line-count (- (- total-line-count del-line-count) 2))
         (new-hunk-info (format "@@ -%s,%s +%s,%s @@" target-line before-line-count target-line after-line-count)))
    (save-excursion
      (goto-char start)
      (delete-region start (line-end-position))
      (insert new-hunk-info))))


(provide 'ai-agent)
;;; ai-agent.el ends here
