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

;; Define custom variables
(defcustom ai-agent-openai-url "https://api.openai.com/v1"
  "Base URL for an OpenAI-compatible API."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-openai-key (or (getenv "OPENAI_API_KEY") "")
  "Key for the OpenAI API."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-default-model "gpt-4o"
  "The model to use by default when asking questions."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-default-system-prompt "You are an AI agent which can interface with the world using Emacs. You are a master programmer. You are happy to help the user achieve what they want, answer any questions and write code.

1. Do not format your response in Markdown. In particular, do not include ```org ... ```
2. Format your response in org-mode instead. This includes *bold*, /italics/, _underline_, =verbatim=, * header1, ** header2, ...
3. Code blocks are in `#+begin_src ... #+end_src' blocks.
4. When the user asks you to edit a code block, you should output a diff patch inside the relevant block. Please do
   /not/ output the whole block again, because it will remain in the conversation history.
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

(define-minor-mode ai-agent-mode
  "Minor mode for AI Agent conversation buffers."
  :init-value nil
  :lighter " AI-Agent"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'ai-agent-tell)
            (define-key map (kbd "C-c g") #'ai-agent-interrupt)
            (define-key map (kbd "C-c i") #'ai-agent-insert-buffer-contents-with-line-numbers)
            map)
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

;;;###autoload
(defun ai-agent-focus-new-conversation ()
  "Toggle the visibility of the sidebar. Return the focused or created buffer."
  (interactive)
  (let ((buffer (generate-new-buffer ai-agent-buffer-prefix))
        (log-file (format "%s/%s.org" ai-agent-logging-directory (format-time-string "%04Y-%02m-%d--%H:%M:%S"))))
    (unless (file-exists-p ai-agent-logging-directory)
      (make-directory ai-agent-logging-directory t))
    (with-current-buffer buffer
      (set-visited-file-name log-file))
    (ai-agent-new-conversation buffer)

    ;; Now Open the buffer
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (progn
        (split-window-right)
        (other-window 1)
        (switch-to-buffer buffer)
        (other-window -1)))
    buffer))

;; Bind the toggle function to a key, e.g., F8
(global-set-key (kbd "<f8>") 'ai-agent-focus-new-conversation)

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
        (let ((start-end (funcall region-function)))
          (ai-agent-insert-code-region (car start-end) (cdr start-end) target-buffer))
      (error (message "AI agent error: major-mode %s not present in `ai-agent-defun-at-point-dispatch-alist'" major-mode)))))

;;;###autoload
(defun ai-agent-insert-statement-defun (&optional target-buffer)
  "Insert the current statement as code in the AI agent session at TARGET-BUFFER."
  (interactive)
  (let ((region-function (cdr (assoc major-mode ai-agent-statement-at-point-dispatch-alist))))
    (if region-function
        (let ((start-end (funcall region-function)))
          (ai-agent-insert-code-region (car start-end) (cdr start-end) target-buffer))
      (error (message "AI agent error: major-mode %s not present in `ai-agent-statement-at-point-dispatch-alist'" major-mode)))))

;;;###autoload
(defun ai-agent-insert-code-buffer (&optional target-buffer)
  "Insert the current buffer as code in the AI agent session at TARGET-BUFFER."
  (interactive)
  (ai-agent-insert-code-region (point-min) (point-max) target-buffer))

;; Bind the toggle function to a key, e.g., F9
(global-set-key (kbd "<f9>") 'ai-agent-insert-code-region)


(defun ai-agent-insert-buffer-contents-with-line-numbers (buffer &optional ai-agent-buffer)
  "Insert selected buffer contents with line numbers into current buffer in `ai-agent-mode' and fold it."
  (interactive "bSelect buffer: ")
  (let* ((ai-agent-buffer (or ai-agent-buffer (current-buffer)))
         (drawer-marker (with-current-buffer ai-agent-buffer (point-max-marker))))

    (with-current-buffer buffer
      (ai-agent-code-prompt-from-region (point-min) (point-max) ai-agent-buffer))

    (with-current-buffer ai-agent-buffer
      (save-excursion
        (goto-char drawer-marker)
        (org-cycle)))))


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


(defun ai-agent--append-streamed-conversation-filter (buffer)
  "Filter to continue the conversation in BUFFER."
  (eval `(lambda (proc string)
           (dolist (line (split-string string "\n" t))

             ;; Skip HTTP headers (before the first blank line) and [DONE] messages
             (when (and (string-prefix-p "data: " line)
                        (not (string= line "data: [DONE]")))
               ;; Parse the JSON data
               (let* ((json-data (json-read-from-string (substring line 6)))
                      (content (ai-agent--get-json-items json-data 'choices 0 'delta 'content)))
                 ;; Insert the content if it exists
                 (when content
                   (with-current-buffer ,buffer
                     (ai-agent--insert-at-marker-in-current-buffer ai-agent-conversation-marker content))))))

           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (ai-agent--insert-at-marker-in-current-buffer (process-mark proc) string))))))


(defun ai-agent-code-block-kill ()
  "Put in `kill-ring' the code from the current #+begin_src ... #+end_src block."
  (interactive)
  (save-excursion
    (let* ((block (org-element-at-point))
           (start (progn (goto-char (org-element-begin block)) (forward-line) (point)))
           (end (progn (goto-char (org-element-end block)) (forward-line -2) (point))))
      (kill-new (buffer-substring start end))
      (pulse-momentary-highlight-region start end))))


(defun ai-agent-tell (&optional buffer)
  "Send to the AI agent the contents of BUFFER (defaults current buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (bound-and-true-p ai-agent-mode)
      (signal 'ai-agent-no-mode (format "Buffer %s not in `ai-agent-mode'." buffer)))

    ;; Set marker to the end of the message
    (goto-char (point-max))
    (unless (looking-back "\n" 1)      ; Check if the last character is a newline
      (insert "\n"))                   ; Insert a newline if the last character is not a newline
    (insert "* Assistant\n")
    (if ai-agent-conversation-marker
        (set-marker ai-agent-conversation-marker (point))
      (setq-local ai-agent-conversation-marker (point-marker)))
    (insert "\n* User\n") ; User always gets a newline to avoid being in the same line as point.

    ;; Collect top-level headers and their contents
    (let* ((messages
            (apply 'vector
                   (org-map-entries
                    (lambda ()
                      (let* ((header (nth 4 (org-heading-components)))
                             (prefix (cond
                                      ((string-prefix-p "User" header) "user")
                                      ((string-prefix-p "Assistant" header) "assistant")
                                      ((string-prefix-p "System" header) "system")))
                             (content (encode-coding-string (org-get-entry) 'utf-8 t)))
                        `(("role" . ,prefix) ("content" . ,content))))
                    "LEVEL=1" nil)))
           (url (concat ai-agent-openai-url "/chat/completions"))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json; charset=utf-8")
              ("Authorization" . ,(format "Bearer %s" ai-agent-openai-key))))
           (url-request-data
            (encode-coding-string
             (json-encode
              `(("messages" . ,messages)
                ("stream". t)
                ("model" . ,ai-agent-default-model)))
             'utf-8 t))
           (response-buffer (url-retrieve url
                                          (lambda (status)
                                            (when-let ((error (plist-get status :error)))
                                              (signal 'error (cdr error)))
                                            nil t t)))
           (proc (get-buffer-process response-buffer)))
      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-filter proc (ai-agent--append-streamed-conversation-filter (current-buffer)))
      (pop-to-buffer (current-buffer)))))


(provide 'ai-agent)
;;; ai-agent.el ends here
