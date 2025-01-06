;;; ai-buffers.el --- Create and manage buffers for the AI agent -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Adrià Garriga-Alonso
;;
;; Author: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Maintainer: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Created: January 06, 2025
;; Modified: January 06, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rhaps0dy/ai-buffers
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Create and manage buffers for the AI agent
;;
;;; Code:

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

(defvar ai-agent-buffer-prefix "*AI agent*"
  "Prefix for all AI agent conversation buffers.")

(define-minor-mode ai-agent-mode
  "Minor mode for AI Agent conversation buffers."
  :init-value nil
  :lighter " AI-Agent"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c a") #'ai-agent-tell)
            (define-key map (kbd "C-c g") #'ai-agent-interrupt)
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

(defun ai-agent-focus-new-conversation ()
  "Toggle the visibility of the sidebar."
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
        (other-window -1)))))

;; Bind the toggle function to a key, e.g., F8
(global-set-key (kbd "<f8>") 'ai-agent-focus-new-conversation)

(defun ai-agent-create-code-prompt (start end)
  "Create an AI prompt for the selected code region between START and END.
If there are no AI-agent mode buffers visible, it creates a new one."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (line-start (line-number-at-pos start))
         (line-numbered-code (with-temp-buffer
                               (insert code)
                               (goto-char (point-min))
                               (let ((line-number line-start))
                                 (while (not (eobp))
                                   (insert (format "%d| " line-number))
                                   (setq line-number (1+ line-number))
                                   (forward-line 1)))
                               (buffer-string)))
         (target-window (or (cl-find-if (lambda (window)
                                          (with-current-buffer (window-buffer window)
                                            (bound-and-true-p ai-agent-mode)))
                                        (window-list))
                            (ai-agent-focus-new-conversation)))
         (file-name (file-relative-name
                     buffer-file-name
                     (when (fboundp 'projectile-root) (projectile-root)))))

    (with-current-buffer (window-buffer target-window)
      (goto-char (point-max))
      (insert (format "#+begin_src emacs-lisp :file %s\n%s#+end_src\n" file-name line-numbered-code)))))


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
                             (content (org-get-entry)))
                        `(("role" . ,prefix) ("content" . ,content))))
                    "LEVEL=1" nil)))
           (url (concat ai-agent-openai-url "/chat/completions"))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,(format "Bearer %s" ai-agent-openai-key))))
           (url-request-data
            (encode-coding-string
             (json-encode
              `(("messages" . ,messages)
                ("stream". t)
                ("model" . ,ai-agent-default-model)))
             'utf-8))
           (response-buffer (url-retrieve url
                                          (lambda (_status)
                                            t)
                                          nil t t))
           (proc (get-buffer-process response-buffer)))

      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-filter proc (ai-agent--append-streamed-conversation-filter (current-buffer)))
      (pop-to-buffer (current-buffer)))))


(provide 'ai-buffers)
;;; ai-buffers.el ends here
