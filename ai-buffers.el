;;; ai-agent.el -- An interface to let AIs use Emacs
;;; Commentary:
;;;
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

1. Format your response in org-mode. Code blocks are in `#+begin_src ... #+end_src' blocks, include line numbers, and are tagged with relevant properties.
2. When the user asks you to edit a file, you should output a diff patch inside the relevant block. Use `:file path/to/file.txt' to indicate which file the diff is for. Paths are relative to the Git project root. Avoid outputting large chunks of the file that won't change, and output only the necessary changes. For example:
"
  "Default system prompt.

It should describe the environment and how the AI can interact with it."
  :type 'string
  :group 'ai-agent)

(defcustom ai-agent-user-name (car (split-string user-full-name " "))
  "Default name for the user."
  :type 'string
  :group 'openai)

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
      (unless ai-agent-conversation-marker
        (setq-local ai-agent-conversation-marker (point-max-marker)))
      (when (= (marker-position ai-agent-conversation-marker) 1)
        (insert (format "* System\n%s\n* User\n" system-prompt))
        (set-marker ai-agent-conversation-marker (point)))))
  buffer)

(defun ai-agent-toggle ()
  "Toggle the visibility of the sidebar."
  (interactive)
  (let ((buffer (get-buffer-create ai-agent-buffer-prefix)))
    (ai-agent-new-conversation buffer)
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (progn
        (split-window-right)
        (other-window 1)
        (switch-to-buffer buffer)
        (other-window -1)))))

;; Bind the toggle function to a key, e.g., F8
(global-set-key (kbd "<f8>") 'ai-agent-toggle)


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
  (message "Coming for buffer %s" buffer)
  (eval `(lambda (proc string)
           (dolist (line (split-string string "\n" t))
             (message "Processing line '%s'" line)

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
    (set-marker ai-agent-conversation-marker (point))
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
                ("model" . "gpt-4o-mini")))
             'utf-8))
           (response-buffer (url-retrieve url (lambda (_status) t) nil t t))
           (proc (get-buffer-process response-buffer)))

      (set-process-coding-system proc 'utf-8 'utf-8)
      (set-process-filter proc (ai-agent--append-streamed-conversation-filter (current-buffer)))
      (pop-to-buffer (current-buffer)))))

;; (let ((buf (get-buffer-create "*Function List*")))
;;   (with-current-buffer buf
;;     (erase-buffer)
;;     (mapatoms
;;      (lambda (sym)
;;        (when (fboundp sym)
;;          (condition-case nil  ; Add error handling
;;              (let* ((doc (documentation sym t))
;;                     ;; Get first line of doc, or "No documentation" if none
;;                     (doc-first-line
;;                      (if doc
;;                          (car (split-string doc "\n"))
;;                        "No documentation"))
;;                     ;; Truncate doc if too long
;;                     (doc-truncated
;;                      (if (> (length doc-first-line) 60)
;;                          (concat (substring doc-first-line 0 57) "...")
;;                        doc-first-line))
;;                     (type-str
;;                      (cond
;;                       ((special-form-p sym) "Special Form")
;;                       ((macrop sym) "Macro")
;;                       ((commandp sym) "Command")
;;                       ((subrp (symbol-function sym)) "Built-in")
;;                       (t "Function"))))
;;                (insert (format "%-30s %-12s %s\n"
;;                              (symbol-name sym)
;;                              (format "[%s]" type-str)
;;                              doc-truncated)))
;;            (error  ; If any error occurs, just show it's an autoloaded function
;;             (insert (format "%-30s %-12s %s\n"
;;                           (symbol-name sym)
;;                           "[Autoload]"
;;                           "Not yet loaded"))))))
;;      obarray)
;;     (sort-lines nil (point-min) (point-max)))
;;   (switch-to-buffer buf))


;; (defface user-face
;;   '((((background light)) :foreground "darkred")
;;     (((background dark)) :foreground "lightred"))
;;   "A custom face for my specific needs.")

;; (defun my-org-mode-custom-faces ()
;;   "Add custom faces to `org-mode'."
;;   (font-lock-add-keywords nil
;;                           '(("^\\*+ User.*" . 'user-face)
;;                             ("User>" . 'user-face))
;;                           'append))

(provide 'ai-buffers)
;;; ai-buffers.el ends here
