(defvar llm-buffer-name "*model out*"
  "Name of the sidebar buffer.")

(defun llm-toggle ()
  "Toggle the visibility of the sidebar."
  (interactive)
  (let ((buffer (get-buffer-create llm-buffer-name)))
    (if (get-buffer-window buffer)
        (delete-window (get-buffer-window buffer))
      (progn
        (split-window-right)
        (other-window 1)
        (switch-to-buffer buffer)
        (other-window -1)))))

;; Bind the toggle function to a key, e.g., F8
(global-set-key (kbd "<f8>") 'llm-toggle)



(defun stop-llm-process ()
  "Stop the LLM process associated with the current buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (process-live-p proc))
        (progn
          (delete-process proc)
          (message "LLM process stopped."))
      (message "No active LLM process to stop."))))

(progn
  (defun llm-filter (proc string)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) (process-mark proc)))
              (clean-buf (get-buffer-create "*model out*")))

          (with-current-buffer clean-buf
            (setq truncate-lines nil)
            ;; Process each line in the incoming string
            (dolist (line (split-string string "\n" t))

              ;; Skip HTTP headers (before the first blank line) and [DONE] messages
              (when (and (string-prefix-p "data: " line)
                         (not (string= line "data: [DONE]")))
                ;; Parse the JSON data
                (let* ((json-data (json-read-from-string (substring line 6)))
                       (content (cdr (assoc 'content
                                            (cdr (assoc 'delta
                                                        (aref (cdr (assoc 'choices json-data)) 0)))))))
                  ;; Insert the content if it exists
                  (when content
                    (save-excursion
                      (goto-char (point-max))
                      (insert content)))))))

          (save-excursion
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc)))))))


  (let* ((url "https://api.openai.com/v1/chat/completions")
         (api-key (getenv "OPENAI_API_KEY"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" api-key))))
         (url-request-data
          (encode-coding-string
           (json-encode
            '(("messages" . [(("role" . "system")
                              ("content" . "You are a test assistant."))
                             (("role" . "user")
                              ("content" . "Testing functionality. Please write a short story."))])
              ("stream". t)
              ("model" . "gpt-4o-mini")))
           'utf-8))
         (response-buffer (url-retrieve url (lambda (_status) t) nil t t))
         (proc (get-buffer-process response-buffer)))
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'llm-filter)
    (pop-to-buffer response-buffer)))

(let ((buf (get-buffer-create "*Function List*")))
  (with-current-buffer buf
    (erase-buffer)
    (mapatoms
     (lambda (sym)
       (when (fboundp sym)
         (condition-case nil  ; Add error handling
             (let* ((doc (documentation sym t))
                    ;; Get first line of doc, or "No documentation" if none
                    (doc-first-line
                     (if doc
                         (car (split-string doc "\n"))
                       "No documentation"))
                    ;; Truncate doc if too long
                    (doc-truncated
                     (if (> (length doc-first-line) 60)
                         (concat (substring doc-first-line 0 57) "...")
                       doc-first-line))
                    (type-str
                     (cond
                      ((special-form-p sym) "Special Form")
                      ((macrop sym) "Macro")
                      ((commandp sym) "Command")
                      ((subrp (symbol-function sym)) "Built-in")
                      (t "Function"))))
               (insert (format "%-30s %-12s %s\n"
                             (symbol-name sym)
                             (format "[%s]" type-str)
                             doc-truncated)))
           (error  ; If any error occurs, just show it's an autoloaded function
            (insert (format "%-30s %-12s %s\n"
                          (symbol-name sym)
                          "[Autoload]"
                          "Not yet loaded"))))))
     obarray)
    (sort-lines nil (point-min) (point-max)))
  (switch-to-buffer buf))
