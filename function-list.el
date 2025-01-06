;;; function-list.el --- Make a list of all Emacs functions and their help -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Adrià Garriga-Alonso
;;
;; Author: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Maintainer: Adrià Garriga-Alonso <adria.garriga@gmail.com>
;; Created: January 06, 2025
;; Modified: January 06, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/rhaps0dy/function-list
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Make a list of all Emacs functions and their help
;;
;;; Code:


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


(provide 'function-list)
;;; function-list.el ends here
