;;; app/ai-agent/config.el -*- lexical-binding: t; -*-
;;;
(use-package! ai-agent
  :config
  (map!
   :map ai-agent-mode-map
   :localleader
   :desc "send messages" "c" #'ai-agent-tell
   :desc "interrupt generation" "g" #'ai-agent-interrupt
   :desc "insert from buffer" "b" #'ai-agent-insert-buffer-contents-with-line-numbers)
  (map!
   :leader
   (:prefix ("a" . "AI agent")
    :desc "new conversation" "an" #'ai-agent-focus-new-conversation
    (:prefix ("s" . "insert to agent")
     :desc "insert region" "r" #'ai-agent-insert-code-region
     :desc "insert buffer" "b" #'ai-agent-insert-code-buffer
     :desc "insert defun" "f" #'ai-agent-insert-code-defun
     :desc "insert statement" "s" #'ai-agent-insert-code-statement))))