;;; app/ai-agent/config.el -*- lexical-binding: t; -*-
;;;

(use-package! ai-agent
  :config
  (map!
   :map ai-agent-mode-map
   :localleader
   :desc "send messages" "c" #'ai-agent-tell
   :desc "interrupt generation" "g" #'ai-agent-interrupt
   :desc "insert from buffer" "b" #'ai-agent-insert-buffer-contents-with-line-numbers
   :desc "copy src block" "y" #'ai-agent-kill-src-block-at-point
   :desc "mark src block" "m" #'ai-agent-kill-select-block-at-point
   :desc "fix hunk" "h" #'ai-agent-fix-diff-hunk-at-point)
  (map!
   :leader
   (:prefix ("a" . "AI agent")
    :desc "new conversation" "n" #'ai-agent-focus-new-conversation
    :desc "model selection" "m" #'ai-agent-select-model
    (:prefix ("i" . "insert to agent")
     :desc "insert region" "r" #'ai-agent-insert-code-region
     :desc "insert buffer" "b" #'ai-agent-insert-code-buffer
     :desc "insert defun" "f" #'ai-agent-insert-code-defun
     :desc "insert statement" "s" #'ai-agent-insert-code-statement))))
