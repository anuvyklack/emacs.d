;;; config.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)

;; ;;;; Scrolling
;; ;;;;; Do not jump half the page when point goes out of the screen.
;;
;; (setopt scroll-conservatively 101)
;;
;; ;; Restore original value for some commands.
;; ;; Mainly for functions that perform text replacement, to center the screen
;; ;; on jumping to the next occurrence.
;; (dolist (cmd '(dired-do-find-regexp-and-replace
;;                projectile-replace
;;                projectile-replace-regexp))
;;   (advice-add cmd :around #'with-original-scroll-conservatively-value-a))
;;
;; (defun with-original-scroll-conservatively-value-a (fun &rest args)
;;   "Meant to be used as `:around' advice."
;;   (let ((scroll-conservatively (my-original-value 'scroll-conservatively)))
;;     (apply fun args)))

;; ;;; Minibuffer & Completion
;;
;; (use-package vertico
;;   :config
;;   (helix-keymap-set vertico-map :state '(normal insert)
;;     ;; "M-<return>" 'vertico-exit-input ;; default setting
;;     "C-p" #'consult-yank-from-kill-ring
;;     ;; Russian
;;     "C-о" 'vertico-next
;;     "C-л" 'vertico-previous))

;;; config.el ends here
