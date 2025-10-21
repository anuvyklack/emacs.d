;;; config.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'dash)

;;; Appearance
;; ;;;; Colorize strings that represent colors
;;
;; (use-package rainbow-mode
;;   :ensure t
;;   :blackout t
;;   :hook (emacs-lisp-mode conf-space-mode conf-toml-mode fish-mode toml-ts-mode))

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

;; ;;; Keybindings
;;
;; (use-package helix-leader
;;   :custom
;;   (helix-leader-send-C-x-with-control-modifier nil))
;;
;; (helix-keymap-global-set
;;   "M-;"   'eval-expression
;;   "C-M-;" 'repeat-complex-command)
;; (helix-keymap-global-set :state 'motion
;;   "<backspace>" #'execute-extended-command)
;; (helix-keymap-global-set :state 'normal
;;   "<backspace>" 'execute-extended-command
;;   "M-;"   nil ;; helix-exchange-point-and-mark
;;   "C-;"   'helix-exchange-point-and-mark)
;; ;; C-w prefix
;; (helix-keymap-set helix-window-map
;;   "N" 'other-tab-prefix)
;; ;; Insert state
;; (helix-keymap-global-set :state 'insert
;;   "C-w" 'backward-kill-word ;; together with C-backspace
;;   "C-/" 'dabbrev-expand)

;;; Provide `config'
(provide 'config)
;;; config.el ends here
