;;; post-init.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;-----------------------
;; Start emacs from command line with `--debug-init' key, or uncomment:
;; (setq debug-on-error t
;;       debug-on-quit t)
;;-----------------------

;;; Auto-compile Emacs lisp packages

(use-package compile-angel
  :ensure t
  :custom
  (compile-angel-verbose t)
  :config
  (dolist (file '("/pre-early-init.el"
                  "/early-init.el"
                  "/post-early-init.el"
                  "/pre-init.el"
                  "/custom.el"
                  "/init.el"
                  "/post-init.el"))
    (push file compile-angel-excluded-files))

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))

;;; Libraries

(use-package dash :ensure t)
(use-package f :ensure t)
(use-package s :ensure t)
(use-package blackout :ensure t)
(use-package nerd-icons :ensure t)

(use-package transient
  :ensure t
  :defer t
  :custom
  ;; Pop up transient windows at the bottom of the current window instead of
  ;; entire frame. This is more ergonomic for users with large displays or many
  ;; splits.
  (transient-display-buffer-action '(display-buffer-below-selected
                                     (dedicated . t)
                                     (inhibit-same-window . t)))
  (transient-show-during-minibuffer-read t)
  :config
  ;; Close transient menus with ESC.
  (keymap-set transient-map "<escape>" #'transient-quit-one))

;; (use-package casual :ensure t :defer t)
;; (use-package hydra :ensure t :defer t)

;;; Appearance
;;;; Line numbers

(use-package display-line-numbers
  :hook prog-mode text-mode conf-mode
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen t)
  (display-line-numbers-grow-only t))

;;; Private config


(provide 'post-init)
;;; post-init.el ends here
