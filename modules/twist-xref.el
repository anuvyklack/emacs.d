;;; twist-xref.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable completion in the minibuffer instead of the definitions buffer
(setq xref-show-definitions-function 'xref-show-definitions-completing-read
      xref-show-xrefs-function 'xref-show-definitions-completing-read)

(use-package xref
  :custom
  ((xref-search-program  'ripgrep) ;; or 'ugrep
   (xref-auto-jump-to-first-definition 'show)
   (xref-prompt-for-identifier nil)
   (xref-history-storage #'xref-window-local-history)
   ;; (xref-show-definitions-function #'xref-show-definitions-buffer-at-bottom)
   ;; (xref-show-definitions-function #'xref-show-definitions-buffer)
   ;; (xref-show-xrefs-function #'xref--show-xref-buffer)
   (xref-show-xrefs-function #'consult-xref)
   (xref-show-definitions-function #'consult-xref))
  :config
  (advice-add 'xref-find-definitions :around #'my-xref-try-all-backends-a)
  (advice-add 'xref-find-references  :around #'my-xref-try-all-backends-a))

(use-package dumb-jump
  :ensure t
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(use-package nerd-icons-xref
  :ensure t
  :after xref
  :config (nerd-icons-xref-mode))

(provide 'twist-xref)
;;; twist-xref.el ends here
