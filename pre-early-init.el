;;; pre-early-init.el --- Description -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:

;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
(setq minimal-emacs-var-dir (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" minimal-emacs-var-dir))
(setq user-emacs-directory minimal-emacs-var-dir)

;; Disable `package.el' built-in package manger. I use Elpaca.
(setq minimal-emacs-package-initialize-and-refresh nil)

(provide 'pre-early-init)
;;; pre-early-init.el ends here
