;;; twist-defaults.el --- Emacs Twist -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright © 2025 Yuriy Artemyev
;;
;; Authors: Yuriy Artemyev <anuvyklack@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/anuvyklack/helix.el
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This file content is primarily taken from the following packages:
;; - `minimal-emacs.d' [[https://github.com/jamescherti/minimal-emacs.d]]
;; - `doom' [[https://github.com/doomemacs/doomemacs]]
;; All credit is given to the original projects.
;;
;;; Code:

;; ;; Ensure use-package is available
;; (require 'use-package)

;; Ask the user whether to terminate asynchronous compilations on exit.
;; This prevents native compilation from leaving temporary files in /tmp.
(setq native-comp-async-query-on-exit t)

;;; User interface

;; Which Function mode
(setq which-func-update-delay 1.0)

;;; Misc

(setq whitespace-line-column nil)  ; Use the value of `fill-column'.

;; Disable truncation of printed s-expressions in the message buffer.
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

(setq custom-buffer-done-kill t)

;;; comint (general command interpreter in a window)

(setq ansi-color-for-comint-mode t
      comint-prompt-read-only t
      comint-buffer-maximum-size 4096)

;;; Compilation

(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;; ;; Skip confirmation prompts when creating a new file or buffer
;; (setq confirm-nonexistent-file-or-buffer nil)

(provide 'twist-defaults)
;;; twist-defaults.el ends here
