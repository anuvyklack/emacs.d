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

;; Accept shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t
      read-answer-short 'auto)

;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;; Minibuffer

;; Allow nested minibuffers
(setq enable-recursive-minibuffers t)

;; Keep the cursor out of the read-only portions of the minibuffer
(setq minibuffer-prompt-properties '( read-only t
                                      intangible t
                                      cursor-intangible t
                                      face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; User interface

;; Which Function mode
(setq which-func-update-delay 1.0)

(keymap-unset help-map "h" :remove) ; unbind `view-hello-file'

;; No beeping or blinking
(setq visible-bell nil
      ring-bell-function #'ignore)

;;; Show-paren

(setopt show-paren-delay 0.1
        show-paren-highlight-openparen t
        ;; show-paren-when-point-inside-paren t
        ;; show-paren-when-point-in-periphery t
        )

;;; Misc

(setq custom-buffer-done-kill t)

(setq whitespace-line-column nil)  ; Use the value of `fill-column'.

;; Can be activated with `display-line-numbers-mode'
(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)

(setq truncate-string-ellipsis "…")

;; Disable truncation of printed s-expressions in the message buffer
(setq eval-expression-print-length nil
      eval-expression-print-level nil)

;; Position underlines at the descent line instead of the baseline.
(setq x-underline-at-descent-line t)

(setq tramp-verbose 1)
(setq tramp-completion-reread-directory-timeout 50)
(setq remote-file-name-inhibit-cache 50)

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings
(setq imenu-max-item-length 160)

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;; This setting forces Emacs to save bookmarks immediately after each change.
;; Benefit: you never lose bookmarks if Emacs crashes.
(setq bookmark-save-flag 1)

(provide 'twist-defaults)
;;; twist-defaults.el ends here
