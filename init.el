;;; init.el --- Init -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev
;; URL: https://github.com/anuvyklack/emacs-twist
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;; Code:

(require 'twist-elpaca)

(elpaca dash)
(elpaca f)
(elpaca s)
(elpaca pcre2el)
(elpaca blackout (require 'blackout))
(elpaca nerd-icons)

(elpaca avy
  (setq avy-keys (number-sequence ?a ?z) ;; Any lower-case letter a-z.
        avy-style 'at-full
        avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump t))

(use-package helix
  :ensure (helix :repo "~/code/emacs/helix"
                 :files (:defaults "**"))
  :hook (elpaca-after-init-hook . helix-mode))

(elpaca-wait)

(require 'twist-editor)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:
;;; init.el ends here
