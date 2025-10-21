;;; twist-core.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(elpaca pcre2el)
(elpaca paredit)
(elpaca blackout (require 'blackout))

(elpaca avy
  (setq avy-keys (number-sequence ?a ?z) ;; Any lower-case letter a-z.
        avy-style 'at-full
        avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump t))

(elpaca (helix :host github :repo "anuvyklack/helix.el"
               :files (:defaults "**"))
  (helix-mode))

(elpaca-wait)

(elpaca nerd-icons)
(elpaca wgrep)

(require 'twist-editor)
(require 'twist-help)
(with-eval-after-load 'info (require 'twist-info))

(provide 'twist-core)
;;; twist-core.el ends here
