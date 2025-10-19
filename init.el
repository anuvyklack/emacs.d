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
(elpaca avy)
(elpaca pcre2el)
;; (elpaca edit-indirect)
(elpaca nerd-icons)

(elpaca blackout (require 'blackout))

(use-package helix
  :ensure (helix :repo "~/code/emacs/helix"
                 :files (:defaults "**")
                 :wait t)
  :hook (elpaca-after-init-hook . helix-mode))

(require 'twist-editor)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:
;;; init.el ends here
