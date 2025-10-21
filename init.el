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
;;; Fonts

(set-face-font 'default (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(set-face-font 'fixed-pitch (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(setq use-default-font-for-symbols t)

;; (set-face-font 'default (font-spec :family "Inconsolata LGC" :size 17))
;; (setopt use-default-font-for-symbols nil)

;; (set-face-attribute 'fixed-pitch nil
;;                     :font (font-spec :family "PragmataPro Mono Liga" :size 13.9))
;; (set-face-attribute 'fixed-pitch-serif nil
;;                     :family "Iosevka Term Curly Slab Medium")

;; Nerd Font
(set-fontset-font t (cons ?\xf0001 ?\xf1af0) "Symbols Nerd Font Mono" nil 'prepend)

;; Unicode Symbols for Legacy Computing
(set-fontset-font t (cons ?\x1fb00 ?\x1fbca) "LegacyComputing" nil 'prepend)
(set-fontset-font t (cons ?🯰 ?🯹) "LegacyComputing" nil 'prepend)

;; (char-to-string ?\x1fb00) ;; ?🬀
;; (char-to-string ?\x1fbca) ;; ?🯊

;;; Package manager

;; In case you use VPN:
;;
;; Emacs populates `url-proxy-services' variable from:
;; `https_proxy', `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'twist-elpaca)

;;; Color theme
(require 'twist-utils)

(use-package ef-themes
  :ensure (ef-themes :host github :repo "anuvyklack/ef-themes" :wait t)
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  ;; Use `ef-themes-toggle' to cycle between these themes.
  (ef-themes-to-toggle '(ef-light ef-dream))
  :config
  (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes.
  (load-theme 'ef-light :no-confirm)
  ;; Load my customizations
  (twist-load-file "modules/ef-light.el")
  ;; (load-file (file-name-concat twist-root-directory "modules/ef-light.el"))
  (enable-theme 'ef-light))

;;; Emacs Twist modules

(require 'twist-core)
(require 'twist-emacs-lisp)
(require 'twist-keybindings)

(require 'twist-vertico)
(require 'twist-consult)
(require 'twist-embark)
(require 'twist-xref)

;; (require 'twist-dired)
;; (require 'twist-outline)
(require 'twist-deadgrep)
;; (require 'twist-edit-indirect)

;;; init.el ends here
