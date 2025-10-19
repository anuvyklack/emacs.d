;;; early-init.el --- Init -*- lexical-binding: t; -*-
;;
;; Author: Yuriy Artemyev
;; URL: https://github.com/anuvyklack/emacs-twist
;;
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.0.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;
;; early-init.el was introduced in Emacs 27.1. It is loaded before init.el,
;; before Emacs initializes its UI or package.el, and before site files are
;; loaded. This is great place for startup optimizing, because only here can
;; you *prevent* things from loading, rather than turn them off after-the-fact.
;;
;; This file borrows a lot from James Cherti's minimal-emacs.d, which is built
;; upon the foundation of Henrik Lissner's Doom Emacs — itself probably inspired
;; by something else whose origins have been lost to history. All credit goes to
;; the original projects.
;;
;;; Code:
;; In case of error start Emacs from command line with `--debug-init' key, or
;; uncomment:
;;-----------------------
;; (setq init-file-debug
;;       debug-on-error t
;;       debug-on-quit t)
;;-----------------------

;;; Garbage collector

(if noninteractive                      ; in CLI sessions
    (setq gc-cons-threshold 134217728)  ; 128mb
  ;; Else, disable garbage collection during startup.
  (setq-default gc-cons-threshold most-positive-fixnum))

;; Enable garbage collection after start up.
(add-hook 'emacs-startup-hook 'twist--restore-original-gc-values 105)

(defun twist--restore-original-gc-values ()
  "Reset `gc-cons-threshold' without user's config."
  (when (= (default-value 'gc-cons-threshold)
           most-positive-fixnum)
    (setq-default gc-cons-threshold (* 16 1024 1024)))) ; 16mb

;;; Native compilation and Byte compilation

(setq native-comp-warning-on-missing-source    debug-on-error
      native-comp-async-report-warnings-errors (or debug-on-error 'silent))

(setq jka-compr-verbose     debug-on-error
      byte-compile-warnings debug-on-error
      byte-compile-verbose  debug-on-error)

;;; Miscellaneous

(set-language-environment "UTF-8")

;; `set-language-environment' sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Increase how much is read from processes in a single chunk
(setq read-process-output-max (* 2 1024 1024))  ; 1024kb

(setq process-adaptive-read-buffering nil)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

(setq warning-minimum-level (if debug-on-error :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when debug-on-error
  (setq message-log-max 16384))

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; Disable warnings from the legacy advice API. They aren't useful.
(setq ad-redefinition-action 'accept)

;;; Performance
;;;; Performance: Miscellaneous options

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(when (and (not (daemonp)) (not noninteractive))
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering  'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  (unless debug-on-error
    ;; Unset command line options irrelevant to the current OS. These options
    ;; are still processed by `command-line-1` but have no effect.
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;;;; Performance: Inhibit redisplay

(unless (or (daemonp)
            noninteractive
            debug-on-error)
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook 'twist--reset-inhibit-redisplay -100))

(defun twist--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook 'twist--reset-inhibit-redisplay))

;;;; Performance: Inhibit message

;; Suppress startup messages for a cleaner experience. The tradeoff is that you
;; won't be informed of the progress or any relevant activities during startup.
(unless (or (daemonp)
            noninteractive
            debug-on-error)
  (setq-default inhibit-message t)
  (add-hook 'post-command-hook 'twist--reset-inhibit-message -100))

(defun twist--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'post-command-hook 'twist--reset-inhibit-message))

;;;; Performance: Disable mode-line during startup

;; Disable the mode line during startup to reduces visual clutter.
(unless (or (daemonp)
            noninteractive
            minimal-emacs-debug)
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil))))

;;;; Restore values

(advice-add 'startup--load-user-init-file :around 'twist--startup-load-user-init-file)

(defun twist--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
      ;; Start up as normal
      (apply fn args)
    ;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
    ;; see nothing but a blank Emacs frame.
    (setq-default inhibit-message nil
                  inhibit-redisplay nil)
    ;; Restore the mode-line
    (unless (default-toplevel-value 'mode-line-format)
      (setq-default mode-line-format (get 'mode-line-format 'initial-value)))))

;;; UI elements

(defvar twist-emacs-ui-elements '()
  "List of user interface features to enable in minimal Emacs setup.
This variable holds a list of Emacs UI features that can be enabled:
- context-menu (Enables the context menu in graphical environments.)
- tool-bar (Enables the tool bar in graphical environments.)
- menu-bar (Enables the menu bar in graphical environments.)
- dialogs (Enables both file dialogs and dialog boxes.)
- tooltips (Enables tooltips.)")

;; (setq frame-title-format twist-emacs-frame-title-format
;;       icon-title-format  twist-emacs-frame-title-format)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; We intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar twist-emacs-ui-elements)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil)))

(unless (or (daemonp) noninteractive)
  (when (fboundp 'tool-bar-setup)
    ;; Temporarily override the `tool-bar-setup' function to prevent it from
    ;; running during the initial stages of startup.
    (advice-add 'tool-bar-setup :override #'ignore)
    (advice-add 'startup--load-user-init-file :after 'twist--setup-toolbar)))

(defun twist--setup-toolbar (&rest _)
  "Setup the toolbar."
  (when (fboundp 'tool-bar-setup)
    (advice-remove 'tool-bar-setup #'ignore)
    (when (bound-and-true-p tool-bar-mode)
      (funcall 'tool-bar-setup))))

(unless (memq 'tool-bar twist-emacs-ui-elements)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(unless (memq 'tooltips twist-emacs-ui-elements)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs twist-emacs-ui-elements)
  (setq use-file-dialog nil))
(setq use-dialog-box (eq system-type 'android)) ; Android dialogs are better UX

;;; Security

(setq gnutls-verify-error t) ; Prompts user if there are certificate issues.
(setq tls-checktrust t) ; Ensure SSL/TLS connections undergo trust verification.
(setq gnutls-min-prime-bits 3072) ; Stronger GnuTLS encryption.

;;; package.el

;; Do not load built-in package manager.
(setq package-enable-at-startup nil)

;; Ensure that, if the user does want package.el, it is configured correctly.
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)))

;;; use-package

(setq use-package-compute-statistics debug-on-error
      ;; Setting use-package-expand-minimally to (t) results in a more compact
      ;; output that emphasizes performance over clarity.
      use-package-expand-minimally (not debug-on-error)
      use-package-minimum-reported-time (if debug-on-error 0 0.1)
      use-package-verbose debug-on-error
      use-package-enable-imenu-support t)

;;; Load paths

(setq load-prefer-newer t)

(defvar twist-root-directory (file-name-directory load-file-name)
  "The root directory of Twist's core files.
Must end with a directory separator.")

(setq user-emacs-directory   (expand-file-name "var/" twist-root-directory)
      custom-theme-directory (expand-file-name "themes/" user-emacs-directory)
      custom-file            (expand-file-name "custom.el" user-emacs-directory))

(add-to-list load-path (expand-file-name "core"    twist-root-directory))
(add-to-list load-path (expand-file-name "modules" twist-root-directory))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:
;;; early-init.el ends here
