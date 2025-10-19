;;; config.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)

;;; Appearance

(add-hook 'prog-mode-hook #'blink-cursor-mode)

;;;; Color theme

(defmacro my-custom-theme-set-faces (theme &rest specs)
  (declare (indent 1))
  `(apply #'custom-theme-set-faces ,theme
          (mapcar (-lambda ((face . spec))
                    `(,face ((t ,spec))))
                  (list ,@specs))))

(use-package ef-themes
  :ensure (ef-themes :host github :repo "anuvyklack/ef-themes" :wait t)
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  ;; Use `ef-themes-toggle' to cycle between these themes.
  (ef-themes-to-toggle '(ef-light ef-dream))
  :config
  ;; Disable all other themes.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-light :no-confirm)
  ;; Load my customizations
  (require 'xdg)
  (load-file (file-name-concat (xdg-config-home) "emacs/modules/ef-light.el"))
  (enable-theme 'ef-light))

;;;; Colorize strings that represent colors

(use-package rainbow-mode
  :ensure t
  :blackout t
  :hook (emacs-lisp-mode conf-space-mode conf-toml-mode fish-mode toml-ts-mode))

;;;; Scrolling
;;;;; Do not jump half the page when point goes out of the screen.

(setopt scroll-conservatively 101)

;; Restore original value for some commands.
;; Mainly for functions that perform text replacement, to center the screen
;; on jumping to the next occurrence.
(dolist (cmd '(dired-do-find-regexp-and-replace
               projectile-replace
               projectile-replace-regexp))
  (advice-add cmd :around #'with-original-scroll-conservatively-value-a))

(defun with-original-scroll-conservatively-value-a (fun &rest args)
  "Meant to be used as `:around' advice."
  (let ((scroll-conservatively (my-original-value 'scroll-conservatively)))
    (apply fun args)))

;;; Keybindings

;; (use-package helix-leader
;;   :custom
;;   (helix-leader-send-C-x-with-control-modifier nil))

(with-eval-after-load 'helix
  (helix-keymap-global-set
    "M-;"   'eval-expression
    "C-M-;" 'repeat-complex-command)
  (helix-keymap-global-set :state 'motion
    "<backspace>" #'execute-extended-command)
  (helix-keymap-global-set :state 'normal
    "<backspace>" 'execute-extended-command
    "M-;"   nil ;; helix-exchange-point-and-mark
    "C-;"   'helix-exchange-point-and-mark
    "z SPC" 'cycle-spacing
    "z ."   'set-fill-prefix
    ;; goto commands
    "g <return>" 'consult-goto-line
    "g e" 'consult-compile-error
    ;; "g n" 'next-error
    ;; "g p" 'previous-error
    ;; "g /" 'consult-ripgrep
    ;; "g /" 'consult-line
    ;; "g ?" 'consult-line-multi
    ;; "g -" 'dired-jump
    )
  ;; C-w prefix
  (helix-keymap-set helix-window-map
    "N" 'other-tab-prefix)
  ;; Insert state
  (helix-keymap-global-set :state 'insert
    "C-w" 'backward-kill-word ;; together with C-backspace
    "C-/" 'dabbrev-expand))

;;; Provide `config'
(provide 'config)
;;; config.el ends here
