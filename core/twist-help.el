;;; twist-help.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Enhance `apropos' and related functions to perform more extensive searches
(setq apropos-do-all t)

(use-package help
  :hook
  (help-mode-hook . disable-hl-line-mode)
  :custom
  (help-enable-autoload nil)
  (help-enable-completion-autoload nil)
  (help-enable-symbol-autoload nil)
  (help-window-select t) ; Focus new help windows when opened
  :config
  (keymap-unset help-map "h" t)    ; unbind `view-hello-file'
  (keymap-unset help-map "C-c" t)) ; unbind `describe-copying'

(use-package helpful
  :ensure t
  :hook
  ;; (helpful-mode-hook . outline-minor-mode)
  (helpful-mode-hook . disable-hl-line-mode)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)))

(helix-keymap-set help-map
  "F" 'describe-face
  "M" 'describe-keymap
  "s" 'helpful-symbol
  ;; Rebind `b' key from `describe-bindings' to prefix with more binding
  ;; related commands.
  "b" (cons "bindings"
            (define-keymap
              "b" 'describe-bindings
              "B" 'embark-bindings ;; alternative for `describe-bindings'
              "i" 'which-key-show-minor-mode-keymap
              "m" 'which-key-show-major-mode
              "t" 'which-key-show-top-level
              "f" 'which-key-show-full-keymap
              "k" 'which-key-show-keymap)))

(provide 'twist-help)
;;; twist-help.el ends here
