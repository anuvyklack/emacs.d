;;; twist-help.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-unset help-map "h" :remove) ; unbind `view-hello-file'

(use-package helpful
  :ensure t
  :after helix
  :hook
  (helpful-mode-hook . outline-minor-mode)
  (helpful-mode-hook . disable-hl-line-mode)
  (help-mode-hook . disable-hl-line-mode)
  :custom
  (help-window-select t)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol))
  ;; :config
  )

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
              "k" 'which-key-show-keymap))
  "C-c" nil) ; unbind `describe-copying'

(provide 'twist-help)
;;; twist-help.el ends here
