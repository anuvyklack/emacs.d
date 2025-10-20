;;; twist-keybindings.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(helix-keymap-global-set :state 'normal
  "z SPC" 'cycle-spacing
  "z ."   'set-fill-prefix)

;; <leader> key
(helix-keymap-set mode-specific-map
  "RET" 'bookmark-jump
  "," 'switch-to-buffer
  "/" 'consult-ripgrep
  "d" 'dired-jump
  "b" (cons "buffer"
            (define-keymap
              "i" 'ibuffer-jump
              "b" 'ibuffer-jump
              "n" 'switch-to-buffer ;; next to `b' key
              "s" 'save-buffer
              "w" 'write-file
              "d" 'kill-current-buffer
              "z" 'bury-buffer
              "g" 'revert-buffer
              "r" 'rename-buffer
              "m" 'bookmark-set
              "M" 'bookmark-delete
              "x" 'scratch-buffer))
  "o" (cons "open"
            (define-keymap
              "t" 'treemacs
              "i" 'imenu-list-smart-toggle))
  "s" (cons "search"  search-map)
  "p" (cons "project" project-prefix-map)
  "v" `("version control" . vc-prefix-map))

(helix-keymap-global-set
  "C-x C-b" 'ibuffer-jump ; override `list-buffers'
  "C-x C-r" 'recentf-open ; override `find-file-read-only'
  "C-x C-d" 'dired-jump)  ; override `list-directory'

(helix-keymap-set search-map
  "i" 'imenu)

(provide 'twist-keybindings)
;;; twist-keybindings.el ends here

