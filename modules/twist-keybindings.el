;;; twist-keybindings.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(elpaca imenu-list)

(helix-keymap-global-set :state 'insert
  "C-/" 'hippie-expand)

(helix-keymap-global-set :state 'normal
  "z SPC" 'cycle-spacing
  "z ."   'set-fill-prefix)

;; <leader>
(helix-keymap-set mode-specific-map
  "RET" 'bookmark-jump
  "," 'switch-to-buffer
  "/" 'consult-ripgrep ; "/" is bound to search in Helix
  "d" 'dired-jump
  "b" (cons "buffer"
            (define-keymap
              "b" 'ibuffer-jump        ; "<leader> bb"
              "n" 'switch-to-buffer    ; next after "b"
              "s" 'save-buffer
              "w" 'write-file
              "d" 'kill-current-buffer ; also "C-w d"
              "z" 'bury-buffer         ; also "C-w z"
              "g" 'revert-buffer       ; also "C-w r"
              "r" 'rename-buffer
              "x" 'scratch-buffer
              ;; Bookmarks
              "m" 'bookmark-set
              "M" 'bookmark-delete))
  "f" (cons "file/find"
            (define-keymap
              ;; "x" 'xref-find-apropos
              "b" 'switch-to-buffer
              "f" 'find-file
              "F" 'consult-find
              "d" 'dired
              "l" 'locate
              "r" '("Recent files" . recentf-open)
              "w" 'write-file))
  "o" (cons "open"
            (define-keymap
              "t" 'treemacs
              "i" 'imenu-list-smart-toggle))
  "s" (cons "search"  search-map)
  "p" (cons "project" project-prefix-map)
  "v" (cons "version control" vc-prefix-map))

;; <leader> s
(helix-keymap-set search-map
  "i" 'imenu)

(helix-keymap-global-set
  "C-x C-b" 'ibuffer-jump ; override `list-buffers'
  "C-x C-r" 'recentf-open ; override `find-file-read-only'
  "C-x C-d" 'dired-jump)  ; override `list-directory'

(provide 'twist-keybindings)
;;; twist-keybindings.el ends here

