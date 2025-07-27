;;; post-init.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
(require 'xdg)
;;-----------------------
;; Start emacs from command line with `--debug-init' key, or uncomment:
;; (setq debug-on-error t
;;       debug-on-quit t)
;;-----------------------

(elpaca leaf (require 'leaf))
(elpaca leaf-keywords
        (require 'leaf-keywords)
        ;; use :ensure keyword instead of :elpaca
        ;; (custom-set-variables '(leaf-alias-keyword-alist '((:ensure . :elpaca))))
        (leaf-keywords-init))
(elpaca-wait)

(leaf f :elpaca t :require t)
(leaf s :elpaca t :require t)
(leaf dash
  :elpaca (dash :wait t)
  :require t)

;; ;; Recursively add to `load-path' all folders in
;; ;; `$XDG_CONFIG_HOME/emacs/modules/' directory.
;; (let ((modules-dir (-> (file-name-concat (xdg-config-home) "emacs" "modules")
;;                        (file-name-as-directory))))
;;   (setq load-path `(,@load-path
;;                     ,modules-dir
;;                     ,@(f-directories modules-dir nil t))))

;;; Auto-compile Emacs lisp packages
;; (leaf compile-angel
;;   :elpaca t
;;   :require t
;;   :custom
;;   ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
;;   ;; Drawback: The minibuffer will not display compile-angel's actions.
;;   (compile-angel-verbose . t)
;;   :config
;;   ;; The following directive prevents compile-angel from compiling your init
;;   ;; files. If you choose to remove this push to `compile-angel-excluded-files'
;;   ;; and compile your pre/post-init files, ensure you understand the
;;   ;; implications and thoroughly test your code. For example, if you're using
;;   ;; `use-package', you'll need to explicitly add `(require 'use-package)` at
;;   ;; the top of your init file.
;;   (push "/init.el" compile-angel-excluded-files)
;;   (push "/early-init.el" compile-angel-excluded-files)
;;   (push "/pre-init.el" compile-angel-excluded-files)
;;   (push "/post-init.el" compile-angel-excluded-files)
;;   (push "/pre-early-init.el" compile-angel-excluded-files)
;;   (push "/post-early-init.el" compile-angel-excluded-files)
;;
;;   ;; A local mode that compiles .el files whenever the user saves them.
;;   ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
;;
;;   ;; A global mode that compiles .el files before they are loaded.
;;   (compile-angel-on-load-mode))

;; ;;; Load packages configs
;; (require 'my-utils)
;; (require 'my-emacs-core)
;; (require 'my-color-scheme)
;; ;; (require 'my-motions)
;; ;; (require 'my-smooth-scrolling)
;; (require 'my-completion)

;; ;; (require 'my-meow)
;; (require 'my-helix)

;;; Utils

;; https://stackoverflow.com/questions/24356401/how-to-append-multiple-elements-to-a-list-in-emacs-lisp
(defun my-add-to-list (list elements &optional append)
  "Add ELEMENTS to the front of the LIST.

If APPEND is non-nil add ELEMENTS to the end of the LIST.
This function change the value of the LIST symbol.

LIST sould be a symbol.
ELEMENTS could be either a list or a single element."
  (set list (if append
                (append (symbol-value list)
                        (ensure-list elements))
              ;; else
              (append (ensure-list elements)
                      (symbol-value list)))))

(defmacro my-custom-theme-set-faces (theme &rest specs)
  (declare (indent 1))
  `(apply #'custom-theme-set-faces ,theme
          (mapcar (-lambda ((face . spec))
                    `(,face ((t ,spec))))
                  (list ,@specs))))

;;; Appearance

(add-hook 'prog-mode-hook #'blink-cursor-mode)

(leaf display-line-numbers
  :hook prog-mode-hook text-mode-hook conf-mode-hook
  :custom
  (display-line-numbers-type . t)
  ;; Explicitly define a width to reduce the cost of on-the-fly computation.
  (display-line-numbers-width . 3)
  (display-line-numbers-width-start . t)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen . t)
  (display-line-numbers-grow-only . t))

(add-hook 'prog-mode-hook
          #'(lambda ()
              (setq-local
               ;; Highlight trailing whitespaces with `trailing-whitespace' face.
               ;; Use `delete-trailing-whitespace' command.
               show-trailing-whitespace t
               ;; Display `fill-column' indicator.
               ;; BUG in `display-fill-column-indicator-mode':
               ;; Comparing 2 strings with `eq' instead of `equal' and get nil.
               ;; So have to set this manually.
               display-fill-column-indicator t
               display-fill-column-indicator-character ?\u2502)))

;;;; Color theme

(leaf ef-themes
  :elpaca (ef-themes :host github :repo "anuvyklack/ef-themes" :wait t)
  ;; :elpaca (ef-themes :host github :repo "anuvyklack/ef-themes")
  :require t
  :custom
  (ef-themes-mixed-fonts . t)
  (ef-themes-variable-pitch-ui . t)
  ;; Use `ef-themes-toggle' to cycle between these themes.
  (ef-themes-to-toggle . '(ef-light ef-dream))
  :config
  ;; Disable all other themes.
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'ef-light :no-confirm)
  ;; Load my customizations
  (load-file (file-name-concat (xdg-config-home) "emacs/modules/ef-light.el"))
  (enable-theme 'ef-light))

;;; Core settings

(leaf emacs
  :custom
  ;; User credentials. Some functionality uses this to identify you,
  ;; e.g. GPG configuration, email clients, file templates and snippets.
  (user-full-name . "Yuriy Artemyev")
  (user-mail-address . "anuvyklack@gmail.com")
  (confirm-kill-emacs . nil)
  (history-delete-duplicates . t)
  ;; `what-cursor-position' will show human readable output.
  (what-cursor-show-names . t)
  ;; Don't show byte-compile warnings at start.
  (byte-compile-warnings . nil)
  ;; Show current key-sequence in minibuffer ala 'set showcmd' in vim.
  ;; Any feedback after typing is better UX than no feedback at all.
  (echo-keystrokes . 0.02)
  :hook
  ;; Automatically revert the buffer when its visited file changes on disk.
  ;; Auto Revert will not revert a buffer if it has unsaved changes, or if
  ;; its file on disk is deleted or renamed.
  (after-init-hook . global-auto-revert-mode)
  ;; Save minibuffer history between sessions.
  (after-init-hook . savehist-mode)
  ;; Save the last location within a file upon reopening.
  (after-init-hook . save-place-mode))

;; Keep track of opened files.
(leaf recentf
  :custom `(recentf-auto-cleanup . ,(if (daemonp) 300))
  :hook ((after-init-hook . (lambda()
                              (let ((inhibit-message t))
                                (recentf-mode 1))))
         (kill-emacs-hook . recentf-cleanup)))

(leaf helpful
  :elpaca t
  :hook (helpful-mode-hook . outline-minor-mode)
  :custom (help-window-select . t)
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         (help-map
          :package help
          ("F" . describe-face)
          ("M" . describe-keymap)
          ("s" . helpful-symbol)
          ("C-c" . nil)) ;; unbind `describe-copying'
         (embark-symbol-map
          :package embark
          ("h" . helpful-symbol))))

(leaf image-mode
  :custom (image-animate-loop . t))

(leaf rainbow-delimiters
  :elpaca t
  ;; :hook prog-mode-hook text-mode-hook conf-mode-hook
  ;; (prog-mode-hook . rainbow-delimiters-mode)
  :hook prog-mode-hook conf-mode-hook)

;;; Minibuffer & Completion

(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

;; Allow opening new minibuffers from inside existing minibuffers.
(setopt enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

;;;; orderless
(leaf orderless
  :elpaca t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

;;;; vertico
(leaf vertico
  :elpaca t
  :after helix
  :global-minor-mode vertico-mode
  :custom
  (vertico-count . 15) ;; How many candidates to show
  (vertico-scroll-margin . 2)
  (vertico-cycle . nil)
  (vertico-resize . 'grow-only) ;; Grow and shrink the Vertico minibuffer
  :hook
  (minibuffer-setup-hook . vertico-repeat-save)
  :bind (vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ;; ("<escape>" . meow-motion-mode)
         ;; ("<escape>" . meow-minibuffer-quit)
         )
  :config
  (helix-keymap-set vertico-map 'normal
    ;; "M-<return>" 'vertico-exit-input ;; default setting
    "<tab>"     'next-history-element
    "<backtab>" 'previous-history-element
    "C-p" 'consult-yank-from-kill-ring
    "n" 'vertico-next-group
    "N" 'vertico-previous-group
    ;; "C-h" (lambda ()
    ;;         (cond ((eq 'file (vertico--metadata-get 'category))
    ;;                (call-interactively #'vertico-directory-up))))
    "C-h" 'vertico-directory-up
    "C-l" #'vertico-insert
    ;; Russian
    "C-о" 'vertico-next
    "C-л" 'vertico-previous))

(leaf vertico-directory
  :after vertico
  :hook
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (helix-keymap-set vertico-map 'normal
    "C-h" 'vertico-directory-up
    ;; "C-h" (lambda ()
    ;;         (cond ((eq 'file (vertico--metadata-get 'category))
    ;;                (call-interactively #'vertico-directory-up))))
    ))

(leaf marginalia
  :elpaca t
  :commands (marginalia-mode marginalia-cycle)
  :global-minor-mode marginalia-mode
  :bind (minibuffer-local-map
         ("M-a" . marginalia-cycle)))

;;;; embark
(leaf embark
  :elpaca t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :setq (prefix-help-command . 'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-m" . embark-act)
         ("M-m" . embark-dwim)
         (help-map
          :package help
          ("B" . embark-bindings)) ;; alternative for `describe-bindings'
         ;; (embark-general-map
         ;;  ("C-s" . nil) ;; embark-isearch-forward
         ;;  ("C-r" . nil) ;; embark-isearch-backward
         ;;  ("m" . mark)) ;; C-SPC
         ;; (embark-expression-map
         ;;  ("j" . forward-list)   ;; n
         ;;  ("k" . backward-list)) ;; p
         ))

(leaf embark-consult
  :elpaca t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; corfu
(leaf corfu
  :elpaca t
  :global-minor-mode global-corfu-mode
  :custom
  (corfu-auto . t)
  (corfu-auto-delay . 0.24)
  (corfu-auto-prefix . 2)
  (corfu-cycle . t)
  (corfu-count . 16)
  (corfu-max-width . 120)
  (corfu-quit-at-boundary . 'separator) ;; M-SPC to continue completion.
  (corfu-quit-no-match . 'separator)
  ;; When the completion popup is visible, by default the current candidate is
  ;; previewed into the buffer, and further input commits that candidate as
  ;; previewed. The feature is in line with other common editors.
  ;; - t :: non-inserting preview
  (corfu-preview-current . 'insert)
  (corfu-preselect . 'prompt)
  (corfu-on-exact-match . nil) ;; Handling of exact matches
  (global-corfu-minibuffer . t)
  (tab-always-indent . 'complete)
  (tab-first-completion . 'word)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion . nil))

;;;; cape
(leaf cape
  :elpaca t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :hook
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'.
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file)
  (completion-at-point-functions . cape-elisp-block))

;;; Search
;;;; Consult

(leaf consult
  :elpaca t
  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode . consult-preview-at-point-mode)
  :setq
  ;; Configure the register formatting. This improves the register.
  (register-preview-delay . 0.5)
  (register-preview-function . 'consult-register-format)
  :config
  ;; Tweak the register preview window.
  (advice-add 'register-preview :override #'consult-register-window)

  ;; Aggressive asynchronous that yield instantaneous results. (suitable for
  ;; high-performance systems.) Note: Minad, the author of Consult, does not
  ;; recommend aggressive values.
  ;; Read: https://github.com/minad/consult/discussions/951
  ;;
  ;; However, the author of minimal-emacs.d uses these parameters to achieve
  ;; immediate feedback from Consult.
  ;; (setq consult-async-input-debounce 0.02
  ;;       consult-async-input-throttle 0.05
  ;;       consult-async-refresh-delay 0.02)

  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")

  ;; C-c bindings are in `mode-specific-map'
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings are in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ;; ("C-x r b" . consult-bookmark)
         ;; ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (isearch-mode-map :package isearch
                           ("M-e" . consult-isearch-history)
                           ("M-s e" . consult-isearch-history)
                           ("M-s l" . consult-line)
                           ("M-s L" . consult-line-multi))
         ;; Minibuffer history
         (minibuffer-local-map :package emacs
                               ("M-s" . consult-history)
                               ("M-r" . consult-history))))

;;; IDE
;;;; xref (goto definition)

;; (leaf dumb-jump
;;   :elpaca t
;;   :init (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(leaf xref
  :elpaca dumb-jump
  :hook (xref-backend-functions . dumb-jump-xref-activate)
  :custom ((xref-auto-jump-to-first-definition . 'show)
           (xref-prompt-for-identifier . nil)
           (xref-history-storage . #'xref-window-local-history)
           ;; (xref-show-definitions-function . #'xref-show-definitions-buffer)
           (xref-show-definitions-function . #'xref-show-definitions-buffer-at-bottom)
           ;; (xref-show-xrefs-function . #'consult-xref)
           ;; (xref-show-definitions-function . #'consult-xref)
           )
  :config
  (advice-add 'xref-find-definitions :around #'my-xref-try-all-backends)
  (advice-add 'xref-find-references :around #'my-xref-try-all-backends))

;; (defun my-xref-try-all-backends (orig-fun &rest args)
;;   (let (;; (pnt (point-marker))
;;         (pnt (point))
;;         (buffer (current-buffer))
;;         jumped
;;         (xref-after-jump-hook (cons (lambda () (setq jumped t))
;;                                     xref-after-jump-hook)))
;;     (cl-dolist (backend (my-hook-values 'xref-backend-functions))
;;       (ignore-error user-error
;;         (let ((xref-backend-functions (list backend)))
;;           (apply orig-fun args)))
;;       (if (and jumped
;;                (or (not (equal buffer (current-buffer)))
;;                    (/= pnt (point))))
;;           (cl-return)
;;         (setq jumped nil)))))

(defun my-xref-try-all-backends (orig-fun &rest args)
  "Try all `xref-backend-functions' in row until first succeed."
  (let* (jumped
         (xref-after-jump-hook (cons (lambda () (setq jumped t))
                                     xref-after-jump-hook)))
    (cl-dolist (backend (my-hook-values 'xref-backend-functions))
      (ignore-error user-error
        (let ((xref-backend-functions (list backend)))
          (apply orig-fun args)
          (when jumped (cl-return)))))))

(defun my-hook-values (hook)
  "Return list with all local and global elements of the HOOK.
HOOK should be a symbol."
  (if (local-variable-p hook)
      (append (-> (buffer-local-value hook (current-buffer))
                  (butlast)) ;; Last element of local hook is always t.
              (default-value hook))
    ;; else
    (symbol-value hook)))

;;; Major-modes
;;;; Emacs Lisp

(leaf elisp-mode
  :after helix
  :hook
  (emacs-lisp-mode-hook
   . (lambda ()
       (setq-local tab-width 8
                   ;; outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
                   )
       ;; Order matters because `outline-minor-mode' and `helix-paredit-mode'
       ;; both binds `C-j' and `C-k', and I want `helix-paredit-mode' bindings
       ;; overlap `outline-minor-mode' bindings.
       (outli-mode 1)
       (helix-paredit-mode 1)))
  :config
  ;; ;; Treat `-' char as part of the word on 'w', 'e', 'b', motions.
  ;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  ;; (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
  (dolist (keymap (list emacs-lisp-mode-map
                        lisp-data-mode-map))
    (helix-keymap-set keymap 'normal
      "C-c k" '("Documentation" . helpful-at-point)
      "M" '("Documentation" . helpful-at-point))))

;; ;; Extra faces definded by `lisp-extra-font-lock' package:
;; ;; - `lisp-extra-font-lock-backquote'
;; (leaf lisp-extra-font-lock
;;   :elpaca t
;;   :require t
;;   :config
;;   :global-minor-mode lisp-extra-font-lock-global-mode)

;; `highlight-defined-builtin-function-name-face'
(leaf highlight-defined
  :elpaca t
  :custom (highlight-defined-face-use-itself . nil)
  :hook ((help-mode-hook . highlight-defined-mode)
         (emacs-lisp-mode-hook . highlight-defined-mode)))

;; ;; Highlight quoted symbols
;; (leaf highlight-quoted
;;   :elpaca t
;;   :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

(leaf elisp-def
  :elpaca t
  :after helix
  :hook (emacs-lisp-mode-hook
         . (lambda ()
             (remove-hook 'xref-backend-functions #'elisp--xref-backend :local)))
  :init
  (helix-keymap-set emacs-lisp-mode-map 'normal
    "g d" '("Find definition" . my-elisp-find-definitions)))

(defun my-elisp-find-definitions ()
  "Try `elisp-def', on fail try other xref backends."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))
  (or (ignore-errors (call-interactively #'elisp-def))
      (call-interactively #'xref-find-definitions)))

(leaf helix-paredit
  :elpaca
  paredit
  (helix-paredit :repo "~/code/emacs/helix-paredit")
  :after helix
  ;; :hook (emacs-lisp-mode-hook . helix-paredit-mode)
  :defer-config
  (helix-keymap-set helix-paredit-mode-map 'normal
    "C-c w" 'paredit-wrap-round
    "C-h" 'helix-paredit-backward
    "C-j" 'helix-paredit-down-sexp
    "C-k" 'helix-paredit-backward-up-sexp
    "C-l" 'helix-paredit-forward))

(leaf elisp-demos
  :elpaca t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;; Extra facilities

(leaf which-key
  :global-minor-mode which-key-mode
  :custom ((which-key-idle-delay . 1.5)
           (which-key-idle-secondary-delay . 0.25)
           (which-key-add-column-padding . 1)
           (which-key-max-description-length . 40)))

(leaf avy
  :elpaca t
  :init
  (setopt avy-keys (number-sequence ?a ?z) ;; Any lower-case letter a-z.
          avy-style 'at-full
          avy-all-windows nil
          avy-all-windows-alt t
          avy-background t
          ;; the unpredictability of this (when enabled) makes it a poor default
          avy-single-candidate-jump t))

;;;; outline
;; Wrapper around `outline'
(leaf outli
  :elpaca (outli :host github :repo "jdtsmith/outli")
  :after helix
  ;; :hook (emacs-lisp-mode-hook . outli-mode)
  :custom
  ;; Use <tab> and S-<tab> to cycle while point is on the button overlay.
  ;; (outline-minor-mode-use-buttons . t)
  :setq
  (outline-level . #'my-lisp-outline-level)
  :defer-config
  (helix-keymap-set outline-overlay-button-map nil
    "<tab>" #'outline-cycle
    "<backtab>" #'outline-cycle-buffer)
  (helix-keymap-set outline-minor-mode-map 'normal
    ;; outline-mark-subtree
    "z <tab>" #'outline-cycle
    "z <backtab>" #'outline-cycle-buffer
    "z e" #'outline-show-entry
    "z u" #'outline-up-heading
    "C-j" #'outline-forward-same-level
    "C-k" #'outline-backward-same-level
    "z j" #'outline-next-visible-heading
    "z k" #'outline-previous-visible-heading
    "z c" #'outline-hide-entry
    "z C" #'outline-hide-leaves
    "z o" #'outline-show-entry
    "z O" #'outline-show-subtree
    "z m" #'outline-hide-sublevels
    "z R" #'outline-show-all
    ;; "z M" #'
    "z S" '("Outline structure" . outline-hide-body)
    "z p" '("Outline path" . outline-hide-other)
    "z >" #'outline-promote
    "z <" #'outline-demote
    "M-h" #'outline-promote
    "M-l" #'outline-demote
    "M-j" #'outline-move-subtree-down
    "M-k" #'outline-move-subtree-up)
  ;; (leaf 'foldout
  ;;   :config
  ;;   (helix-keymap-set outline-mode-map 'normal
  ;;     "z n" #'foldout-zoom-subtree
  ;;     "z w" #'foldout-exit-fold)
  ;;   (helix-keymap-set outline-minor-mode-map 'normal
  ;;     "z n" #'foldout-zoom-subtree
  ;;     "z w" #'foldout-exit-fold))
  )

(defun my-lisp-outline-level ()
  "Return outline level for comment at point.
Replacement for `lisp-outline-level'."
  (if (match-beginning 1)
      (- (match-end 1) (match-beginning 1))
    0))

;;; Commands

(define-advice keyboard-quit (:around (orig-fun) quit-current-context)
  "Quit the current context.
When there is an active minibuffer and we are not inside it close it.
When we are inside the minibuffer use `abort-recursive-edit' which
quits any active region before exiting.  When there is no minibuffer
`keyboard-quit' unless we are defining or executing a macro."
  (if (active-minibuffer-window)
      (abort-recursive-edit)
    (unless (or defining-kbd-macro
                executing-kbd-macro)
      (call-interactively orig-fun))))

;;; Keybindings
;;;; General Keybindings

;; Rebind `universal-argument' from `C-u' to `M-u'.
;; By default `M-u' is binded to `upcase-word', so we can reuse it,
;; and `C-u' I use for scrolling like in Vim.
(keymap-global-set "M-u" #'universal-argument)
(keymap-set universal-argument-map "M-u" #'universal-argument-more)

(leaf helix
  ;; :load-path "~/code/emacs/helix"
  :elpaca
  pcre2el
  (helix :repo "~/code/emacs/helix")
  :require helix keypad
  :global-minor-mode helix-mode
  :custom
  ;; Vertical motion starting at end of line keeps to ends of lines.
  (track-eol . t)
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  :bind
  ;; ;; By default binded to `RET', but I rebind `C-m' which is also `RET'.
  ;; ("<return>" . newline)
  :config
  (helix-keymap-set nil 'normal
    "<backspace>" #'execute-extended-command
    "g o"   #'exchange-point-and-mark
    "C-M-;" #'eval-expression ;; default M-; but in Helix it reverse region
    "M-o"   #'pop-to-mark-command
    "C-S-o" #'pop-global-mark
    "C-w n" #'other-window-prefix
    "g a"   #'describe-char)
  (helix-keymap-set global-map nil
    "C-x C-b" #'ibuffer       ;; list-buffers
    "C-x C-r" #'recentf-open) ;; find-file-read-only
  ;; <leader> key
  (helix-keymap-set mode-specific-map nil
    ;; "f x" #'xref-find-apropos
    "f f" #'find-file
    "f F" #'+default/find-file-under-here
    "f d" #'dired
    "f l" #'locate
    "f r" '("Recent files" . recentf-open)
    "f R" #'projectile-recentf
    ;; "f u" '("Sudo this file" . doom/sudo-this-file)
    ;; "f U" '("Sudo find file" . doom/sudo-find-file)
    ;; "f x" '("Open scratch buffer" . doom/open-scratch-buffer)
    ;; "f X" '("Switch to scratch buffer" . doom/switch-to-scratch-buffer)
    ))

;; (leaf keypad
;;   :load-path "~/code/emacs/helix"
;;   :require t
;;   :config
;;   ;; (helix-keymap-set nil 'normal
;;   ;;   "SPC" #'keypad
;;   ;;   "C-h k" #'keypad-describe-key)
;;   )

(provide 'post-init)
;;; post-init.el ends here
