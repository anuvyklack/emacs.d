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
(leaf hydra :elpaca t)
(leaf delight :elpaca t)
(leaf transient :elpaca t)
(leaf casual :elpaca t)
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

;;; My functions

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

(defun my-keymap-set (keymap &rest bindings)
  "Create keybinding from KEY to DEFINITION in KEYMAP.
Accepts any number of KEY DEFINITION pairs.
KEYMAP can be nil, then keybindings will be set in main STATE keymap.
This function is a wrapper around `keymap-set' that allows to set multiple
keybindings at once.

KEY, DEFINITION arguments are like those of `keymap-set'.
If DEFINITION is nil, then keybinding will be unset with `keymap-unset'
instead.

\(fn KEYMAP &rest [KEY DEFINITION]...)"
  (declare (indent defun))
  (unless (cl-evenp (length bindings))
    (user-error "The number of [KEY DEFINITION] pairs is not even"))
  (unless keymap (setq keymap (current-global-map)))
  (while bindings
    (let ((key (pop bindings))
          (definition (pop bindings)))
      (if definition
          (keymap-set keymap key definition)
        (keymap-unset keymap key :remove)))))

(defun my-original-value (symbol)
  "Return SYMBOL's original value."
  (car (get symbol 'standard-value)))

;;; Appearance

(add-hook 'prog-mode-hook #'blink-cursor-mode)

(leaf display-line-numbers
  :hook prog-mode-hook text-mode-hook conf-mode-hook
  :custom
  (display-line-numbers-type . t)
  ;; Explicitly define a width to reduce the cost of on-the-fly computation.
  ;; (display-line-numbers-width . 3)
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

;;;; Colorize strings that represent colors

(leaf rainbow-mode
  :elpaca t
  :delight t
  :hook (emacs-lisp-mode-hook
         conf-space-mode-hook
         conf-toml-mode-hook
         fish-mode-hook
         toml-ts-mode-hook))

;;;; prettify-symbols-mode

(setopt prettify-symbols-unprettify-at-point t)

;;;; Scrolling

;; ;; Move point to top/bottom of buffer before signaling a scrolling error.
;; (setq scroll-error-top-bottom nil)

;; Why is `jit-lock-stealth-time' nil by default?
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-02/msg00352.html
(setq jit-lock-stealth-time 1.25 ; Calculate fonts when idle for 1.25 seconds
      jit-lock-stealth-nice 0.5  ; Seconds between font locking
      jit-lock-chunk-size 4096)

(setq jit-lock-defer-time 0)
(with-eval-after-load 'helix
  ;; Avoid fontification while typing
  (add-hook 'helix-insert-state-enter-hook (lambda () (setq jit-lock-defer-time 0.25)))
  (add-hook 'helix-insert-state-exit-hook  (lambda () (setq jit-lock-defer-time 0))))

;;;;; Scrolling with mouse and touchpad

(leaf ultra-scroll
  :elpaca (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  :global-minor-mode ultra-scroll-mode
  :custom
  (mouse-wheel-tilt-scroll . t) ;; Scroll horizontally with mouse side wheel.
  (mouse-wheel-progressive-speed . nil)
  ;; Enable smooth scrolling with PageDown and PageUp keys
  (pixel-scroll-precision-interpolate-page . t)
  ;; (pixel-scroll-precision-large-scroll-height . 20.0)
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up))

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

;;; Core settings

(leaf emacs
  :delight (eldoc-mode nil t)
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
  ;; :custom `(recentf-auto-cleanup . ,(if (daemonp) 300 'mode))
  :config
  (setopt recentf-auto-cleanup (if (daemonp) 300 'mode))
  :hook ((after-init-hook . (lambda()
                              (let ((inhibit-message t))
                                (recentf-mode 1))))
         (kill-emacs-hook . recentf-cleanup)))

;; ;; `describe-repeat-maps'
;; (leaf repeat
;;   :global-minor-mode repeat-mode
;;   :custom
;;   (repeat-exit-key . "<escape>")
;;   (repeat-check-key . nil))

(leaf image-mode
  :custom (image-animate-loop . t))

(leaf rainbow-delimiters
  :elpaca t
  ;; :hook prog-mode-hook text-mode-hook conf-mode-hook
  ;; (prog-mode-hook . rainbow-delimiters-mode)
  :hook prog-mode-hook conf-mode-hook)

;;;; Help system

(leaf helpful
  :elpaca t
  :require helpful
  :hook (helpful-mode-hook . outline-minor-mode)
  :custom (help-window-select . t)
  :bind `(([remap describe-function] . helpful-callable)
          ([remap describe-variable] . helpful-variable)
          ([remap describe-command] . helpful-command)
          ([remap describe-key] . helpful-key)
          ([remap describe-symbol] . helpful-symbol))
  :config
  (my-keymap-set help-map
    "F" 'describe-face
    "M" 'describe-keymap
    "s" 'helpful-symbol
    ;; Rebind `b' key from `describe-bindings' to prefix with more binding
    ;; related commands.
    "b" `("bindings" . ,(define-keymap
                          "b" 'describe-bindings
                          "B" 'embark-bindings ;; alternative for `describe-bindings'
                          "i" 'which-key-show-minor-mode-keymap
                          "m" 'which-key-show-major-mode
                          "t" 'which-key-show-top-level
                          "f" 'which-key-show-full-keymap
                          "k" 'which-key-show-keymap))
    "C-c" nil)) ;; unbind `describe-copying'

;; define-key
;; global-set-key

;;; Windows
;;;; tab-bar

;; Each tab represents a window configuration (like in Vim).
(leaf tab-bar
  :after helix
  :global-minor-mode
  tab-bar-mode
  ;; Like `winner-mode' but for `tab-bar-mode'.
  tab-bar-history-mode
  :custom
  ;; Replacing `tab-bar-format-tabs' with `tab-bar-format-tabs-groups' will
  ;; group tabs on the tab bar.
  ;;
  ;; Possible values:
  ;; - tab-bar-format-menu-bar
  ;; - tab-bar-format-tabs
  ;; - tab-bar-format-tabs-groups
  ;; - tab-bar-separator
  ;; - tab-bar-format-add-tab
  ;; - tab-bar-format-align-right
  ;; - tab-bar-format-global
  ;;
  ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/etc/NEWS?id=f9b737fb9d21ac7adff403274167e76e77d033b8
  (tab-bar-format . '(tab-bar-format-history
                      tab-bar-format-tabs-groups
                      tab-bar-separator
                      tab-bar-format-add-tab))
  ;; (tab-bar-new-tab-choice . "*dashboard*") ;; Buffer to show in new tab.
  (tab-bar-tab-hints . nil) ;; Show tab numbers.
  (tab-bar-close-button-show . nil)
  ;; - 1 :: Hide tab bar if only 1 tabs open.
  ;; - t :: Always show tab bar.
  (tab-bar-show . t)
  (tab-bar-history-limit . 20)
  :config
  ;; (global-set-key [remap winner-undo] #'tab-bar-history-back)
  ;; (global-set-key [remap winner-redo] #'tab-bar-history-forward)
  (keymap-global-set "<remap> <winner-undo>" #'tab-bar-history-back)
  (keymap-global-set "<remap> <winner-redo>" #'tab-bar-history-forward)
  (dolist (state '(normal motion))
    ;; tab-bar-mode-map
    (helix-keymap-global-set state
                             "C-<tab>"     #'tab-next
                             "C-<backtab>" #'tab-previous
                             "] t" #'tab-next
                             "[ t" #'tab-previous))
  (my-keymap-set helix-window-map
    "<tab>"     '("New tab" . my-tab-new)
    "<backtab>" '("Duplicate tab" . tab-duplicate)
    "C-<tab>"   #'other-tab-prefix
    "u" '("Winner undo" . tab-bar-history-back)
    "U" '("Winner redo" . tab-bar-history-forward)
    "t" (cons "tab-bar" (define-keymap
                          "t" #'my-tab-new
                          "T" #'tab-duplicate
                          "c" #'tab-close
                          "C" #'tab-close-other ;; Close all other tabs.
                          "g" #'tab-group       ;; Add current tab to group.
                          ">" #'tab-bar-move-tab
                          "<" #'tab-bar-move-tab-backward
                          "F" #'tab-detach
                          "n" #'other-tab-prefix
                          "d" #'dired-other-tab
                          "r" #'tab-rename
                          "u" #'tab-undo)))) ;; Restore last closed tab.

(defun my-tab-new (arg)
  "Create new tab.
With universal argument move current window into new tab."
  (interactive "P")
  (if arg (tab-window-detach) (tab-new)))

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
  :hook (minibuffer-setup-hook . vertico-repeat-save)
  :config
  (my-keymap-set vertico-map
    "<tab>"     'next-history-element
    "<backtab>" 'previous-history-element
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  (dolist (state '(normal insert))
    (helix-keymap-set vertico-map state
      ;; "M-<return>" 'vertico-exit-input ;; default setting
      "C-p" #'consult-yank-from-kill-ring
      "C-l" #'vertico-insert
      ;; Russian
      "C-о" 'vertico-next
      "C-л" 'vertico-previous))
  (helix-keymap-set vertico-map 'normal
    "g j" 'vertico-next-group
    "g k" 'vertico-previous-group
    "z j" 'vertico-next-group
    "z k" 'vertico-previous-group
    "n"   'vertico-next-group
    "N"   'vertico-previous-group))

(leaf vertico-directory
  :after vertico
  :hook
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy)
  :config
  (dolist (state ('normal insert))
    (helix-keymap-set vertico-map state
      "C-h" 'vertico-directory-up
      ;; "C-h" (lambda ()
      ;;         (cond ((eq 'file (vertico--metadata-get 'category))
      ;;                (call-interactively #'vertico-directory-up))))
      )))

(leaf marginalia
  :elpaca t
  :commands (marginalia-mode marginalia-cycle)
  :global-minor-mode marginalia-mode
  :bind (minibuffer-local-map
         ("M-a" . marginalia-cycle)))

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
         ;; ("M-y" . consult-yank-pop)
         ([remap yank-pop] . consult-yank-pop)
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
      (append (-remove #'(lambda (elt) (eq elt 't))
                       (buffer-local-value hook (current-buffer)))
              (default-value hook))
    ;; else
    (symbol-value hook)))

;;;; treemacs

;; (leaf treemacs-projectile :elpaca t)
;; (leaf treemacs-magit :elpaca t)
;; (leaf lsp-treemacs :elpaca t)

(leaf treemacs
  :elpaca t
  ;; :init
  ;; (treemacs-start-on-boot)
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  ;; :bind
  ;; (("M-0"       . treemacs-select-window)
  ;;  ("C-x t 1"   . treemacs-delete-other-windows)
  ;;  ("C-x t t"   . treemacs)
  ;;  ("C-x t d"   . treemacs-select-directory)
  ;;  ("C-x t B"   . treemacs-bookmark)
  ;;  ("C-x t C-t" . treemacs-find-file)
  ;;  ("C-x t M-t" . treemacs-find-tag))
  :require t
  :config
  (setq treemacs-buffer-name-function #'treemacs-default-buffer-name
        treemacs-buffer-name-prefix " *Treemacs-Buffer-"
        treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay 0.5
        treemacs-directory-name-transformer #'identity
        treemacs-display-in-side-window t
        treemacs-eldoc-display 'simple
        treemacs-file-event-delay 2000
        treemacs-file-extension-regex treemacs-last-period-regex-value
        treemacs-file-follow-delay 0.2
        treemacs-file-name-transformer #'identity
        treemacs-follow-after-init t
        treemacs-expand-after-init t
        treemacs-find-workspace-method 'find-for-file-or-pick-first
        treemacs-git-command-pipe ""
        treemacs-goto-tag-strategy 'refetch-index
        treemacs-header-scroll-indicators '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory t
        treemacs-indentation 2
        treemacs-indentation-string " "
        treemacs-is-never-other-window nil
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        treemacs-move-files-by-mouse-dragging t
        treemacs-move-forward-on-expand nil
        treemacs-no-png-images nil
        treemacs-no-delete-other-windows t
        treemacs-project-follow-cleanup nil
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position 'left
        treemacs-read-string-input 'from-child-frame
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home nil
        treemacs-show-cursor nil
        treemacs-show-hidden-files t
        treemacs-silent-filewatch nil
        treemacs-silent-refresh nil
        treemacs-sorting 'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.5
        treemacs-text-scale nil
        treemacs-user-mode-line-format nil
        treemacs-user-header-line-format nil
        treemacs-wide-toggle-width 70
        treemacs-width 35
        treemacs-width-increment 1
        treemacs-width-is-initially-locked t
        treemacs-workspace-switch-cleanup nil)
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (when (executable-find "git")
    (if treemacs-python-executable
        (treemacs-git-mode 'deferred)
      (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

;; (leaf treemacs-projectile
;;   :elpaca t
;;   :after treemacs projectile)
;; 
;; (leaf treemacs-icons-dired
;;   :elpaca t
;;   :hook (dired-mode-hook . treemacs-icons-dired-enable-once))
;; 
;; (leaf treemacs-magit
;;   :elpaca t
;;   :after treemacs magit)
;; 
;; ;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;; ;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;; ;;   :ensure t
;; ;;   :config (treemacs-set-scope-type 'Perspectives))
;; 
;; (leaf treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
;;   :elpaca t
;;   :after treemacs
;;   :config (treemacs-set-scope-type 'Tabs))


;;; Org-mode
;;;; Variables
;;;;; General settings
(setopt
 ;; The `org-directory' variable must be set before Org loads!
 org-directory (expand-file-name "~/notes/")

 org-modules '(;; ol-doi
               ;; ol-w3m
               ;; ol-bbdb
               ol-bibtex
               ol-docview
               ;; ol-gnus
               ol-info
               ;; ol-irc
               ;; ol-mhe
               ;; ol-rmail
               ;; ol-eww
               )

 org-return-follows-link t
 ;; org-fold-core-style 'overlays
 org-tags-column -80 ; Прижимать тэги к 80 колонке справа.

 ;; org-M-RET-may-split-line '((default . t))
 org-insert-heading-respect-content nil
 org-default-notes-file (file-name-concat org-directory "inbox.org")

 ;; Indentation for the content of a source code block.
 org-edit-src-content-indentation 0
 org-src-preserve-indentation nil

 ;; Use `TAB' language's major-mode binding in code blocks.
 org-src-tab-acts-natively nil

 ;; ;; Follow org links by press Enter with point on it.
 ;; org-return-follows-link t

 ;; ;; Changes to task states might get logged, especially for recurring
 ;; ;; routines. If so, log them in a drawer, not the content of the note.
 ;; org-log-state-notes-into-drawer t

 ;; org-indirect-buffer-display 'current-window
 org-list-allow-alphabetical t
 ;; org-log-into-drawer t

 org-startup-folded 'show2levels ; Initial visibility

 ;; Properties apply also for sublevels.
 org-use-property-inheritance t

 ;; org-log-done 'time ; Track time when tasks were finished.
 org-deadline-warning-days 14
 org-log-redeadline 'note
 org-log-reschedule nil
 org-blank-before-new-entry '((heading . t)
                              (plain-list-item . auto)))

;;;;; appearence

(setq org-startup-indented t
      org-fontify-whole-heading-line t
      org-fontify-quote-and-verse-blocks t
      org-pretty-entities t)

;; Enclose text in "{}" after "_" to make it treated as subscript.
(setq org-use-sub-superscripts '{})

;;;;; org-id

(setq org-id-method 'ts
      org-id-ts-format "%Y%m%dT%H%M%S")

;; How to store link on org-mode outline node:
(setq org-id-link-to-org-use-id 'create-if-interactive)
;; (setq org-id-link-to-org-use-id 'use-existing)

;;;;; org-attach

(setopt
 org-attach-store-link-p 'attached
 org-attach-dir-relative t
 org-attach-id-dir (file-name-concat org-directory "org-attach/")
 org-attach-method 'mv ; move
 ;; org-attach-use-inheritance nil
 org-attach-auto-tag "ATTACH"
 org-attach-preferred-new-method 'id
 org-attach-sync-delete-empty-dir 'query
 org-file-apps '((system . "xdg-open %s")
                 ("\\.pdf\\'" . system)
                 ("\\.djvu?\\'" . system)
                 (directory . system)
                 (auto-mode . emacs)
                 ("\\.x?html?\\'" . default))
 org-attach-id-to-path-function-list '(identity
                                       org-attach-id-uuid-folder-format
                                       org-attach-id-ts-folder-format
                                       org-attach-id-fallback-folder-format))

;; (defun my-org-attach-id-ts-folder-format (id)
;;   "Translate an UUID ID into a folder-path.
;; Default format for how Org translates ID properties to a path for
;; attachments.  Useful if ID is generated with UUID."
;;   (and (< 4 (length id))
;;        (format "%s/%s"
;;                (substring id 0 4)
;;                id
;;                ;; (substring id 4)
;;                )))

;;;;; Capture templates

(setq org-capture-templates
      '(("j" "journal" plain
         (file+olp+datetree +org-capture-journal-file)
         "%?"
         :empty-lines-before 1
         ;; :kill-buffer t
         )))

;;;;; babel

(setopt
 ;; Open source block with `org-edit-special' in the same window.
 org-src-window-setup 'current-window

 ;; Allow babel code execution without confirming it every time.
 org-confirm-babel-evaluate nil

 ;; Available embedded languages for babel.
 org-babel-load-languages '((sql . t)
                            (shell . t)
                            (emacs-lisp . t)
                            (python . t)
                            (plantuml . t))

 ;; Use PlantUML executable instead of `.jar' file together with Java.
 org-plantuml-exec-mode 'plantuml
 org-plantuml-jar-path (expand-file-name "~/.nix-profile/lib/plantuml.jar"))

;;;;; footnotes

(setq org-footnote-define-inline nil
      org-footnote-auto-adjust t)

;;;;; images

(setq org-startup-with-inline-images t
      org-cycle-inline-images-display t
      org-image-actual-width '(300))

;;;;; TODO keywords and Priorities

(setq org-todo-keywords
      '((sequence
         ;; "󰔌" ; SOMEDAY
         "󰒅" ; SOMEDAY
         "󰄱" ; TODO
         "󰡖" ; NEXT
         ;; "󰤌" ; IN PROCESS
         ;; "󱅊" ; IN PROGRESS
         ;; "󱗝" ; IN PROGRESS
         "󰔟" ; WAITING
         "|"
         "󰄵" ; DONE
         "󱈎" ; ARCHIVED
         "󰅘" ; CANCELLED
         )
        (sequence "󰃃" "" "|" "󱍻")
        (sequence "SOMEDAY" "TODO" "NEXT" "IN-PROGRESS" "WAITING" "|"
                  "DONE" "ARCHIVED" "CANCELLED")
        (sequence "TOREAD" "READING" "|" "READ")
        ))

;; Make priority signs be integers from 1 to 5, with 3 as default.
;; Default priorities are: #A, #B, #C, with #B as default.
(setq org-priority-highest ?A
      org-priority-lowest  ?D
      org-priority-default ?C)

;; Consider all nested entries in the subtree for cookies.
;; [[info:org#Breaking Down Tasks]]
(setq org-hierarchical-todo-statistics nil)

;;;;; tags

;; (setq org-use-tag-inheritance nil)
(setq org-tags-exclude-from-inheritance '("project" "main" "index")
      org-tags-match-list-sublevels nil)

;;;; zotero integration

;; Redirect `zotero:' links to the system for handling:
(with-eval-after-load 'org
  (org-link-set-parameters
   "zotero"
   :follow (lambda (zpath)
             (browse-url (format "zotero:%s" zpath)))))

;;;; Org files appearence
;;;;; Prettify symbols mode

;; ("TODO" . "")
;; ("WAIT" . "")
;; ("NOPE" . "")
;; ("DONE" . "")
;; ("[#A]" . "")
;; ("[#B]" . "")
;; ("[#C]" . "")
;; ("[ ]" . "")
;; ("[X]" . "")
;; ("[-]" . "")
;; (":PROPERTIES:" . "")
;; (":END:" . "―")
;; ("#+STARTUP:" . "")
;; ("#+TITLE: " . "")
;; ("#+RESULTS:" . "")
;; ("#+NAME:" . "")
;; ("#+ROAM_TAGS:" . "")
;; ("#+FILETAGS:" . "")
;; ("#+HTML_HEAD:" . "")
;; ("#+SUBTITLE:" . "")
;; ("#+AUTHOR:" . "")
;; (":Effort:" . "")
;; ("SCHEDULED:" . "")
;; ("DEADLINE:" . "")

(defun my-org-prettify-symbols ()
  "Beautify org mode keywords using `prettify-symbols-mode'."
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(("#+begin_src" . "")
                  ("#+end_src" . "―")
                  ("#+begin_example" . "")
                  ("#+end_example" . "")
                  ("#+begin_quote" . "")
                  ("#+end_quote" . "")
                  ;; ("#+begin_quote" . "")
                  ;; ("#+end_quote" . "")
                  ;; ("#+header:" . ?)
                  ;; ("#+name:" . ?﮸)
                  ;; ("#+results:" . ?)
                  ;; ("#+call:" . ?)
                  ;; (":properties:" . ?)
                  ;; (":logbook:" . ?)
                  )))
  (prettify-symbols-mode +1))

;;;;; org-superstar
;; • ◦ ‣ ￭ ■ ⋄ ○ □ ▬ ▶ ▸ ◂ ◆
(leaf org-superstar
  :elpaca t
  :hook (org-mode-hook . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars . nil)
  (org-superstar-headline-bullets-list . '("●"))
  ;; (org-superstar-leading-bullet)
  (org-superstar-item-bullet-alist . '((? . ?◦)
                                       (?- . ?•)
                                       (?* . ?◆))))

;;;;; org-pretty-tags

(leaf org-pretty-tags
  :elpaca t
  :hook (org-mode-hook . org-pretty-tags-mode)
  :custom (org-pretty-tags-surrogate-strings . '(("attach" . "󰁦")
                                                 ("ATTACH" . "󰁦"))))

;;;;; org-appear

(leaf org-appear
  :elpaca t
  :hook (org-mode-hook . org-appear-mode)
  :custom (org-hide-emphasis-markers . t))

;;;; org-auto-tangle

(leaf org-auto-tangle
  :elpaca t
  :hook (org-mode-hook . org-auto-tangle-mode)
  ;; :custom (org-auto-tangle-babel-safelist . '("~/.config/emacs/README.org"))
  )

;;;; org-tempo
;; Org 9.2 introduced a new template expansion mechanism, combining
;; `org-insert-structure-template' bound to z, (default binding C-c C-,).
;; The previous `easy-templates' mechanism (<s Tab) should be enabled manualy.
;; For more information, refer to the commentary section in `org-tempo.el'.
;;
;; Type `<se Tab' to insert emacs-lisp source code block,
;; type `<sh Tab' to insert bash source block and so on.

(leaf org-tempo
  :after org
  :config
  ;; Elements of length one have a tab appended. Elements of length two are
  ;; kept as is. Longer elements are truncated to length two. If an element
  ;; cannot be made unique, an error is raised.
  (my-add-to-list 'org-structure-template-alist
                  '(("se" . "src emacs-lisp")
                    ("sh" . "src sh")
                    ("sc" . "src cpp")
                    ("sf" . "src fennel")
                    ("sl" . "src common-lisp")
                    ("sm" . "src markdown")
                    ;; ("sr" . "src rust")
                    ("sp" . "src python")
                    ("su" . "src lua"))))

;;;; org-ql
(leaf org-ql :elpaca t)

;;;; org-roam

(leaf org-roam
  :elpaca t
  :hook (org-mode-hook . org-roam-db-autosync-mode)
  :custom
  (org-roam-directory . org-directory)
  ;; Provide link completion matching outside of Org links.
  (org-roam-completion-everywhere . t)
  ;; Make org-roam snappier during sqlite database synchronization.
  (org-roam-db-gc-threshold . most-positive-fixnum)
  ;; https://github.com/org-roam/org-roam/issues/576
  (org-roam-graph-executable . "dot")
  (org-roam-mode-section-functions . '(org-roam-backlinks-section
                                       org-roam-reflinks-section
                                       ;; org-roam-unlinked-references-section
                                       ))
  :config
  (add-to-list 'org-default-properties "ROAM_EXCLUDE"))

;; 5 org-roam hacks by System Crafters
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun my-org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my-org-roam-create-untracked-node ()
  "Create Org-Roam node with `ROAM_EXCLUDE' property."
  (interactive)
  (org-id-get-create)
  (org-set-property "ROAM_EXCLUDE" "t"))

;;;;; Node searching interface in completion menu
(setq org-roam-node-display-template
      (format "${title:*} %s %s"
              (propertize "${doom-type:12}" 'face 'font-lock-keyword-face)
              (propertize "${doom-tags:42}" 'face '(:inherit org-tag :box nil))))

;;;;; Capture template using Denote filenaming scheme

(setq org-roam-extract-new-file-path "${slug}.org"
      org-roam-capture-templates
      `(("d" "denote" plain "%?" :target
         (file+head "${slug}.org"
                    ,(concat "#+title:    ${title}\n"
                             "#+filetags: ${my-tags}"))
         :immediate-finish t
         :unnarrowed t)))

(with-eval-after-load 'org-roam
  ;; Custom `${slug}' function
  ;; -------------------------
  ;; The slug function is used to determine the filename to use for a node, based
  ;; on its title. It is calculated in `org-roam-node-slug'. If you wish to change
  ;; the default behaviour of the slug function, you should override the entire
  ;; function.
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (id (org-roam-node-id node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ;; U+0300 COMBINING GRAVE ACCENT
                             769 ;; U+0301 COMBINING ACUTE ACCENT
                             770 ;; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ;; U+0303 COMBINING TILDE
                             772 ;; U+0304 COMBINING MACRON
                             774 ;; U+0306 COMBINING BREVE
                             775 ;; U+0307 COMBINING DOT ABOVE
                             776 ;; U+0308 COMBINING DIAERESIS
                             777 ;; U+0309 COMBINING HOOK ABOVE
                             778 ;; U+030A COMBINING RING ABOVE
                             780 ;; U+030C COMBINING CARON
                             795 ;; U+031B COMBINING HORN
                             803 ;; U+0323 COMBINING DOT BELOW
                             804 ;; U+0324 COMBINING DIAERESIS BELOW
                             805 ;; U+0325 COMBINING RING BELOW
                             807 ;; U+0327 COMBINING CEDILLA
                             813 ;; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ;; U+032E COMBINING BREVE BELOW
                             816 ;; U+0330 COMBINING TILDE BELOW
                             817 ;; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                   (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                   (string-glyph-compose
                    (apply #'string (seq-remove #'nonspacing-mark-p
                                                (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                   (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . " ") ;; convert anything not alphanumeric to spaces
                        ("%s*" . " ") ;; remove sequential spaces
                        ("^-" . "")   ;; remove starting underscore
                        ("-$" . ""))) ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          ;; (downcase slug)
          (concat id "--" slug)))))

  ;; `${my-tags}' function
  (cl-defmethod org-roam-node-my-tags ((node org-roam-node))
    (when-let* ((tags (completing-read-multiple "New note KEYWORDS: "
                                                (org-roam-tag-completions))))
      (concat ":" (mapconcat #'identity tags ":") ":"))))

;;;;; org-roam-dailies

;; Note that for daily files to show up in the calendar, they have to be
;; of format "org-time-string.org".
(leaf org-roam-dailies
  :after org-roam
  :config
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")
           :empty-lines 1))))

;;;;; consult-org-roam

(leaf consult-org-roam
  :elpaca t
  :delight t
  :after org-roam
  :custom
  ;; Use ripgrep for searching with `consult-org-roam-search'.
  (consult-org-roam-grep-func . #'consult-ripgrep)

  ;; Configure a custom narrow key for `consult-buffer'.
  (consult-org-roam-buffer-narrow-key . ?r)

  ;; ;; Display org-roam buffers right after non-org-roam buffers in
  ;; ;; `consult-buffer' (and not down at the bottom).
  ;; (consult-org-roam-buffer-after-buffers . t)
  :config
  (consult-org-roam-mode))

;;;;; org-roam-ui

(leaf org-roam-ui
  :elpaca t
  :delight t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme . t)
  (org-roam-ui-follow . t)
  (org-roam-ui-update-on-save . t)
  (org-roam-ui-follow-mode . t)
  (org-roam-ui-open-on-start . nil))

;;;;; org-roam-ql

(leaf org-roam-ql
  :elpaca t
  :after org-roam)

;;;;; DISABLED org-roam-tags

;; (leaf org-roam-tags
;;   :elpaca (org-roam-tags :host github :repo "simoninireland/org-roam-tags")
;;   (helix-keymap-set org-mode-map 'normal
;;     "" #'org-roam-tags-tag-note          ; "C-c C-g"
;;     "" #'org-roam-tags-tag-note-at-point ; "C-c g"
;;     "" #'org-roam-tags-open-tag))        ; "C-c M-g"

;;;;; nursery

(leaf org-roam-dblocks
  :elpaca (nursery :host github :repo "chrisbarrett/nursery")
  :hook (org-mode-hook . org-roam-dblocks-autoupdate-mode)
  :config
  (defalias 'org-roam-insert-notes-dblock #'org-insert-dblock:notes)
  (defalias 'org-roam-insert-backlinks-dblock #'org-insert-dblock:backlinks))

;; (use-package org-roam-lazy-previews
;;   :after org-roam
;;   :demand t)

;;;; org-bookmarks

(leaf org-bookmarks
  :elpaca t
  :after org
  :commands (org-bookmarks)
  :init
  (setq org-bookmarks-file (file-name-concat org-directory "bookmarks.org")
        org-bookmarks-add-org-capture-template t
        org-bookmarks-display-screenshot t)
  (org-bookmarks-add-org-capture-template))

;;;; org-journal

(leaf org-journal
  :elpaca t
  :custom
  ;; When switching from daily to weekly, monthly, yearly, or from weekly,
  ;; monthly, yearly to daily, you need to invalidate the cache. This has
  ;; currently to be done manually by calling `org-journal-invalidate-cache'.
  (org-journal-file-type . 'monthly)
  (org-extend-today-until . 4)
  (org-journal-date-format . "%x, %A")) ;; "DATE, WEEKDAY"

;;;; org-web-tools

(leaf org-web-tools
  :elpaca t
  ;; :after org
  :commands (org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries
             org-web-tools-archive-attach
             org-web-tools-archive-view))

;;;; scrolling over images

(leaf org-sliced-images
  :elpaca t
  :global-minor-mode org-sliced-images-mode)

;;;; LaTeX previews

(leaf org-fragtog
  :elpaca t
  :after org
  :hook (org-mode-hook . org-fragtog-mode)
  :init
  (setq org-startup-with-latex-preview t
        org-format-latex-options (-> org-format-latex-options
                                     (plist-put :scale 0.8)
                                     ;; (plist-put :foreground 'auto)
                                     ;; (plist-put :background 'auto)
                                     )))

;;; Extra facilities
;;;; which-key

(leaf which-key
  :global-minor-mode which-key-mode
  :custom ((which-key-lighter . nil)
           (which-key-idle-delay . 1.5)
           (which-key-idle-secondary-delay . 0.25)
           (which-key-add-column-padding . 1)
           (which-key-max-description-length . 40)))

;;;; avy

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
  ;; Hide the modeline of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind (("C-v" . embark-act)  ;; scroll-up-command
         ("M-v" . embark-dwim) ;; scroll-down-command
         (embark-symbol-map
          ("h" . helpful-symbol))
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

;;;; outline

;; Wrapper around `outline'
(leaf outli
  :elpaca (outli :host github :repo "jdtsmith/outli")
  :after helix
  :delight
  outline-mode
  (outline-minor-mode nil t)
  ;; :hook (emacs-lisp-mode-hook . outli-mode)
  ;; :custom
  ;; Use <tab> and S-<tab> to cycle while point is on the button overlay.
  ;; (outline-minor-mode-use-buttons . t)
  :setq
  (outline-level . #'my-lisp-outline-level)
  :bind
  (outline-overlay-button-map
   ("<tab>" . outline-cycle)
   ("<backtab>" . outline-cycle-buffer))
  :defer-config
  (helix-keymap-set outline-minor-mode-map 'normal
    "z <tab>"     'outline-cycle
    "z <backtab>" 'outline-cycle-buffer
    "z c" 'outline-hide-subtree
    "z C" 'outline-hide-body
    "z o" 'outline-show-entry
    "z O" 'outline-show-branches
    "z a" 'outline-toggle-children
    "z m" 'outline-hide-sublevels
    "z 2" (cons "Outline hide up to 2 sublevels"
                (lambda () (interactive) (outline-hide-sublevels 2)))
    "z r" 'outline-show-all
    "m o" 'outline-mark-subtree
    "z p" '("Outline path" . outline-hide-other)
    ;; "z >" 'outline-promote
    ;; "z <" 'outline-demote
    "z <return>" (cons "Outline insert heading"
                       (lambda ()
                         (interactive)
                         (outline-insert-heading)
                         (hydra-outline/body)))
    ;;; Jump over headings
    "z u" 'outline-up-heading
    "z j" (cons "Outline next visible heading"
                (lambda (count)
                  (interactive "p")
                  (helix-maybe-deactivate-mark)
                  (outline-next-visible-heading count)
                  (hydra-outline/body)))
    "z k" (cons "Outline previous visible heading"
                (lambda (count)
                  (interactive "p")
                  (helix-maybe-deactivate-mark)
                  (outline-previous-visible-heading count)
                  (hydra-outline/body)))
    "z C-j" '("Outline forward same level" . hydra-outline/outline-forward-same-level)
    "z C-k" '("Outline backward same level" . hydra-outline/outline-backward-same-level)
    ;;; Move headings
    "z M-j" '("Outline move subtree down" . hydra-outline/outline-move-subtree-down)
    "z M-k" '("Outline move subtree up" . hydra-outline/outline-move-subtree-up)
    "z M-h" '("Outline promote" . hydra-outline/outline-promote)
    "z M-l" '("Outline demote" . hydra-outline/outline-demote))

  (defhydra hydra-outline (:hint nil)
    "
Jump: _C-j_, _C-k_  Move: _M-h_, _M-j_, _M-k_, _M-l_
"
    ("C-j" outline-forward-same-level)
    ("C-k" outline-backward-same-level)
    ("M-j" outline-move-subtree-down)
    ("M-k" outline-move-subtree-up)
    ("M-h" outline-promote)
    ("M-l" outline-demote)
    ("<tab>" outline-cycle)
    ("C-u" helix-smooth-scroll-up :color blue)
    ;; Scrolling
    ;; ("C-b" helix-smooth-scroll-page-up)
    ;; ("C-f" helix-smooth-scroll-page-down)
    ;; ("C-d" helix-smooth-scroll-down)
    ;; ("C-u" helix-smooth-scroll-up)
    ;; ("C-e" helix-mix-scroll-line-down)
    ;; ("C-y" helix-mix-scroll-line-up)
    ;; ("z z" helix-smooth-scroll-line-not-to-very-top)
    ;; ("z t" helix-smooth-scroll-line-to-top)
    ;; ("z b" helix-smooth-scroll-line-to-bottom)
    )

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

;;;; Dired
;;;;; dired

(leaf dired
  :after helix
  :custom
  ;; -l               :: use a long listing format
  ;; -a, --all        :: do not ignore entries starting with `.'
  ;; -A, --almost-all :: do not list implied `.' and `..'
  ;; -F, --classify   :: append indicator (one of /=>@|) to entries
  ;; -v               :: natural sort of (version) numbers within text
  (dired-listing-switches . "-lAhF -v --group-directories-first")
  ;; (dired-listing-switches . "-l --human-readable --group-directories-first")
  (dired-dwim-target . t)
  (dired-auto-revert-buffer . #'dired-buffer-stale-p)
  (dired-kill-when-opening-new-dired-buffer . t)
  (delete-by-moving-to-trash . t)
  (dired-recursive-deletes . 'always) ;; 'top
  (dired-recursive-copies . 'always)
  (dired-no-confirm . t)
  ;; Ask whether destination dirs should get created when copying/removing files.
  (dired-create-destination-dirs . 'ask)
  (dired-maybe-use-globstar . t)
  :hook
  (dired-mode-hook . dired-hide-details-mode)
  (dired-mode-hook . hl-line-mode)
  :config
  (helix-keymap-set dired-mode-map 'motion
    "h" 'dired-up-directory
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    ;; "l" 'dired-open-file ;; from `dired-hacks' package
    "J" 'dired-goto-file
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    "?" 'casual-dired-tmenu
    ))

(leaf dired-x
  :custom (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`[.].+")
  :hook (dired-mode-hook . dired-omit-mode))

(leaf diredc :elpaca t)

;;;;; My dired commands

(defalias 'my-dired-copy-file-name #'dired-copy-filename-as-kill)

(defun my-dired-copy-file-path ()
  "Copy full path to the file into kill ring."
  (interactive)
  (dired-copy-filename-as-kill 0))

(defun dired-do-flagged-delete-permanently ()
  "Delete files permanently instead of trashing them"
  (declare (interactive-only t))
  (interactive nil dired-mode)
  ;; (interactive)
  (let ((delete-by-moving-to-trash nil))
    (dired-do-flagged-delete)))

(defalias 'dired-delete-permanently #'dired-do-flagged-delete-permanently)

(defun my-dired-toggle-omit-mode ()
  "Toggle `dired-omit-mode' not only in current buffer, but in general."
  (interactive)
  (if dired-omit-mode
      (progn
        (dired-omit-mode -1)
        (remove-hook 'dired-mode-hook #'dired-omit-mode))
    (dired-omit-mode +1)
    (add-hook 'dired-mode-hook #'dired-omit-mode)))

;;;;; image-dired

;; Use Thumbnail Managing Standard
(setq image-dired-thumbnail-storage 'standard)  ; 128x128
;; (setq image-dired-thumbnail-storage 'standard-large)  ; 256x256
;; (setq image-dired-thumbnail-storage 'standard-x-large) ; 512x512

(setq image-dired-marking-shows-next nil)

;; TODO: xdg-open doesn't worked
(setq image-dired-external-viewer "qimgv")

;; image-dired-dired-display-image
;; dired-open-file

;;;; ibuffer

(leaf ibuffer
  :custom
  (ibuffer-expert . t) ; Do not ask for confirmation to delete the unmodified buffer.
  (ibuffer-truncate-lines . t)
  (ibuffer-show-empty-filter-groups . nil) ; Don't show emtpy filter groups
  (ibuffer-display-summary . nil)
  ;; (ibuffer-default-sorting-mode . 'filename/process) ; recency alphabetic major-mode
  ;; (ibuffer-read-only-char . "%")
  ;; (ibuffer-modified-char . "*")
  ;; (ibuffer-marked-char . ">")
  ;; (ibuffer-locked-char . "L")
  ;; (ibuffer-deletion-char . "D")
  (ibuffer-eliding-string . "…")
  :hook
  (ibuffer-mode-hook . ibuffer-auto-mode)    ; automatically update ibuffer
  (ibuffer-mode-hook . hl-line-mode)
  ;; (ibuffer-hook . (lambda ()
  ;;                   ;; (hl-line-mode +1)
  ;;                   (unless hl-line-mode (hl-line-mode +1))))
  :config
  (cl-pushnew #'helpful-mode ibuffer-help-buffer-modes)
  ;; (ibuffer-switch-to-saved-filter-groups "home")

  ;; Custom columns
  ;; icons column
  (define-ibuffer-column icon
    (:name "  ")
    (let ((icon (if (and (buffer-file-name) (nerd-icons-auto-mode-match?))
                    (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name))
                                              :v-adjust -0.05)
                  (nerd-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  ;; Human readable size column
  (define-ibuffer-column size
    (:name "Size"
           :inline t
           :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;;;; layout

(let ((path (if (require 'ibuffer-projectile nil t)
                'my/project-relative-filename-or-process
              'filename-and-process)))
  (setq ibuffer-formats
        `((mark modified read-only locked
           " " (icon 2 2 :left :elide)
           ,(propertize " " 'display `(space :align-to 8))
           (name 26 -1)
           "  " ,path)
          (mark modified read-only locked
                " " (icon 2 2 :left :elide)
                ,(propertize " " 'display `(space :align-to 8))
                (name 30 30 :left :elide)
                " " (size 6 -1 :right)
                " " (mode 16 16 :left :elide)
                ;; ,@(when (require 'ibuffer-vc nil t)
                ;;     '(" " (vc-status 12 :left)))
                " " ,path))))

;;;;; ibuffer-vc

;; (leaf ibuffer-vc
;;   :elpaca t
;;   :hook (ibuffer-hook . (lambda ()
;;                           (ibuffer-vc-set-filter-groups-by-vc-root)
;;                           (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                             (ibuffer-do-sort-by-alphabetic))))
;;   ;; :congig
;;   ;; ibuffer-vc-generate-filter-groups-by-vc-root
;;   )
;; 
;; ;; (setq ibuffer-filter-groups)
;; 
;; (defun my-ibuffer-vc-root (buf)
;;   "Return a cons cell (backend-name . root-dir) for BUF.
;; If the file is not under version control, nil is returned instead."
;;   (when-let* ((file-name (funcall ibuffer-vc-buffer-file-name-function buf))
;;               ((ibuffer-vc--include-file-p file-name))
;;               (backend (ibuffer-vc--deduce-backend file-name)))
;;     (let* ((root-fn-name (intern (format "vc-%s-root" (downcase (symbol-name backend)))))
;;            (root-dir (if (fboundp root-fn-name) ;; git, svn, hg, bzr (at least)
;;                          (funcall root-fn-name file-name)
;;                        (pcase backend
;;                          ((and (or 'darcs 'DARCS)
;;                                ;; `vc-darcs' is an external package
;;                                (guard (fboundp 'vc-darcs-find-root)))
;;                           (vc-darcs-find-root file-name))
;;                          ((or 'cvs 'CVS) (vc-find-root file-name "CVS"))
;;                          ((or 'rcs 'RCS) (or (vc-find-root file-name "RCS")
;;                                              (concat file-name ",v")))
;;                          ((or 'sccs 'SCCS) (or (vc-find-root file-name "SCCS")
;;                                                (concat "s." file-name)))
;;                          ((or 'src 'SRC) (or (vc-find-root file-name ".src")
;;                                              (concat file-name ",v")))
;;                          (_ (error "ibuffer-vc: unknown vc backend '%s'" backend))))))
;;       (cons backend root-dir))))
;; 
;; (defun my-ibuffer-generate-filter-groups ()
;;   "Set Ibuffer filter groups."
;;   (->> (buffer-list)
;;        (mapcar #'my-ibuffer-vc-root)
;;        (delq nil)
;;        (-uniq)
;;        (mapcar #'(lambda (vc-root)
;;                    (list (format "%s: %s" (car vc-root) (cdr vc-root))
;;                          `(vc-root . ,vc-root))))))
;; 
;; (defun my-ibuffer-set-filter-groups ()
;;   "Set Ibuffer filter groups."
;;   (setq ibuffer-filter-groups (ibuffer-vc-generate-filter-groups-by-vc-root))
;;   (when-let ((ibuf (get-buffer "*Ibuffer*")))
;;     (with-current-buffer ibuf
;;       (pop-to-buffer ibuf)
;;       (ibuffer-update nil t))))

;;;;; ibuffer-projectile

;; (->> (buffer-list)
;;      (mapcar #'ibuffer-projectile-root)
;;      (delq nil)
;;      (seq-uniq)
;;      (mapcar (lambda (root)
;;                (cons (funcall ibuffer-projectile-group-name-function (car root) (cdr root))
;;                      `((projectile-root . ,root))))))

;; (leaf ibuffer-projectile
;;   :elpaca t
;;   ;; :after ibuffer
;;   :hook (ibuffer-hook . ibuffer-projectile-set-filter-groups)
;;   :config
;;   (setq ibuffer-projectile-prefix (concat (nerd-icons-octicon
;;                                            "nf-oct-file_directory"
;;                                            :face ibuffer-filter-group-name-face
;;                                            :v-adjust -0.05)
;;                                           " "))
;;   ;; Render filnames relative to project root
;;   (define-ibuffer-column my/project-relative-filename-or-process
;;     (:name "Filename/Process"
;;      :header-mouse-map ibuffer-filename/process-header-map
;;      :summarizer
;;      (lambda (strings)
;;        (setq strings (delete "" strings))
;;        (let ((procs (--count (get-text-property 1 'ibuffer-process it)
;;                              strings))
;;              (files (length strings)))
;;          (concat (pcase files
;;                    (0 "No files")
;;                    (1 "1 file")
;;                    (_ (format "%d files" files)))
;;                  ", "
;;                  (pcase files
;;                    (0 "no processes")
;;                    (1 "1 process")
;;                    (_ (format "%d processes" procs)))))))
;;     (let ((filename (ibuffer-make-column-filename buffer mark)))
;;       (if-let* ((proc (get-buffer-process buffer)))
;;           (concat (propertize (format "(%s %s)" proc (process-status proc))
;;                               'font-lock-face 'italic
;;                               'ibuffer-process proc)
;;                   (if (< 0 (length filename))
;;                       (format " %s" filename)
;;                     ""))
;;         ;; else
;;         (if-let* ((root-dir (cdr (ibuffer-projectile-root buffer))))
;;             (file-relative-name filename root-dir)
;;           (abbreviate-file-name filename))))))

;;;;; narrow to indirect buffer

(keymap-global-set "<remap> <narrow-to-region>" #'my-narrow-buffer-indirectly)
(keymap-global-set "<remap> <widen>" #'my-widen-indirectly-narrowed-buffer)

(defvar my--narrowed-base-buffer nil)

;;;###autoload
(defun my-narrow-buffer-indirectly (beg end)
  "Restrict editing in this buffer to the current region, indirectly.

This recursively creates indirect clones of the current buffer so that the
narrowing doesn't affect other windows displaying the same buffer. Call
`my-widen-indirectly-narrowed-buffer' to undo it (incrementally).

Inspired from http://demonastery.org/2013/04/emacs-evil-narrow-region/"
  (interactive "r")
  (when (use-region-p)
    (helix-carry-linewise-selection)
    (deactivate-mark)
    (let ((orig-buffer (current-buffer)))
      (with-current-buffer (switch-to-buffer (clone-indirect-buffer nil nil))
        (narrow-to-region beg end)
        (setq-local my--narrowed-base-buffer orig-buffer)))))

;;;###autoload
(defun my-widen-indirectly-narrowed-buffer (&optional arg)
  "Widens narrowed buffers.
This command will incrementally kill indirect buffers (under the assumption they
were created by `my-narrow-buffer-indirectly') and switch to their base buffer.

If ARG is non-nil, then kill all indirect buffers, return the base buffer and
widen it.

If the current buffer is not an indirect buffer, it is `widen'ed."
  (interactive "P")
  (unless (buffer-narrowed-p)
    (user-error "Buffer isn't narrowed"))
  (let ((orig-buffer (current-buffer))
        (base-buffer my--narrowed-base-buffer))
    (cond ((or (not base-buffer)
               (not (buffer-live-p base-buffer)))
           (widen))
          (arg
           (let ((buffer orig-buffer)
                 (buffers-to-kill (list orig-buffer)))
             (while (setq buffer (buffer-local-value 'my--narrowed-base-buffer buffer))
               (push buffer buffers-to-kill))
             (switch-to-buffer (buffer-base-buffer))
             (mapc #'kill-buffer (remove (current-buffer) buffers-to-kill))))
          ((switch-to-buffer base-buffer)
           (kill-buffer orig-buffer)))))

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
  (deactivate-mark)
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
  (helix-keymap-global-set 'normal
    "<backspace>" 'execute-extended-command
    "g o"   'exchange-point-and-mark
    "C-M-;" 'eval-expression ;; default M-; but in Helix it reverse region
    "M-o"   'pop-to-mark-command
    "C-S-o" 'pop-global-mark
    "C-w n" 'other-window-prefix
    "g a"   'describe-char
    "z SPC" 'cycle-spacing
    "z ."   'set-fill-prefix)
  (my-keymap-set global-map
    "C-x C-b" 'ibuffer-jump ;; list-buffers
    "C-x C-r" 'recentf-open ;; find-file-read-only
    "C-x C-d" 'dired-jump)  ;; list-directory
  ;; <leader> key
  (my-keymap-set mode-specific-map
    ;; "f x" 'xref-find-apropos
    "f f" 'find-file
    "f F" 'default/find-file-under-here
    "f d" 'dired
    "f l" 'locate
    "f r" '("Recent files" . recentf-open)
    "f R" 'projectile-recentf
    ;; "f u" '("Sudo this file" . doom/sudo-this-file)
    ;; "f U" '("Sudo find file" . doom/sudo-find-file)
    ;; "f x" '("Open scratch buffer" . doom/open-scratch-buffer)
    ;; "f X" '("Switch to scratch buffer" . doom/switch-to-scratch-buffer)
    )

  (my-keymap-set helix-window-map
   "b" '("Clone buffer" . clone-indirect-buffer)
   "B" '("Clone buffer other window" . clone-indirect-buffer-other-window)))

;; (leaf keypad
;;   :load-path "~/code/emacs/helix"
;;   :require t
;;   :config
;;   ;; (helix-keymap-global-set 'normal
;;   ;;   "SPC" #'keypad
;;   ;;   "C-h k" #'keypad-describe-key)
;;   )

;;;; Info-mode

(leaf info
  :after helix
  :config
  (helix-set-initial-state 'Info-mode 'normal)
  (helix-keymap-set Info-mode-map 'normal
    "C-j" #'Info-next
    "C-k" #'Info-prev
    "z j" #'Info-forward-node
    "z k" #'Info-backward-node
    "z u" #'Info-up
    "z d" #'Info-directory
    "z m" #'Info-menu
    "g m" #'Info-menu
    "M-h" #'Info-help
    ))

(provide 'post-init)
;;; post-init.el ends here
