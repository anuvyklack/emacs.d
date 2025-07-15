;;; post-init.el --- -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;;-----------------------
;; Start emacs from command line with `--debug-init' key, or uncomment:
;; (setq debug-on-error t
;;       debug-on-quit t)
;;-----------------------

(require 'xdg)

(elpaca dash
  (require 'dash))

(elpaca leaf
  (require 'leaf))

(elpaca leaf-keywords
  (require 'leaf-keywords)
  ;; ;; Use :ensure keyword instead of :elpaca
  ;; (setopt '(leaf-alias-keyword-alist '((:ensure . :elpaca))))
  (leaf-keywords-init))

(elpaca-wait) ;; Block until current Elpaca queue is processed.

(leaf f :elpaca t :require t)
(leaf s :elpaca t :require t)
(leaf nerd-icons :elpaca t :require t)
(leaf blackout :elpaca t)
(leaf casual :elpaca t)
(leaf hydra :elpaca t)

(leaf transient
  :elpaca t
  :custom
  ;; Pop up transient windows at the bottom of the current window instead of
  ;; entire frame. This is more ergonomic for users with large displays or many
  ;; splits.
  (transient-display-buffer-action . '(display-buffer-below-selected
                                       (dedicated . t)
                                       (inhibit-same-window . t)))
  (transient-show-during-minibuffer-read . t)
  :defer-config
  ;; Close transient with ESC
  (define-key transient-map [escape] #'transient-quit-one))

;; ;; Profiler
;; (leaf esup :elpaca t)

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

(defun my-disable-hl-line-mode ()
  "Disable in current buffer the highlighting of current line."
  (setq-local global-hl-line-mode nil)
  (global-hl-line-unhighlight))

;;; Appearance

(add-hook 'prog-mode-hook #'blink-cursor-mode)

(global-hl-line-mode)
(add-hook 'text-mode-hook #'my-disable-hl-line-mode)

(add-hook 'prog-mode-hook
          #'(lambda ()
              (setq-local
               global-hl-line-mode nil ;; Disable current line highlighting.
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

;;;; Line numbers

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

;;;; Colorize strings that represent colors

(leaf rainbow-mode
  :elpaca t
  :blackout t
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
  :blackout eldoc-mode
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
  (after-init-hook . save-place-mode)
  ;; Show (line,column) indicator in modeline.
  (after-init-hook . line-number-mode)
  (after-init-hook . column-number-mode)
  :bind
  ([remap dabbrev-expand] . hippie-expand))

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
  :after helix
  :require helpful
  :hook
  (helpful-mode-hook . outline-minor-mode)
  (helpful-mode-hook . my-disable-hl-line-mode)
  (help-mode-hook . my-disable-hl-line-mode)
  :custom
  (help-window-select . t)
  :bind `(([remap describe-function] . helpful-callable)
          ([remap describe-variable] . helpful-variable)
          ([remap describe-command] . helpful-command)
          ([remap describe-key] . helpful-key)
          ([remap describe-symbol] . helpful-symbol))
  :config
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
    "C-c" nil)) ;; unbind `describe-copying'

;; (setq helpful-switch-buffer-function
;;       (lambda (buffer)
;;         (let ((display-buffer-alist
;;                (cons '(t . (display-buffer-reuse-mode-window
;;                             (mode . helpful-mode)
;;                             (body-function . select-window)))
;;                      display-buffer-alist)))
;;           (pop-to-buffer buffer))))
;;
;; (setq helpful-switch-buffer-function
;;       (lambda (buffer)
;;         (if-let ((window (display-buffer-reuse-mode-window buffer '((mode . helpful-mode)))))
;;             (select-window window)
;;           (pop-to-buffer buffer))))

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
  (helix-keymap-set helix-window-map
    "<tab>"   '("New tab" . my-tab-new)
    "C-<tab>" 'other-tab-prefix
    "m" 'other-tab-prefix ;; next to `n' which is `other-window-prefix'
    ;; "u" '("Winner undo" . tab-bar-history-back)
    ;; "U" '("Winner redo" . tab-bar-history-forward)
    "t" (cons "tab-bar"
              (define-keymap
                ;; "t" 'my-tab-new
                "t" 'tab-duplicate
                "d" 'tab-duplicate
                "n" 'other-tab-prefix
                "g" 'tab-group ;; Add current tab to group.
                ">" 'tab-bar-move-tab
                "<" 'tab-bar-move-tab-backward
                "r" 'tab-rename
                "u" 'tab-undo ;; Restore last closed tab.
                "c" 'tab-close
                "o" 'tab-close-other ;; Close all other tabs.
                "w" 'tab-window-detach
                "F" 'tab-detach)))
  ;; for `repeat-mode'
  (put 'tab-next     'repeat-map nil)
  (put 'tab-previous 'repeat-map nil))

(defun my-tab-new (arg)
  "Create new tab. With `universal argument' detach current window into new tab."
  (interactive "P")
  (if arg (tab-window-detach) (tab-new)))

;;; Minibuffer & Completion

(setopt completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)

;; Allow opening new minibuffers from inside existing minibuffers.
(setopt enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode +1)

;;;; orderless

(leaf orderless
  :elpaca t
  :custom
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  ;; note that despite override in the name orderless can still be used in
  ;; find-file etc.
  (completion-category-overrides . '((file (styles orderless partial-completion))))
  ;; (orderless-component-separator . #'orderless-escapable-split-on-space)
  :config

  ;; Default values
  (setq orderless-affix-dispatch-alist '((?% . char-fold-to-regexp)
                                         (?! . orderless-not)
                                         (?& . orderless-annotation)
                                         (?, . orderless-initialism)
                                         (?= . orderless-literal)
                                         (?^ . orderless-literal-prefix)
                                         (?~ . orderless-flex))
        orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; ;; Doom values
  ;; (setq orderless-affix-dispatch-alist '((?% . char-fold-to-regexp)
  ;;                                        (?! . orderless-without-literal)
  ;;                                        (?& . orderless-annotation)
  ;;                                        (?` . orderless-initialism)
  ;;                                        (?= . orderless-literal)
  ;;                                        (?^ . orderless-literal-prefix)
  ;;                                        (?~ . orderless-flex))
  ;;       orderless-style-dispatchers '(+vertico-orderless-dispatch
  ;;                                     +vertico-orderless-disambiguation-dispatch))
  )

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
  (dolist (state '(normal insert))
    (helix-keymap-set vertico-map :state state
      ;; "M-<return>" 'vertico-exit-input ;; default setting
      "C-p" #'consult-yank-from-kill-ring
      ;; Russian
      "C-о" 'vertico-next
      "C-л" 'vertico-previous))
  ;; (helix-keymap-set vertico-map :state 'normal
  ;;   "z j" 'vertico-next-group
  ;;   "z k" 'vertico-previous-group
  ;;   "n"   'vertico-next-group
  ;;   "N"   'vertico-previous-group)
  )

(leaf vertico-directory
  :after vertico helix
  :hook
  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

(leaf marginalia
  :elpaca t
  :commands (marginalia-mode marginalia-cycle)
  :global-minor-mode marginalia-mode
  :bind (minibuffer-local-map
         ("M-a" . marginalia-cycle)))

;;;; consult

(leaf consult
  :elpaca t
  :hook
  ;; Enable automatic preview at point in the *Completions* buffer.
  (completion-list-mode-hook . consult-preview-at-point-mode)
  :bind
  (;; ("C-/" . consult-line)       ;; #'undo
   ;; ("C-?" . consult-line-multi) ;; #'undo-redo
   ;; C-c bindings are in `mode-specific-map'
   (("C-c M-x" . consult-mode-command)
    ("C-c <backspace>" . consult-mode-command)
    ("C-c H" . consult-history)
    ("C-c K" . consult-kmacro)
    ("C-c M" . consult-man)
    ("C-c I" . consult-info))
   ;; C-x bindings are in `ctl-x-map'
   (("C-x b"   . consult-buffer)
    ("C-x 4 b" . consult-buffer-other-window)
    ("C-x 5 b" . consult-buffer-other-frame)
    ("C-x t b" . consult-buffer-other-tab)
    ("C-x r b" . consult-bookmark))
   ;; "C-x p" bindings are in `project-prefix-map'
   ("C-x p b" . consult-project-buffer)
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   ("C-M-#" . consult-register)
   ;; M-g bindings in `goto-map'
   (("M-g e" . consult-compile-error)
    ("M-g f" . consult-flymake)
    ("M-g g" . consult-goto-line)
    ("M-g M-g" . consult-goto-line)
    ("M-g o" . consult-outline)
    ("M-g m" . consult-mark)
    ("M-g k" . consult-global-mark)
    ("M-g i" . consult-imenu)
    ("M-g I" . consult-imenu-multi))
   ;; `search-map' is binded to `M-s' prefix by default
   (search-map
    :package emacs
    ("f" . consult-find)
    ("l" . consult-locate)
    ("g" . consult-grep)
    ("G" . consult-git-grep)
    ("r" . consult-ripgrep)
    ("/" . consult-ripgrep)
    ("k" . consult-keep-lines)
    ("u" . consult-focus-lines))
   ;; Minibuffer history
   (minibuffer-local-map
    :package emacs
    ("C-s" . consult-history)
    ("C-r" . consult-history))
   ;; Other custom bindings
   ;; ("M-y" . consult-yank-pop)
   ([remap repeat-complex-command]        . consult-complex-command)
   ([remap recentf-open]                  . consult-recent-file)
   ([remap recentf-open-files]            . consult-recent-file)
   ([remap bookmark-jump]                 . consult-bookmark)
   ([remap goto-line]                     . consult-goto-line)
   ([remap imenu]                         . consult-imenu)
   ([remap Info-search]                   . consult-info)
   ([remap load-theme]                    . consult-theme)
   ([remap switch-to-buffer]              . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
   ([remap yank-pop]                      . consult-yank-pop)
   ;; ([remap locate]                        . consult-locate)
   )
  :config
  (setopt consult-narrow-key "<")
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
  (setq consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  (setq consult-fd-args '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
                          "--color=never"
                          ;; https://github.com/sharkdp/fd/issues/839
                          "--full-path --absolute-path"
                          "--hidden --exclude .git"))

  ;; Configure the register formatting. This improves the register.
  (setopt register-preview-delay 0.5
          register-preview-function #'consult-register-format)
  ;; Tweak the register preview window.
  (advice-add 'register-preview :override #'consult-register-window)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)))

;;;; corfu

(leaf corfu
  :elpaca t
  :after helix
  :require t
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
  (text-mode-ispell-word-completion . nil)
  :config
  ;; (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (helix-keymap-set corfu-map
    ;; Expands the common prefix of all candidates, or insert selected one.
    "C-l" 'corfu-insert-separator
    "M-l" 'corfu-complete
    "C-i" 'corfu-info-documentation
    "C-h" 'corfu-info-documentation
    "C-d" 'corfu-info-location
    "<tab>"     'corfu-next
    "<backtab>" 'corfu-previous))

(leaf corfu-history
  :hook (corfu-mode-hook . corfu-history-mode)
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(leaf corfu-popupinfo
  :after helix
  :hook (corfu-mode-hook . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay . '(0.5 . 0.5))
  ;; (corfu-popupinfo-delay . nil)
  :config
  (helix-keymap-set corfu-popupinfo-map
    "M-h"   'corfu-popupinfo-toggle
    "C-S-b" 'corfu-popupinfo-scroll-down
    "C-S-f" 'corfu-popupinfo-scroll-up))

(leaf nerd-icons-corfu
  :elpaca t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(defun my-corfu-move-to-minibuffer ()
  "Move list of candidates to your choice of minibuffer completion UI."
  (interactive)
  (pcase completion-in-region--data
    (`(,beg ,end ,table ,pred ,extras)
     (let ((completion-extra-properties extras)
           (completion-cycle-threshold nil)
           (completion-cycling nil))
       (consult-completion-in-region beg end table pred)))))

;;;; cape

(leaf cape
  :elpaca t
  :init
  (with-eval-after-load 'helix
    (helix-keymap-global-set :state 'insert
      ;; Emulate Vim's omni-completion keybinds
      "C-x" #'cape-prefix-map))
  :bind
  (cape-prefix-map
   ("C-o" . completion-at-point) ;; C-x C-o is Vim's omni-completion keybinding
   ;; ("C-e" . cape-elisp-block)
   ;; ("C-s" . cape-elisp-symbol)
   ("/" . cape-tex)
   ("C-/" . cape-tex)
   ("C-h" . cape-history)
   ("C-l" . cape-line)
   ("C-k" . cape-keyword)
   ("C-f" . cape-file)
   ("C-t" . complete-tag)
   ("C-w" . cape-dict)
   ("C-r" . cape-rfc1345)
   ;; ("s"   . cape-dict)
   ;; ("C-s" . yasnippet-capf)
   ("C-a" . cape-abbrev)
   ("C-d" . cape-dabbrev)
   ("C-n" . cape-dabbrev)
   ;; ("C-p" . +corfu/dabbrev-this-buffer)
   )
  :config
  ;; Add to the global default value of `completion-at-point-functions'
  ;; which is used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history))

;;; Search
;;;; deadgrep

(leaf deadgrep
  :elpaca t
  :hook
  ;; (deadgrep-mode-hook . next-error-follow-minor-mode)
  (deadgrep-edit-mode-hook . my-disable-hl-line-mode)
  :init
  (keymap-set search-map "s" 'deadgrep))

;;;; wgrep

(leaf wgrep :elpaca t)

;;; IDE
;;;; project.el

(setopt project-vc-extra-root-markers '(".projectile" ".project"))

(leaf consult-project-extra
  :elpaca t
  :after helix
  :config
  (helix-keymap-set project-prefix-map
    "p" 'consult-project-extra-find
    "P" 'consult-project-extra-find-other-window))

;;;; DISABLED projectile

;; (leaf projectile
;;   :elpaca t
;;   :require t
;;   :global-minor-mode projectile-mode
;;   :config
;;   (setopt consult-project-function #'(lambda (_) (projectile-project-root)))
;;   ;; (setopt projectile-project-search-path (list (file-truename "~"))
;;   ;;         projectile-auto-discover t)
;;   )
;;
;; (leaf consult-projectile
;;   :elpaca t
;;   :after projectile
;;   :bind (([remap projectile-switch-to-buffer] . consult-projectile-switch-to-buffer)
;;          ([remap projectile-find-dir] . consult-projectile-find-dir)
;;          ([remap projectile-find-file] . consult-projectile-find-file)
;;          ([remap projectile-recentf] . consult-projectile-recentf)
;;          ;; ([remap projectile-switch-project] . consult-projectile-switch-project)
;;          ([remap projectile-switch-project] . consult-projectile)))
;;
;; ;; The multiview includes initially buffers, files and known projects.  To include
;; ;; recent files and directires add `consult-projectile--source-projectile-dir' and/or
;; ;; `consult-projectile--source-projectile-recentf' to `consult-projectile-sources'.

;;;; xref (goto definition)

(leaf xref
  :custom
  ((xref-search-program  . 'ripgrep) ;; or 'ugrep
   (xref-auto-jump-to-first-definition . 'show)
   (xref-prompt-for-identifier . nil)
   (xref-history-storage . #'xref-window-local-history)
   ;; (xref-show-definitions-function . #'xref-show-definitions-buffer-at-bottom)
   ;; (xref-show-definitions-function . #'xref-show-definitions-buffer)
   ;; (xref-show-xrefs-function . #'xref--show-xref-buffer)
   (xref-show-xrefs-function . #'consult-xref)
   (xref-show-definitions-function . #'consult-xref))
  :config
  (advice-add 'xref-find-definitions :around #'my-xref-try-all-backends-a)
  (advice-add 'xref-find-references  :around #'my-xref-try-all-backends-a))

(leaf dumb-jump
  :elpaca t
  :after xref
  :hook (xref-backend-functions . dumb-jump-xref-activate))

(leaf nerd-icons-xref
  :elpaca t
  :after xref
  :global-minor-mode nerd-icons-xref-mode)

(defun my-xref-try-all-backends-a (orig-fun &rest args)
  "Try all `xref-backend-functions' in row until first succeed."
  (let* ((jumped nil)
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

;;;; imenu

(leaf imenu-list
  :elpaca t)

;;;; diff-hl

;; diff-hl-command-map
;; diff-hl-mode-map
(leaf diff-hl
  :elpaca t
  :commands (diff-hl-stage-current-hunk diff-hl-revert-hunk diff-hl-next-hunk diff-hl-previous-hunk)
  :global-minor-mode global-diff-hl-mode
  :hook ((vc-dir-mode-hook . turn-on-diff-hl-mode)
         (diff-hl-mode-hook . diff-hl-flydiff-mode)))

;;;; treemacs

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
  ;; :require t
  :defer-config
  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-directory-name-transformer #'identity
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
        treemacs-is-never-other-window t
        treemacs-max-git-entries 5000
        treemacs-missing-project-action 'ask
        treemacs-move-forward-on-expand nil
        treemacs-no-png-images nil
        treemacs-no-delete-other-windows t
        treemacs-project-follow-cleanup nil
        treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-read-string-input 'from-child-frame
        treemacs-recenter-distance 0.1
        treemacs-recenter-after-file-follow nil
        treemacs-recenter-after-tag-follow nil
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-litter-directories '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home nil
        treemacs-show-cursor nil
        treemacs-show-hidden-files t ;; toggled with `treemacs-toggle-show-dotfiles'
        treemacs-silent-filewatch nil
        treemacs-silent-refresh nil
        treemacs-sorting 'alphabetic-asc ;; 'alphabetic-case-insensitive-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes t
        treemacs-tag-follow-cleanup t
        treemacs-tag-follow-delay 1.5
        treemacs-user-mode-line-format nil
        treemacs-user-header-line-format nil
        treemacs-wide-toggle-width 70
        treemacs-width 35
        treemacs-width-increment 1
        treemacs-width-is-initially-locked t
        treemacs-workspace-switch-cleanup nil
        treemacs-elisp-imenu-expression nil) ;; use default value
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)
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

;; (leaf treemacs-icons-dired
;;   :elpaca t
;;   :hook (dired-mode-hook . treemacs-icons-dired-enable-once))

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
 org-tags-column -80 ;; Прижимать тэги к 80 колонке справа.

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

 org-startup-folded 'show2levels ;; Initial visibility

 ;; Properties apply also for sublevels.
 org-use-property-inheritance t

 ;; org-log-done 'time ; Track time when tasks were finished.
 org-deadline-warning-days 14
 org-log-redeadline 'note
 org-log-reschedule nil
 org-blank-before-new-entry '((heading . t)
                              (plain-list-item . auto))

 org-special-ctrl-a/e t)

;;;;; appearence

(setq org-startup-indented t)
(with-eval-after-load 'org-indent
  (blackout 'org-indent-mode)) ;; Hide `org-indent-mode' from modeline.

(setq org-fontify-whole-heading-line t
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
  :blackout t
  :after org
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
  :blackout t
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

(leaf consult-org-roam
  :elpaca t
  :blackout t
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

;;;;; my org-roam commands

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

;;;;; org-roam-ui

(leaf org-roam-ui
  :elpaca t
  :blackout (org-roam-ui-mode
             org-roam-ui-follow-mode)
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
;;   (helix-keymap-set org-mode-map :state 'normal
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
  (org-bookmarks-add-to-org-capture-templates))

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
  :after org
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
;;;; elpaca

(leaf elpaca
  :after helix
  :config
  (helix-set-initial-state 'elpaca-info-mode 'normal)
  (keymap-set elpaca-ui-mode-map "/" #'elpaca-ui-search))

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
  :config
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
  :after helix
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :custom
  (which-key-use-C-h-commands . nil)
  (prefix-help-command . 'embark-prefix-help-command)
  :config
  ;; Hide the modeline of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Keybindings
  :init
  (helix-keymap-set global-map
    "C-<return>" 'embark-act
    "C-v"   'embark-act  ;; scroll-up-command
    "M-v"   'embark-dwim ;; scroll-down-command
    "C-<m>" 'embark-act
    "M-m"   'embark-dwim)
  (helix-keymap-set minibuffer-local-map
    "C-c C-c"   'embark-export
    "C-c C-v"   'embark-collect
    "C-c C-<m>" 'embark-collect
    "C-c b"     'embark-become)
  :config
  (helix-keymap-set embark-general-map
    "C-v"   'embark-select
    ;; "m"     'mark
    )
  (helix-keymap-set embark-region-map
    "F" 'fill-region-as-paragraph
    "w" 'whitespace-cleanup-region
    "n" 'helix-narrow-to-region-indirectly)
  (helix-keymap-set embark-heading-map
    "m" 'outline-mark-subtree)
  ;; Unbind keys that I will never use with Helix so as not to clutter up menus.
  ;; I use `helix-keymap-set' :state because it actually removes keybindings
  ;; from keymap, while `:bind' only binds them to nil.
  (helix-keymap-set embark-general-map
    "C-SPC" nil  ;; mark
    "DEL"   nil  ;; delete-region
    "w"     nil) ;; embark-copy-as-kill
  (helix-keymap-set embark-region-map
    "W" nil) ;; write-region
  (helix-keymap-set embark-heading-map
    "C-SPC" nil)) ;; outline-mark-subtree

(leaf embark-consult
  :elpaca t
  :after consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

;;;; outline

;; Wrapper around `outline'
(leaf outli
  :elpaca (outli :host github :repo "jdtsmith/outli")
  :after helix
  :blackout (outline-mode outline-minor-mode)
  ;; :custom
  ;; Use <tab> and S-<tab> to cycle while point is on the button overlay.
  ;; (outline-minor-mode-use-buttons . t)
  :setq
  (outline-level . #'my-lisp-outline-level)
  :defer-config
  (helix-define-advice outline-up-heading (:before (&rest _) push-mark)
    (helix-push-point))

  ;; Keybindings
  (helix-keymap-set outline-overlay-button-map
    "<tab>"     'outline-cycle
    "<backtab>" 'outline-cycle-buffer)
  (helix-keymap-set outline-minor-mode-map :state 'normal
    ;; Jump over headings
    "z u"   '("Outline up deading" . hydra-outline/helix-outline-up-heading)
    "z j"   '("Outline next visible heading" . hydra-outline/outline-next-visible-heading)
    "z k"   '("Outline previous visible heading" . hydra-outline/outline-previous-visible-heading)
    "z C-j" '("Outline forward same level" . hydra-outline/outline-forward-same-level)
    "z C-k" '("Outline backward same level" . hydra-outline/outline-backward-same-level)
    ;; Move headings
    "z M-j" '("Outline move subtree down" . hydra-outline/outline-move-subtree-down)
    "z M-k" '("Outline move subtree up" . hydra-outline/outline-move-subtree-up)
    "z M-h" '("Outline promote" . hydra-outline/outline-promote)
    "z M-l" '("Outline demote" . hydra-outline/outline-demote)))

(defhydra hydra-outline ( :hint nil
                          :pre (helix-maybe-deactivate-mark))
  "
Jump: _C-j_, _C-k_  Move: _M-h_, _M-j_, _M-k_, _M-l_ Cycle: _<tab>_
"
  ("<tab>" outline-cycle)
  ("u"   helix-outline-up-heading)
  ("j"   outline-next-visible-heading)
  ("k"   outline-previous-visible-heading)
  ("C-j" outline-forward-same-level)
  ("C-k" outline-backward-same-level)
  ("M-j" outline-move-subtree-down)
  ("M-k" outline-move-subtree-up)
  ("M-h" outline-promote)
  ("M-l" outline-demote)
  ("C-u" helix-smooth-scroll-up))

(defun my-lisp-outline-level ()
  "Return outline level for comment at point.
Replacement for `lisp-outline-level'."
  (if (match-beginning 1)
      (- (match-end 1)
         (match-beginning 1))
    0))

;; (leaf 'foldout
;;   :config
;;   (helix-keymap-set outline-mode-map :state 'normal
;;     "z n" #'foldout-zoom-subtree
;;     "z w" #'foldout-exit-fold)
;;   (helix-keymap-set outline-minor-mode-map :state 'normal
;;     "z n" #'foldout-zoom-subtree
;;     "z w" #'foldout-exit-fold))


;;;; Dired
;;;;; dired

(leaf dired
  :after helix
  :commands (dired dired-jump)
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
  :config
  (put 'dired-jump 'repeat-map nil))

(leaf dired-x
  :custom (dired-omit-files . "\\`[.]?#\\|\\`[.][.]?\\'\\|\\`[.].+")
  :hook (dired-mode-hook . dired-omit-mode))

(leaf diredfl
  :elpaca t
  :after dired
  :hook (dired-mode-hook . diredfl-mode))

(leaf dired-narrow  :elpaca t :after dired)
(leaf dired-subtree :elpaca t :after dired)

(leaf dired-copy-paste
  :elpaca (dired-copy-paste :host github :repo "jsilve24/dired-copy-paste")
  :after dired
  :commands (dired-copy-paste-do-copy
             dired-copy-paste-do-cut
             dired-copy-paste-do-paste))

(leaf dired-du
  :elpaca t
  :commands dired-du-mode
  :custom (dired-du-size-format . t))

(leaf wdired
  :after helix
  :custom
  (wdired-use-dired-vertical-movement . t)
  (wdired-allow-to-change-permissions . t) ; or 'advanced
  :defer-config
  (advice-add 'wdired-change-to-wdired-mode :after #'my-disable-hl-line-mode)
  ;; Re-enable current line highlighting on switching back to `dired-mode'.
  (advice-add 'wdired-change-to-dired-mode :after
              (lambda () (setq-local global-hl-line-mode t)))
  (helix-keymap-set wdired-mode-map :state 'normal
    ;; "C-x C-s" 'wdired-finish-edit
    "Z Z" 'wdired-finish-edit
    "Z Q" 'wdired-abort-changes
    "<escape>" 'wdired-exit))

;;;;; dired filter

(leaf dired-filter
  :elpaca t
  :config
  (setopt dired-filter-group-saved-groups
          '(("default"
             ("Directories"
              (directory))
             ("Archives"
              (extension "zip" "rar" "gz" "bz2" "tar"))
             ("Pictures"
              (or (extension "jfif" "JPG")
                  (mode . 'image-mode)))
             ("Videos"
              (extension "mp4" "mkv" "flv" "mpg" "avi" "webm"))
             ;; ("LaTeX"
             ;;  (extension "tex" "bib"))
             ;; ("Org"
             ;;  (extension . "org"))
             ("PDF"
              (extension . "pdf"))))))

(defun my-dired-filter-group-mode ()
  "Toggle `dired-filter-group-mode' in all buffers."
  (interactive)
  (if dired-filter-group-mode
      (progn
        (dired-filter-group-mode -1)
        (remove-hook 'dired-mode-hook #'dired-filter-group-mode))
    (dired-filter-group-mode +1)
    (add-hook 'dired-mode-hook #'dired-filter-group-mode)))

;;;;; diredc — Midnight Commander

(leaf diredc
  :elpaca t
  ;; :require t
  )

;;;;; dired keybindings

(with-eval-after-load 'dired
  (my-keymap-set dired-mode-map
    "h" 'dired-up-directory
    "j" 'dired-next-line
    "k" 'dired-previous-line
    "l" 'dired-find-file
    ;; "l" 'dired-open-file ;; from `dired-hacks' package

    "/" 'dired-goto-file
    "i" 'dired-toggle-read-only ;; wdired
    "K" 'dired-do-kill-lines
    "r" 'dired-do-redisplay
    "?" 'casual-dired-tmenu

    "!" 'dired-do-shell-command
    "&" 'dired-do-async-shell-command

    "x" 'dired-do-flagged-delete
    "X" 'dired-do-flagged-delete-permanently

    "C-c u" 'dired-undo
    ;; "C-c a" 'org-attach-dired-to-subtree

    "s" 'dired-sort-toggle-or-edit
    "S" 'casual-dired-sort-by-tmenu

    "o"   'dired-do-open
    "RET" 'dired-do-open
    "C-w <return>" 'dired-find-file-other-window
    "w"   'dired-display-file

    "("   'dired-hide-details-mode
    ")"   'my-dired-toggle-omit-mode
    "z ," 'dired-hide-details-mode
    "z ." 'my-dired-toggle-omit-mode

    "z d" 'dired-hide-details-mode
    "z i" 'dired-hide-details-mode

    "<" 'dired-prev-marked-file
    ">" 'dired-next-marked-file

    "p" 'dired-copy-paste-do-paste
    "y" (cons "yank"
              (define-keymap
                "y" 'dired-copy-paste-do-copy
                "d" 'dired-copy-paste-do-cut
                "n" 'my-dired-copy-file-name
                "p" 'my-dired-copy-file-path
                "c" 'dired-do-copy
                "l" 'dired-do-symlink
                "L" 'dired-do-relsymlink
                "h" 'dired-do-hardlink
                "x" 'dired-do-shell-command
                "X" 'dired-do-async-shell-command))
    "g" (define-keymap
          "RET" 'dired-do-open
          "o" 'dired-find-file-other-window
          "O" 'dired-view-file
          "x" 'browse-url-of-dired-file

          "d" 'dired-do-delete
          "c" 'dired-do-compress-to
          "z" 'dired-do-compress
          "l" 'dired-do-load
          "s" 'dired-do-shell-command
          "S" 'dired-do-async-shell-command

          "u" 'dired-upcase
          "U" 'dired-downcase
          "A" 'dired-show-file-type

          "r" 'dired-do-find-regexp
          "b" 'dired-do-byte-compile
          "i" 'dired-do-info
          "m" 'dired-do-man)
    "c" (cons "change"
              (define-keymap
                "d" 'my-dired-add-denote-id
                "n" 'dired-do-rename
                "r" 'dired-do-rename
                "g" 'dired-do-chgrp
                "o" 'dired-do-chown
                "m" 'dired-do-chmod
                "t" 'dired-do-touch
                ;; "c" 'dired-do-compress-to
                ;; "z" 'dired-do-compress
                ;; "r" 'dired-do-find-regexp-and-replace
                "/" 'dired-do-find-regexp-and-replace
                "x" 'dired-do-shell-command
                "X" 'dired-do-async-shell-command))

    "I" 'dired-maybe-insert-subdir

    ;; Commands for marking and unmarking.
    "m" 'dired-mark
    "U" 'dired-unmark-all-marks
    "*" (cons "mark"
              (define-keymap
                "%"   'dired-mark-subdir-files ;; mark all
                "."   'dired-mark-extension
                "*"   'dired-mark-executables
                "d"   'dired-mark-directories
                "l"   'dired-mark-symlinks
                "h"   'dired-mark-omitted ;; mark hidden files
                ";"   'dired-mark-sexp
                "u"   'dired-unmark-all-files
                "n"   'dired-number-of-marked-files
                "c"   'dired-change-marks
                "DEL" 'dired-unmark-backward

                "~"   'dired-toggle-marks
                "t"   'dired-toggle-marks

                ;; mark files by regexp
                "/"   'dired-mark-files-regexp
                "f"   'dired-mark-files-regexp
                "m"   'dired-mark-files-regexp
                "r"   'dired-mark-files-regexp))
    ;; All regexp commands share a `%' prefix
    "%" (cons "regexp commands"
              (define-keymap
                "/" 'dired-mark-files-regexp
                "f" 'dired-mark-files-regexp
                "m" 'dired-mark-files-regexp

                "F" 'dired-mark-files-containing-regexp
                "d" 'dired-flag-files-regexp

                "g" 'dired-flag-garbage-files
                "&" 'dired-flag-garbage-files

                "r" 'dired-do-rename-regexp
                "c" 'dired-do-copy-regexp
                "l" 'dired-do-symlink-regexp
                "L" 'dired-do-relsymlink-regexp
                "h" 'dired-do-hardlink-regexp))
    ;; dired narrow
    "n"   'dired-narrow-fuzzy
    "N"   'dired-narrow-regexp
    "z n" 'dired-narrow-fuzzy
    "z N" 'dired-narrow-regexp
    ;; dired-subtree
    "<tab>"     'dired-subtree-toggle
    "<backtab>" 'dired-subtree-cycle
    "z j" 'dired-subtree-down
    "z k" 'dired-subtree-up
    "z u" 'dired-subtree-up
    "C-j" 'dired-subtree-next-sibling
    "C-k" 'dired-subtree-previous-sibling))

;; (with-eval-after-load 'wdired
;;   (helix-keymap-set wdired-mode-map :state 'normal
;;     ;; "("   'dired-hide-details-mode
;;     ;; "RET" 'my-wdired-toggle-bit
;;     ;; "ESC" 'wdired-exit
;;     ))

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

;;;;; denote-dired

(defconst denote-id-format "%Y%m%dT%H%M%S")
(defconst denote-id-regexp "\\([0-9]\\{8\\}\\)\\(T[0-9]\\{6\\}\\)")

(defun my-dired-add-denote-id ()
  "Add denote timestamp in front of the files name, unless it's already there."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (dolist (file (dired-get-marked-files))
    (unless (my-get-file-denote-id file)
      (let ((filename (file-name-nondirectory file)))
        (cond
         ;; Files from Reddit app on android. They have timestamp in their name,
         ;; like this: RDT_20220820_0858002573777192519160821.jpg
         ((string-match "^RDT_\\([0-9]\\{8\\}\\)_\\([0-9]\\{6\\}\\)" filename)
          (let* ((date (match-string-no-properties 1 filename))
                 (time (match-string-no-properties 2 filename))
                 (extension (file-name-extension file))
                 (newname (format "%sT%s.%s" date time extension)))
            (rename-file file newname)))
         (t
          (let* ((id (my-create-file-denote-id file))
                 (newname (format "%s--%s" id filename)))
            (rename-file file newname)))))))
  (dired-revert))

(defun my-get-file-creation-time (filepath)
  "Get the creation time of FILEPATH using the `stat' from coreutils."
  (-> (format "stat --format=%%w %s" (shell-quote-argument filepath))
      (shell-command-to-string)
      (string-trim)
      (parse-time-string)
      (encode-time)))

(defun my-get-file-modification-time (filepath)
  (file-attribute-modification-time (file-attributes filepath)))

(defun my-get-file-denote-id (filepath)
  "Get file denote ID or `nil'"
  (let ((filename (file-name-nondirectory filepath)))
    (if (string-match (concat "\\`" denote-id-regexp) filename)
        (match-string-no-properties 0 filename))))

(defun my-create-file-denote-id (filepath)
  "Generate denote ID based on file creation time"
  (let* ((created (my-get-file-creation-time filepath))
         (modified (file-attribute-modification-time (file-attributes filepath)))
         (time (if (time-less-p created modified)
                   created modified)))
    (format-time-string denote-id-format time)))

;;;;; image-dired

;; image-dired-dired-display-image
;; dired-open-file
(leaf image-dired
  :after dired
  :custom
  ;; TODO: xdg-open doesn't worked
  (image-dired-external-viewer . "qimgv")
  (image-dired-marking-shows-next . nil)

  ;; Use Thumbnail Managing Standard
  (image-dired-thumbnail-storage . 'standard)  ; 128x128
  ;; (image-dired-thumbnail-storage . 'standard-large)  ; 256x256
  ;; (image-dired-thumbnail-storage . 'standard-x-large) ; 512x512
  :bind
  (image-dired-thumbnail-mode-map
   ("RET" . image-dired-display-this)
   ("o" . image-dired-thumbnail-display-external)
   ("m" . image-dired-mark-thumb-original-file)
   ("u" . image-dired-unmark-thumb-original-file)
   ("d" . image-dired-flag-thumb-original-file)
   ("K" . image-dired-delete-char)
   ("n" . image-dired-display-next)
   ("p" . image-dired-display-previous)))

;;;; Ibuffer

(leaf ibuffer
  :custom
  (ibuffer-expert . t) ; Do not ask for confirmation to delete the unmodified buffer.
  (ibuffer-truncate-lines . t)
  (ibuffer-show-empty-filter-groups . nil) ; Don't show emtpy filter groups
  (ibuffer-display-summary . nil)
  (ibuffer-movement-cycle . nil)
  (ibuffer-old-time . 2) ;; hours
  (ibuffer-directory-abbrev-alist . `((,abbreviated-home-dir . "~/")))
  ;; (ibuffer-default-sorting-mode . 'filename/process) ; recency alphabetic major-mode
  ;; (ibuffer-read-only-char . "%")
  ;; (ibuffer-modified-char . "*")
  ;; (ibuffer-marked-char . ">")
  ;; (ibuffer-locked-char . "L")
  ;; (ibuffer-deletion-char . "D")
  (ibuffer-eliding-string . "…")
  :hook
  (ibuffer-mode-hook . ibuffer-auto-mode) ;; automatically update ibuffer
  :config
  (cl-pushnew #'helpful-mode ibuffer-help-buffer-modes)
  ;; (ibuffer-switch-to-saved-filter-groups "home")
  )

;;;;; ibuffer appearance

(with-eval-after-load 'ibuffer
  (setq ibuffer-formats
        (let ((path (cond ((require 'ibuffer-vc nil t)
                           'my-vc-root-relative-filename-or-process)
                          ((require 'ibuffer-projectile nil t)
                           'my-project-relative-filename-or-process)
                          (t 'filename-and-process))))
          (list `( mark modified read-only locked
                   " " (icon 2 2 :left :elide)
                   ,(propertize " " 'display `(space :align-to 8))
                   (name 26 -1)
                   "  " ,path)
                `( mark modified read-only locked
                   " " (icon 2 2 :left :elide)
                   ,(propertize " " 'display `(space :align-to 8))
                   (name 30 30 :left :elide)
                   " " (size 6 -1 :right)
                   " " (mode 16 16 :left :elide)
                   ;; ,@(when (require 'ibuffer-vc nil t)
                   ;;     '(" " (vc-status 12 :left)))
                   " " ,path))))

  ;; Custom columns:
  ;; ---------------

  ;; Icons column
  (define-ibuffer-column icon
    ( :name "  ")
    (let ((icon (if (and (buffer-file-name) (nerd-icons-auto-mode-match?))
                    (nerd-icons-icon-for-file (file-name-nondirectory (buffer-file-name)) :v-adjust -0.05)
                  (nerd-icons-icon-for-mode major-mode :v-adjust -0.05))))
      (if (symbolp icon)
          (setq icon (nerd-icons-faicon "nf-fa-file_o" :face 'nerd-icons-dsilver :height 0.8 :v-adjust 0.0))
        icon)))

  ;; Human readable size column
  (define-ibuffer-column size
    ( :name "Size"
      :inline t
      :header-mouse-map ibuffer-size-header-map)
    (file-size-human-readable (buffer-size))))

;;;;; ibuffer keybindings

(with-eval-after-load 'ibuffer
  (my-keymap-set ibuffer-mode-map
    "C-c f f" 'ibuffer-find-file

    ;; Inherit hjkl keys from `special-mode-map'.
    "h" nil
    "j" nil ;; ibuffer-jump-to-buffer
    "k" nil ;; ibuffer-do-kill-lines
    "l" nil ;; ibuffer-redisplay

    "j" 'ibuffer-forward-line
    "k" 'ibuffer-backward-line

    ">" 'ibuffer-forward-next-marked
    "<" 'ibuffer-backwards-next-marked

    "d"   'ibuffer-mark-for-delete
    "M-d" 'ibuffer-mark-for-delete-backwards
    "x"   'ibuffer-do-kill-on-deletion-marks

    "K" 'ibuffer-do-kill-lines
    "f" `("filter" . ,ibuffer--filter-map)
    "/" 'ibuffer-jump-to-buffer ;; ibuffer--filter-map

    "g" (define-keymap
          "r" 'ibuffer-do-revert
          "R" 'ibuffer-redisplay
          "d" 'ibuffer-do-delete
          "s" 'ibuffer-do-save
          "z" 'ibuffer-bury-buffer)

    "X" 'ibuffer-bury-buffer
    "R" 'ibuffer-do-replace-regexp
    ;; "A"   #'ibuffer-do-view-horizontally

    "v"     'ibuffer-do-view-horizontally
    "C-x v" 'ibuffer-do-view

    ;; Filter groups
    "TAB" 'ibuffer-toggle-filter-group
    "z f" 'ibuffer-jump-to-filter-group
    "C-j" 'ibuffer-forward-filter-group
    "C-k" 'ibuffer-backward-filter-group
    "] ]" 'ibuffer-forward-filter-group
    "[ [" 'ibuffer-backward-filter-group
    "z j" 'ibuffer-forward-filter-group
    "z k" 'ibuffer-backward-filter-group
    "z u" 'ibuffer-backward-filter-group)

  (my-keymap-set ibuffer--filter-map
    "y" 'ibuffer-yank
    "q" 'ibuffer-filter-disable
    "Q" 'ibuffer-clear-filter-groups))

;;;;; ibuffer-vc

(leaf ibuffer-vc
  ;; :elpaca t
  :elpaca (ibuffer-vc :repo "~/.config/emacs/modules")
  :hook
  (ibuffer-hook . (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic))))
  :config
  ;; Render filenames relative to project root
  (define-ibuffer-column my-vc-root-relative-filename-or-process
    ( :name "Filename/Process"
      :header-mouse-map ibuffer-filename/process-header-map
      :summarizer (lambda (strings)
                    (setq strings (delete "" strings))
                    (let ((procs (--count (get-text-property 1 'ibuffer-process it)
                                          strings))
                          (files (length strings)))
                      (concat (pcase files
                                (0 "No files")
                                (1 "1 file")
                                (_ (format "%d files" files)))
                              ", "
                              (pcase files
                                (0 "no processes")
                                (1 "1 process")
                                (_ (format "%d processes" procs)))))))
    (let ((filename (ibuffer-make-column-filename buffer mark))
          proc root-dir)
      (cond ((setq proc (get-buffer-process buffer))
             (concat (propertize (format "(%s %s)" proc (process-status proc))
                                 'font-lock-face 'italic
                                 'ibuffer-process proc)
                     (if (length> filename 0)
                         (format " %s" filename)
                       "")))
            ((setq root-dir (cdr (ibuffer-vc-root buffer)))
             (file-relative-name filename root-dir))
            (t (abbreviate-file-name filename))))))

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
;;   (define-ibuffer-column my-project-relative-filename-or-process
;;     ( :name "Filename/Process"
;;       :header-mouse-map ibuffer-filename/process-header-map
;;       :summarizer
;;       (lambda (strings)
;;         (setq strings (delete "" strings))
;;         (let ((procs (--count (get-text-property 1 'ibuffer-process it)
;;                               strings))
;;               (files (length strings)))
;;           (concat (pcase files
;;                     (0 "No files")
;;                     (1 "1 file")
;;                     (_ (format "%d files" files)))
;;                   ", "
;;                   (pcase files
;;                     (0 "no processes")
;;                     (1 "1 process")
;;                     (_ (format "%d processes" procs)))))))
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

;;;; tramp

(setopt tramp-default-method "ssh") ;; faster than the default "scp"


;;;; visualize whitespaces

;; Enable with `whitespace-mode'
(leaf whitespace
  :custom
  ;; (whitespace-line-column . nil)
  ;; (whitespace-style . '( face indentation tabs tab-mark spaces space-mark
  ;;                        newline newline-mark trailing lines-tail))
  (whitespace-display-mappings . '((tab-mark ?\t [?› ?\t])
                                   (newline-mark ?\n [?¬ ?\n])
                                   (space-mark ?\  [?·] [?.]))))

;;;; magit

(leaf magit
  :elpaca t
  ;; :global-minor-mode magit-auto-revert-mode
  :custom
  ;; (magit-display-buffer-function . #'my/magit-display-buffer-fn)
  (magit-diff-refine-hunk . 'all)
  ;; hide ^M characters at the end of a line in diffs
  (magit-diff-hide-trailing-cr-characters . t)
  ;; Don't autosave repo buffers. This is too magical, and saving can
  ;; trigger a bunch of unwanted side-effects, like save hooks and
  ;; formatters. Trust the user to know what they're doing.
  (magit-save-repository-buffers . nil)
  ;; Don't display parent/related refs in commit buffers; they are rarely
  ;; helpful and only add to runtime costs.
  (magit-revision-insert-related-refs . nil)
  ;; If two projects have the same project name (e.g. A/src and B/src will
  ;; both resolve to the name "src"), Magit will treat them as the same
  ;; project and destructively hijack each other's magit buffers. This is
  ;; especially problematic if you use workspaces and have magit open in
  ;; each, and the two projects happen to have the same name! By unsetting
  ;; `magit-uniquify-buffer-names', magit uses the project's full path as
  ;; its name, preventing such naming collisions.
  (magit-uniquify-buffer-names . nil)
  :hook
  ;; Turn ref links into clickable buttons.
  (magit-process-mode-hook . goto-address-mode)
  ;; Reveal the point if in an invisible region.
  (magit-diff-visit-file-hook . (lambda ()
                                  (if (derived-mode-p 'org-mode)
                                      (org-reveal '(4))
                                    (require 'reveal)
                                    (reveal-post-command))))
  :config
  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash")))

(leaf forge
  :elpaca t)

(leaf ghub
  :elpaca t)

(leaf code-review
  :elpaca (code-review :host github :repo "doomelpa/code-review"))

(leaf browse-at-remote
  :elpaca t)

(leaf git-timemachine
  :elpaca t)

(leaf git-modes
  :elpaca t)

(leaf vc
  :custom
  ;; Remove RCS, CVS, SCCS, and Bzr, because it's a lot less work for vc to
  ;; check them all (especially in TRAMP buffers), and who uses any of these?
  (vc-handled-backends . '(Git Hg SVN SRC))
  ;; PERF: Ignore node_modules (expensive for vc ops to index).
  `(vc-ignore-dir-regexp . ,(format "%s\\|%s"
                                    locate-dominating-stop-dir-regexp
                                    "[/\\\\]node_modules")))

(leaf vc-annotate
  :commands vc-annotate
  :config
  (keymap-set vc-annotate-mode-map "<remap> <quit-window>" #'kill-current-buffer))

(leaf smerge-mode)

;; (define-advice magit-display-buffer-traditional
;;       (:around (orig-fun buffer) open-status-buffer-in-current-window)
;;     "Show `magit-status' buffer in current window."
;;     (if (with-current-buffer buffer
;;           (equal major-mode 'magit-status-mode))
;;         (display-buffer buffer '(display-buffer-same-window))
;;       ;; else
;;       (funcall orig-fun buffer)))

(defun my/magit-display-buffer-fn (buffer)
  "The same as `magit-display-buffer-traditional', except:
- Open status buffer in the same window;
- Magit process windows are always opened in small windows below the current. "
  (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
    (display-buffer
     buffer (cond ((eq buffer-mode 'magit-status-mode)
                   '(display-buffer-same-window))
                  ((eq buffer-mode 'magit-process-mode)
                   '(display-buffer-below-selected
                     (window-height . (truncate (* (window-height) 0.35)))))
                  ((and (derived-mode-p 'magit-mode)
                        (not (memq buffer-mode
                                   '(magit-process-mode
                                     magit-revision-mode
                                     magit-diff-mode
                                     magit-stash-mode))))
                   '(display-buffer-same-window))
                  ('(+magit--display-buffer-in-direction))))))

;;;;; magit keybindings



;;;; separedit

(leaf separedit
  :elpaca t
  :after helix
  :custom
  ;; Default major-mode for edit buffer can also be other mode e.g. ‘org-mode’.
  (separedit-default-mode . 'markdown-mode)
  (separedit-preserve-string-indentation . t)
  (separedit-continue-fill-column . t)
  (separedit-write-file-when-execute-save . t)
  (separedit-remove-trailing-spaces-in-comment . t)
  :config
  ;; Key binding for modes you want edit or simply bind ‘global-map’ for all.
  (helix-keymap-global-set :state 'normal
    "z '" 'separedit)
  ;; (dolist (keymap (list prog-mode-map
  ;;                       minibuffer-local-map
  ;;                       help-mode-map
  ;;                       helpful-mode-map))
  ;;   (helix-keymap-set keymap :state 'normal
  ;;     "z '" 'separedit))
  ;; (with-eval-after-load 'obsidian
  ;;   (helix-keymap-set obsidian-mode-map :state 'normal
  ;;     "z '" 'separedit))
  :defer-config
  (helix-keymap-set edit-indirect-mode-map :state 'normal
    "Z Z" 'edit-indirect-commit
    "Z Q" 'edit-indirect-abort)
  (helix-keymap-set separedit-mode-map
    "<remap> <edit-indirect-commit>" 'separedit-commit
    "<remap> <edit-indirect-abort>"  'separedit-abort
    "<remap> <save-buffer>"          'separedit-save))

;;; Major-modes
;;;; Emacs Lisp (Elisp)

(leaf elisp-mode
  :after helix
  ;; :custom
  ;; (pp-default-function . #'pp-emacs-lisp-code)
  :hook
  (lisp-data-mode-hook . helix-paredit-mode)
  (emacs-lisp-mode-hook
   . (lambda ()
       (setq-local tab-width 8)
       ;; Minor modes
       ;; -----------
       ;; Order matters because `outline-minor-mode' and `helix-paredit-mode'
       ;; both binds `C-j' and `C-k'. I want `helix-paredit-mode' bindings
       ;; overlap `outline-minor-mode' bindings.
       (outli-mode 1)
       (helix-paredit-mode 1)))
  :config
  (helix-keymap-set emacs-lisp-mode-map :state 'normal
    "M" '("Documentation" . helpful-at-point)
    "g q" 'prog-fill-reindent-defun)
  (my-keymap-set emacs-lisp-mode-map
    "C-c e" (cons "eval elisp"
                  (define-keymap
                    "e" 'pp-eval-last-sexp
                    ;; "e" 'elisp-eval-region-or-buffer ;; C-c C-e
                    "r" 'eval-region
                    "b" 'eval-buffer
                    "f" 'eval-defun
                    ;; "m" 'macrostep-expand
                    "m" 'emacs-lisp-macroexpand
                    "p" 'pp-macroexpand-last-sexp)))

  (helix-keymap-set lisp-data-mode-map :state 'normal
    "M" '("Documentation" . helpful-at-point))

  ;; ;; Treat `-' char as part of the word on `w', `e', `b', motions.
  ;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  ;; (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)

  (with-eval-after-load 'consult-imenu
    (setf (alist-get 'emacs-lisp-mode consult-imenu-config)
          '( :toplevel "Functions"
             :types ((?h "Headings"  font-lock-constant-face)
                     (?f "Functions" font-lock-function-name-face)
                     (?v "Variables" font-lock-variable-name-face)
                     (?t "Types"     font-lock-type-face)
                     (?l "Leaf"      font-lock-keyword-face))))))

(leaf elisp-demos
  :elpaca t
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;;; xref integration for Elisp

;; elisp-refs-function
;; elisp-refs-macro
;; elisp-refs-symbol
;; elisp-refs-special
;; elisp-refs-variable
(leaf elisp-refs
  :elpaca t
  :after helix
  :config
  (my-keymap-set elisp-refs-mode-map
    "C-j" 'elisp-refs-next-match
    "C-k" 'elisp-refs-prev-match
    "n"   'elisp-refs-next-match
    "N"   'elisp-refs-prev-match)
  (dolist (cmd '(elisp-refs-visit-match
                 elisp-refs-next-match
                 elisp-refs-prev-match))
    (helix-advice-add cmd :around #'helix-jump-command-a)))

(leaf elisp-def
  :elpaca t
  :after helix
  :hook
  (emacs-lisp-mode-hook
   . (lambda ()
       (remove-hook 'xref-backend-functions #'elisp--xref-backend :local)))
  :init
  (helix-keymap-set emacs-lisp-mode-map :state 'normal
    "g d" '("Find definition" . my-elisp-find-definitions)
    "C-w g d" '("Find definition other window" . my-elisp-find-definitions-other-window))

  (helix-advice-add 'my-elisp-find-definitions :around #'helix-jump-command-a)
  (helix-advice-add 'my-elisp-find-definitions-other-window :around #'helix-jump-command-a))

(defun my-elisp-find-definitions ()
  "Try `elisp-def', on fail try other xref backends."
  (interactive)
  (deactivate-mark)
  (or (ignore-errors (call-interactively #'elisp-def))
      (call-interactively #'xref-find-definitions)))

(defun my-elisp-find-definitions-other-window ()
  (interactive)
  (other-window-prefix)
  (my-elisp-find-definitions))

;;;;; Extra highlighting

;; `highlight-defined-builtin-function-name-face'
(leaf highlight-defined
  :elpaca t
  ;; :custom (highlight-defined-face-use-itself . t)
  :hook ((help-mode-hook . highlight-defined-mode)
         (emacs-lisp-mode-hook . highlight-defined-mode)))

;; ;; Extra faces definded by `lisp-extra-font-lock' package:
;; ;; - `lisp-extra-font-lock-backquote'
;; (leaf lisp-extra-font-lock
;;   :elpaca t
;;   :require t
;;   :config
;;   :global-minor-mode lisp-extra-font-lock-global-mode)

;; ;; Highlight quoted symbols
;; (leaf highlight-quoted
;;   :elpaca t
;;   :hook (emacs-lisp-mode-hook . highlight-quoted-mode))

;;; My commands

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

;; Rebind `universal-argument' from `C-u' to `M-u'.
;; By default `M-u' is binded to `upcase-word', so we can reuse it,
;; and `C-u' I use for scrolling like in Vim.
(keymap-global-set "M-u" #'universal-argument)
(keymap-set universal-argument-map "M-u" #'universal-argument-more)

;;;; Helix

(leaf helix
  :elpaca
  pcre2el
  edit-indirect
  (helix :repo "~/code/emacs/helix"
         :files (:defaults "**"))
  :require helix helix-leader
  :global-minor-mode helix-mode
  :custom
  (pixel-scroll-precision-interpolation-total-time . 0.3)
  (helix-leader-send-C-x-with-control-modifier . nil)
  :config
  (my-keymap-set global-map
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
  (my-keymap-set helix-window-map
    "N" 'other-tab-prefix)
  ;; Insert state
  (helix-keymap-global-set :state 'insert
    "C-w" 'backward-kill-word ;; together with C-backspace
    "C-/" 'dabbrev-expand))

;;;; <leader> key

(my-keymap-set global-map
  "C-x C-b" 'ibuffer-jump ;; list-buffers
  "C-x C-r" 'recentf-open ;; find-file-read-only
  "C-x C-d" 'dired-jump)  ;; list-directory

;; `mode-specific-map' keymap corresponds to the `C-c' prefix.
(my-keymap-set mode-specific-map
  "RET" 'bookmark-jump
  "," 'switch-to-buffer
  "'" '("Vertico repeat" . vertico-repeat)
  "\"" '("Select vertico session" . vertico-repeat-select)
  "/" 'consult-ripgrep
  "-" 'dired-jump
  "d" 'dired-jump
  ;; "d" 'kill-current-buffer
  "k" 'kill-current-buffer
  "f" (cons "file/find"
            (define-keymap
              ;; "x" 'xref-find-apropos
              "b" 'switch-to-buffer
              "f" 'find-file
              "F" 'consult-find
              "d" 'dired
              "l" 'locate
              "r" '("Recent files" . recentf-open)
              "w" 'write-file
              ;; "R" 'projectile-recentf
              ;; "u" '("Sudo this file" . doom/sudo-this-file)
              ;; "U" '("Sudo find file" . doom/sudo-find-file)
              ;; "x" '("Open scratch buffer" . doom/open-scratch-buffer)
              ;; "X" '("Switch to scratch buffer" . doom/switch-to-scratch-buffer)
              ))
  "b" (cons "buffer"
            (define-keymap
              "i" 'ibuffer-jump
              "b" 'ibuffer-jump
              "n" 'switch-to-buffer ;; next to `b' key
              "c" '("Clone indirect buffer other window" . clone-indirect-buffer-other-window)
              "C" '("Clone indirect buffer" . my-clone-indirect-buffer-same-window)
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
  "s" `("search" . ,search-map)
  "p" `("project" . ,project-prefix-map)
  "v" `("version control" . vc-prefix-map))

(my-keymap-set search-map
  "i" 'imenu)

;;;; Info-mode

(leaf info
  :after helix
  :hook
  (Info-mode-hook . my-disable-hl-line-mode)
  :config
  (helix-keymap-set Info-mode-map :state 'normal
    "M" 'helpful-at-point

    "g /"   'Info-menu
    "C-c /" 'Info-search
    "C-c ?" 'Info-search-case-sensitively
    "] ]"   'Info-next-reference
    "[ ["   'Info-prev-reference

    "H"     'Info-history
    "C-c h" 'Info-history))

;;;; org-mode keybindings

(with-eval-after-load 'helix
  (with-eval-after-load 'org
    (helix-keymap-set org-mode-map :state 'normal
      "C-h" #'helix-org-up-element
      "C-j" #'helix-org-next-element
      "C-k" #'helix-org-previous-element
      "C-l" #'helix-org-down-element

      ;; "z j" 'org-next-visible-heading
      ;; "z k" 'org-previous-visible-heading
      ;; "z p" 'outline-hide-other

      ;; "<remap> <outline-show-children>" 'org-fold-show-children
      ;; "<remap> <outline-show-subtree>"  'org-fold-show-subtree
      ;; "<remap> <transpose-words>"       'org-transpose-words
      ;; "<remap> <yank>"                  'org-yank

      ;; "[ RET" '+org/insert-item-above
      ;; "] RET" '+org/insert-item-below
      ;; "z n"   'org-narrow-to-subtree
      )))

;;;; paredit

(leaf helix-paredit
  :elpaca
  paredit
  (helix-paredit :repo "~/code/emacs/helix-paredit")
  :after helix
  :defer-config
  (helix-keymap-set helix-paredit-mode-map :state 'normal
    "C-h" 'helix-paredit-backward
    "C-j" 'helix-paredit-down-sexp
    "C-k" 'helix-paredit-backward-up-sexp
    "C-l" 'helix-paredit-forward))

;;;; occur-mode

(leaf replace
  :after helix
  :defer-config
  (helix-keymap-set occur-mode-map
    ;; "C-c t"   'next-error-follow-minor-mode
    "C-c m t" 'next-error-follow-minor-mode)
  (helix-keymap-set occur-edit-mode-map
    "RET" 'occur-mode-goto-occurrence-other-window)) ;; default C-o

;;;; Disable Isearch keys

(leaf isearch
  :after helix
  :config
  (helix-keymap-set global-map
    "C-s"   nil  ;; isearch-forward
    "C-M-s" nil  ;; isearch-forward-regexp
    "C-r"   nil  ;; isearch-backward
    "C-M-r" nil) ;; isearch-backward-regexp
  (helix-keymap-set search-map
    "w"   nil  ;; isearch-forward-word
    "_"   nil  ;; isearch-forward-symbol
    "."   nil  ;; isearch-forward-symbol-at-point
    "M-." nil) ;; isearch-forward-thing-at-point
  (with-eval-after-load 'embark
    (helix-keymap-set embark-general-map
      "C-s" nil   ;; embark-isearch-forward
      "C-r" nil)) ;; embark-isearch-backward
  ;; Delete all `M-' keybindings from `search-map' to make `g' key awailable
  ;; from the `keypad'.
  (setq search-map (assq-delete-all 27 search-map)))

;; (leaf isearch
;;   :bind
;;   ;; Consult integration
;;   (isearch-mode-map
;;    ("M-e"   . consult-isearch-history)
;;    ("M-s e" . consult-isearch-history)
;;    ("M-s l" . consult-line)
;;    ("M-s L" . consult-line-multi))
;;   (search-map
;;    ("M-s e" . consult-isearch-history)))

(provide 'post-init)
;;; post-init.el ends here
