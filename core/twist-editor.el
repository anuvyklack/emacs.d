;;; twist-editor.el --- defaults for text editing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Editor
;;;; Minibuffer

(setq enable-recursive-minibuffers t ; allow nested minibuffers
      history-delete-duplicates t
      resize-mini-windows 'grow-only)

;; Keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '( read-only t
                                      intangible t
                                      cursor-intangible t
                                      face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Save minibuffer history between sessions.
(use-package savehist
  :hook (after-init-hook . savehist-mode)
  :custom
  (history-length 300)
  (savehist-additional-variables '(kill-ring
                                   mark-ring
                                   global-mark-ring
                                   register-alist
                                   search-ring
                                   regexp-search-ring)))

;;;; Buffers

(setq uniquify-buffer-name-style 'forward)

;; ;; The initial buffer is created during startup even in non-interactive
;; ;; sessions, and its major mode is fully initialized. Modes like `text-mode',
;; ;; `org-mode', or even the default `lisp-interaction-mode' load extra packages
;; ;; and run hooks, which can slow down startup.
;; ;;
;; ;; Using `fundamental-mode' for the initial buffer to avoid unnecessary
;; ;; startup overhead.
;; (setq initial-major-mode 'fundamental-mode
;;       initial-scratch-message nil)

;; (substitute-command-keys initial-scratch-message)

;;;; Undo/redo

(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))

;;;; Files

;; ;; Resolve symlinks when opening files, so that any operations are conducted
;; ;; from the file's true directory (like `find-file').
;; (setq find-file-visit-truename t
;;       vc-follow-symlinks t)

;; ;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; ;; warning as it will redirect you to the existing buffer anyway.
;; (setq find-file-suppress-same-file-warnings t)

;; Delete by moving to trash in interactive mode
(setq delete-by-moving-to-trash (not noninteractive)
      remote-file-name-inhibit-delete-by-moving-to-trash t)

;; Create missing directories when we open a file that doesn't exist under
;; a directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (defun twist-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))

;;;;; Backup files

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      backup-directory-alist (list (cons "." (expand-file-name "backup" user-emacs-directory)))
      tramp-backup-directory-alist backup-directory-alist
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5)

;;;;; Auto save changes in files
;; Use `recover-file' or `recover-session' commands to restore auto-saved data.

(setq auto-save-default t
      auto-save-no-message t
      ;; Auto-disable auto-save after deleting large chunks of text.
      ;; I believe that in case of crash the more date the better.
      auto-save-include-big-deletions nil
      kill-buffer-delete-auto-save-files t)

(setq auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)
      tramp-auto-save-directory  (expand-file-name "tramp-autosave/" user-emacs-directory))

;; HACK: Emacs generates long file paths for its auto-save files; long is:
;;   `auto-save-list-file-prefix' + `buffer-file-name'. If too long, the
;;   filesystem will murder your family. To appease it the `buffer-file-name'
;;   is compressed to a stable 40 characters.
;; Borrowed from Doom Emacs.
(define-advice make-auto-save-file-name ( :around (fn)
                                          twist-make-hashed-auto-save-file-name)
  "Compress the auto-save file name so paths don't get too long."
  (let ((buffer-file-name
         (if (or
              ;; Don't do anything for non-file-visiting buffers. Names
              ;; generated for those are short enough already.
              (null buffer-file-name)
              ;; If an alternate handler exists for this path, bow out. Most of
              ;; them end up calling `make-auto-save-file-name' again anyway, so
              ;; we still achieve this advice's ultimate goal.
              (find-file-name-handler buffer-file-name
                                      'make-auto-save-file-name))
             buffer-file-name
           (sha1 buffer-file-name))))
    (funcall fn)))

;; HACK: ...does the same for Emacs backup files, but also packages that use
;;   `make-backup-file-name-1' directly (like undo-tree).
(define-advice make-backup-file-name-1 ( :around (fn file)
                                         twist-make-hashed-backup-file-name)
  "A few places use the backup file name so paths don't get too long."
  (let ((alist backup-directory-alist)
        backup-directory)
    (while alist
      (let ((elt (car alist)))
        (if (string-match (car elt) file)
            (setq backup-directory (cdr elt)
                  alist nil)
          (setq alist (cdr alist)))))
    (let ((file (funcall fn file)))
      (if (or (null backup-directory)
              (not (file-name-absolute-p backup-directory)))
          file
        (expand-file-name (sha1 (file-name-nondirectory file))
                          (file-name-directory file))))))

;;;;; Auto revert

;; Automatically revert the buffer when its visited file changes on disk.
;; Auto Revert will not revert a buffer if it has unsaved changes, or if
;; its file on disk is deleted or renamed.
(use-package autorevert
  :hook (after-init-hook . global-auto-revert-mode)
  :custom
  (auto-revert-stop-on-user-input nil)
  (auto-revert-verbose t) ; let us know when it happens
  (auto-revert-use-notify nil)
  (auto-revert-stop-on-user-input nil)
  (global-auto-revert-non-file-buffers t) ; e.g, Dired
  (global-auto-revert-ignore-modes '(Buffer-menu-mode))
  ;; Only prompts for confirmation when buffer is unsaved.
  (revert-without-query (list ".")))

;;;;; Save list of opened files and places in them

;; Keep track of opened files.
(use-package recentf
  :hook (after-init-hook . (lambda ()
                             (let ((inhibit-message t))
                               (recentf-mode 1))))
  :custom
  (recentf-max-saved-items 300) ; default is 20
  :config
  ;; Auto clean up recent files only in long-running daemon sessions, else
  ;; do it on quiting Emacs.
  (setopt recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

;; Save the last location within a file upon reopening.
(use-package saveplace
  :custom (save-place-limit 600)
  :hook (after-init-hook . save-place-mode)
  ;; :config
  ;; (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  )

;;;; Imenu

;; Automatically rescan the buffer for Imenu entries when `imenu' is invoked.
;; This ensures the index reflects recent edits.
(setq imenu-auto-rescan t)

;; Prevent truncation of long function names in `imenu' listings.
(setq imenu-max-item-length 160)

;;;; VC

(setq vc-git-print-log-follow t
      vc-make-backup-files nil ; Do not backup version controlled files.
      vc-git-diff-switches '("--histogram")) ; Faster algorithm for diffing.

;;; UI

;; Accept shorter responses: "y" for yes and "n" for no.
(setq use-short-answers t
      read-answer-short 'auto)

;; No beeping or blinking
(setq visible-bell nil
      ring-bell-function #'ignore)

(setq truncate-string-ellipsis "…")

;;;; Mouse

;; middle-click paste at point, not at click
(setq mouse-yank-at-point t)

;; Context menu on right mouse button click.
(when (and (display-graphic-p)
           (memq 'context-menu twist-emacs-ui-elements))
  (add-hook 'after-init-hook #'context-menu-mode))

;;;; Cursor

;; I anable blinking cursor, but it may interferes with cursor settings in some
;; minor modes that try to change it buffer-locally (e.g., Treemacs).
(blink-cursor-mode 1)

;; Do not extend the cursor to fit wide characters.
(setq x-stretch-cursor nil)

;;;; Line numbers

(use-package display-line-numbers
  :hook prog-mode text-mode conf-mode
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-type t)
  (display-line-numbers-width-start t)
  (display-line-numbers-grow-only t)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell
  ;; the buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen t))

;;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like diff-hl and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;;; Windows

;; Reduce rendering/line scan work by not rendering cursors or regions in
;; non-focused windows.
(setq-default cursor-in-non-selected-windows t)
(setq highlight-nonselected-windows t)

(setq window-resize-pixelwise t)

;; FIX: The native border "consumes" a pixel of the fringe on righter-most
;;   splits, `window-divider' does not.
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'after-init-hook #'window-divider-mode)

;; Prefer vertical splits over horizontal ones
(setq split-width-threshold 170
      split-height-threshold nil)

;;;; Scrolling

(setq pixel-scroll-precision-interpolation-total-time 0.3)

;; Enables faster scrolling. This may result in brief periods of inaccurate
;; syntax highlighting, which should quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keep screen position if scroll command moved it vertically out of the window.
(setq scroll-preserve-screen-position t)

;; 1. Preventing automatic adjustments to `window-vscroll' for long lines.
;; 2. Resolving the issue of random half-screen jumps during scrolling.
(setq auto-window-vscroll nil)

;; Number of lines of margin at the top and bottom of a window.
(setq scroll-margin 0)

;; Number of lines of continuity when scrolling by screenfuls.
(setq next-screen-context-lines 0)

;; Horizontal scrolling
(setq hscroll-margin 2
      hscroll-step 1)

;;; Text editing

;; Disable auto-adding a new line at the bottom when scrolling.
(setq next-line-add-newlines nil)

;; Remove duplicates from the kill ring to reduce clutter
(setq kill-do-not-save-duplicates t)

;;;; Parens

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

(setopt show-paren-delay 0.1
        show-paren-highlight-openparen t
        ;; show-paren-when-point-inside-paren t
        ;; show-paren-when-point-in-periphery t
        )

(provide 'twist-editor)
;;; twist-editor.el ends here
