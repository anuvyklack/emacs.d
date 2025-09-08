;;; pre-init.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;
;;; Commentary:
;;; Code:
;;; Fonts

(set-face-font 'default (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(set-face-font 'fixed-pitch (font-spec :family "PragmataPro Mono Liga" :size 13.9))
(setopt use-default-font-for-symbols t)

;; (set-face-font 'default (font-spec :family "Inconsolata LGC" :size 17))
;; (setopt use-default-font-for-symbols nil)

;; (set-face-attribute 'fixed-pitch nil
;;                     :font (font-spec :family "PragmataPro Mono Liga" :size 13.9))
;; (set-face-attribute 'fixed-pitch-serif nil
;;                     :family "Iosevka Term Curly Slab Medium")

;; Nerd Font
(set-fontset-font t (cons ?\xf0001 ?\xf1af0) "Symbols Nerd Font Mono" nil 'prepend)

;; Unicode Symbols for Legacy Computing
(set-fontset-font t (cons ?\x1fb00 ?\x1fbca) "LegacyComputing" nil 'prepend)
(set-fontset-font t (cons ?🯰 ?🯹) "LegacyComputing" nil 'prepend)

;;; Network

;; Emacs populates `url-proxy-services' variable from `https_proxy',
;; `socks_proxy', `no_proxy' environment variables.
(setq url-proxy-services '(("socks" . "127.0.0.1:10808")
                           ("https" . "127.0.0.1:10809")))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; customize-set-variable
(setopt package-archive-priorities '(("gnu"    . 0)
                                     ("nongnu" . 0)
                                     ("melpa"  . 75)))

;;; Elpaca bootstrap

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1 :inherit ignore
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;; Load `custom.el'

(add-hook 'elpaca-after-init-hook
          #'(lambda ()
              (when (file-exists-p custom-file)
                (load custom-file :noerror :nomessage))))

(provide 'pre-init)
;;; pre-init.el ends here
