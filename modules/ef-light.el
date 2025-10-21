;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;; (require 'twist-utils)

;; (add-hook 'enable-theme-functions
;;           (lambda (theme)
;;             (custom-theme-set-faces
;;              'ef-light
;;              '(help-key-binding ((t (:foreground "DarkBlue" :background "grey96"
;;                                      :box (:line-width (-1 . -1) :color "grey80")
;;                                      :inherit fixed-pitch))))
;;              '(line-number ((t (:background "#f5f5f5" :inherit fixed-pitch))))
;;              '(line-number-current-line ((t (:background "#dddddd" :weight bold
;;                                              :inherit line-number))))
;;              '(fringe ((t (:background "#f3f3f3")))))))

;;; General

(+theme-set-faces 'ef-light
  '(region
    :background "#d6f4ff" ;; original #bfefff
    :extend t)
  '(help-key-binding
    :foreground "DarkBlue" :background "grey96"
    :box (:line-width (-1 . -1) :color "grey80")
    :inherit fixed-pitch)
  '(line-number :background "#f5f5f5" :inherit fixed-pitch)
  '(line-number-current-line
    :background "#dddddd" :weight bold :inherit line-number)
  '(fringe :background "#f3f3f3"))

;;; helix

(+theme-set-faces 'ef-light
  '(helix-mode-line-cursors-indicator
    :weight bold
    ;; :inherit mode-line-emphasis
    :inherit ef-themes-mark-other)
  '(helix-search-highlight
    :background "#ffeabb"
    ;; :inherit ef-themes-search-lazy
    ))

;;; org-mode
;;;; headings

;; (font '(:family "Basic Commercial LT" :weight normal))

;; (let ((font '(:family "ITC Avant Garde Gothic W1G" :weight medium)))
;;   (+theme-set-faces 'ef-light
;;     `(org-level-1 :foreground "#375cd8" :height 1.09 ,@font)
;;     '(org-level-2 :foreground "#cf25aa" :weight normal :height 1.09)
;;     '(org-level-3 :foreground "#1f77bb" :weight normal :height 1.09)
;;     '(org-level-4 :foreground "#b65050" :weight normal :height 1.09)
;;     '(org-level-5 :foreground "#6052cf" :weight normal :height 1.09)))

;; Add a box with the same color as a background around every org heading.
;; It does take effect of adding padding around headings, but it is a hack
;; and not reliable.
(let ((font (font-spec :family "ITC Avant Garde Gothic W1G" :weight 'semibold)))
  (+theme-set-faces 'ef-light
    `(org-level-1
      :foreground "#375cd8" :height 1.09 :font ,font
      :box (:line-width 2 :color ,(face-background 'default)))
    `(org-level-2
      :foreground "#cf25aa" :weight normal :height 1.08
      :box (:line-width 2 :color ,(face-background 'default)))
    `(org-level-3
      :foreground "#1f77bb" :weight normal :height 1.08
      :box (:line-width 2 :color ,(face-background 'default)))
    `(org-level-4
      :foreground "#b65050" :weight normal :height 1.08
      :box (:line-width 2 :color ,(face-background 'default)))
    `(org-level-5
      :foreground "#6052cf" :weight normal :height 1.08
      :box (:line-width 2 :color ,(face-background 'default)))))

;;;; code blocks

(+theme-set-faces 'ef-light
  '(org-verbatim :foreground "#4250ef" :background "#f5f5f5")
  '(org-code     :foreground "#cf25aa" :background "#f5f5f5"))
