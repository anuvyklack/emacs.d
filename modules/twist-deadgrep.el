;;; twist-deadgrep.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'helix-macros)
(require 'helix-core)

(use-package deadgrep
  :ensure t
  :defer t
  :config
  ;; (add-hook 'deadgrep-mode-hook #'next-error-follow-minor-mode)
  (add-hook 'deadgrep-edit-mode-hook #'twist-disable-hl-line-mode))

;;; Keybindings

(keymap-set search-map "s" 'deadgrep)

(with-eval-after-load 'deadgrep
  (helix-keymap-set deadgrep-mode-map :state 'motion
    "i"   'deadgrep-edit-mode

    "a"   'deadgrep-incremental ; `a' for amend
    "g r" 'deadgrep-restart

    "RET" 'deadgrep-visit-result-other-window

    "o"   'twist-deadgrep-show-result-other-window
    "C-o" 'twist-deadgrep-show-result-other-window

    "n"   'deadgrep-forward-match
    "N"   'deadgrep-backward-match

    "C-j" 'twist-deadgrep-forward-match-show-other-window
    "C-k" 'twist-deadgrep-backward-match-show-other-window

    "}"   'deadgrep-forward-filename
    "{"   'deadgrep-backward-filename
    "] p" 'deadgrep-forward-filename
    "[ p" 'deadgrep-backward-filename
    "z j" 'deadgrep-forward-filename
    "z k" 'deadgrep-backward-filename

    "z u" 'deadgrep-parent-directory)

  (helix-keymap-set deadgrep-edit-mode-map :state 'normal
    "<escape>" 'deadgrep-mode
    "Z Z" 'deadgrep-mode

    ;; Commands bound to these keys have no sense for Deadgrep.
    "o"   'undefined
    "O"   'undefined
    "J"   'undefined))

;;; Commands

(defun twist-deadgrep-show-result-other-window ()
  "Show search result at point in another window."
  (interactive)
  (unless next-error-follow-minor-mode
    (helix-recenter-point-on-jump
      (save-selected-window
        (deadgrep-visit-result-other-window)
        (deactivate-mark)))))

(defun twist-deadgrep-forward-match-show-other-window ()
  "Move point to next search result and show it in another window."
  (interactive)
  (deadgrep-forward-match)
  (twist-deadgrep-show-result-other-window))

(defun twist-deadgrep-backward-match-show-other-window ()
  "Move point to previous search result and show it in another window."
  (interactive)
  (deadgrep-backward-match)
  (twist-deadgrep-show-result-other-window))

(provide 'twist-deadgrep)
;;; twist-deadgrep.el ends here
