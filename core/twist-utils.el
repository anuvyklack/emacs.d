;;; twist-utils.el -*- lexical-binding: t; -*-
;;; Code:
(require 's)
(require 'dash)

(defun +original-value (symbol)
  "Return the original value for SYMBOL, if any."
  ;; This code is taken from the `helpful' package. I have no idea why it’s
  ;; written this way, but the original author seems to be a very proficient
  ;; Elisp hacker.
  (let ((orig-val-expr (get symbol 'standard-value)))
    (if (consp orig-val-expr)
        (ignore-errors
          (eval (car orig-val-expr))))))

(defun +common-indentation ()
  "Return the common indentation off all lines in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((indentation 0))
      (while (not (eobp))
        (unless (s-blank-str? (thing-at-point 'line))
          (cl-callf min indentation (current-indentation)))
        (forward-line))
      indentation)))

(defun +hook-values (hook)
  "Return list with all local and global elements of the HOOK.
HOOK should be a symbol."
  (if (local-variable-p hook)
      (append (-remove (lambda (x) (eq x 't))
                       (buffer-local-value hook (current-buffer)))
              (default-value hook))
    ;; else
    (ensure-list (symbol-value hook))))

(provide 'twist-utils)
;;; twist-utils.el ends here
