;;; twist-utils.el -*- lexical-binding: t; -*-
;;; Code:

(defun twist-original-value (symbol)
  "Return the original value for SYMBOL, if any."
  ;; This code is taken from the `helpful' package. I have no idea why it’s
  ;; written this way, but the original author seems to be a very proficient
  ;; Elisp hacker.
  (let ((orig-val-expr (get symbol 'standard-value)))
    (if (consp orig-val-expr)
        (ignore-errors
          (eval (car orig-val-expr))))))

(provide 'twist-utils)
;;; twist-utils.el ends here
