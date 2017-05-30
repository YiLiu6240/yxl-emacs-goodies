(require 'hi-lock)

(defvar-local yxl-dired-highlight-keyword-alist nil
  "List of keyword and faces.
Example: '((\"regex1\" . face2) (\"regex2\" . face2)).
Best to set it as dir-local.")

(define-minor-mode yxl-dired-highlight-minor-mode
  "Highlight keywords using their associated faces in dired."
  :init-value nil
  (if yxl-dired-highlight-minor-mode
      (progn
        (unless hi-lock-mode (hi-lock-mode 1))
        (dolist (pair yxl-dired-highlight-keyword-alist)
          (hi-lock-set-pattern (car pair) (cdr pair))))
    (hi-lock-mode 0)))

(add-hook 'dired-mode-hook 'yxl-dired-highlight-minor-mode)

(provide 'yxl-dired-highlight)
