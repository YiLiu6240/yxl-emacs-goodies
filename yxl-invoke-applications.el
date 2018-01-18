(require 'ivy)

(defvar yxl-ia-list
  '(("calendar" . calendar)
    ("org-agenda" . org-agenda)
    ("org-capture" . org-capture))
  "List of registered 'applications' to be invoked by
`yxl-invoke-applications'. Elements should be as '('app-name' . func-name).")

(defun yxl-invoke-applications ()
  (interactive)
  (let* ((sort-func (lambda (x y) (string< (car x) (car y))))
         (sorted-seq (seq-sort sort-func yxl-ia-list)))
    (ivy-read "Invoke applications:"
              sorted-seq
              :action (lambda (x) (funcall (cdr x)))
              :caller 'yxl-invoke-select)))

(provide 'yxl-invoke-applications)
