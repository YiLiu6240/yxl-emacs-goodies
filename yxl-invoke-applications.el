(require 'ivy)

(defvar yxl-ia-list
  '(("calendar" . calendar)
    ("org-agenda" . org-agenda)
    ("org-capture" . org-capture))
  "List of registered 'applications' to be invoked by
`yxl-invoke-applications'. Elements should be as '('app-name' . func-name).")

(defun yxl-invoke-applications ()
  (interactive)
  (ivy-read "Invoke applications:"
            (sort (delete-dups yxl-ia-list) (lambda (elem1 elem2)
                                              (let ((str1 (car elem1))
                                                    (str2 (car elem2)))
                                                (string-lessp str1 str2))))
            :action (lambda (x) (funcall (cdr x)))
            :caller 'yxl-invoke-select))

(provide 'yxl-invoke-applications)
