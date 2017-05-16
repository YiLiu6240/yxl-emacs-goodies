(require 'ivy)

(defvar yxl-ia-list
  '("calendar" "org-agenda" "org-capture")
  "List of registered 'applications' to be invoked by
`yxl-invoke-applications'. Elements should be the string of the command,
to be able to sort.")

(defun yxl-invoke-applications ()
  (interactive)
  (ivy-read "Invoke applications:"
            (sort (delete-dups yxl-ia-list) #'string<)
            :action (lambda (x) (funcall (intern x)))
            :caller 'yxl-invoke-select))

(provide 'yxl-invoke-applications)
