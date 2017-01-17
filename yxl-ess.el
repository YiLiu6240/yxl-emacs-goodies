(require 'ess-site)



(defun yxl-ess-rdired-str ()
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-execute (concat "str(" objname ")\n"))))

(defun yxl-ess-execute-func-at-point (r-func)
  (let ((objname (current-word)))
    (if objname
        (progn
          (ess-execute (concat r-func "(" objname ")"))))))

(defun yxl-ess-at-point-str ()
  (interactive)
  (yxl-ess-execute-func-at-point "str"))

(defun yxl-ess-at-point-generic (r-func)
  (interactive "sR function to execute: ")
  (yxl-ess-execute-func-at-point r-func))



(provide 'yxl-ess)
