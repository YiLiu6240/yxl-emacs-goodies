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

;; my own custom funcs in R

(defun yxl-ess-exec-lsos ()
  "invoke lsos function, this function needs to be in environment"
  (interactive)
  (ess-execute "lsos()"))

(defun yxl-ess-exec-lsdf ()
  "invoke lsos function, this function needs to be in environment"
  (interactive)
  (ess-execute "lsdf()"))

(provide 'yxl-ess)
