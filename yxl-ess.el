(require 'ess-site)
(require 'ivy)

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

(setq yxl-ess-useful-funcs
      '(("sessionInfo() -- info on loaded pacakges" . "sessionInfo()")
        ("lsdf() -- list current dataframes -- user-defined" . "lsdf()")
        ("lsos() -- list object by size -- user-defined" . "lsos()")))

(defun yxl-ess-call-useful-funcs ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-funcs
            :action (lambda (x) (ess-execute (cdr x)))
            :caller 'yxl-ess-call-useful-funcs))

(provide 'yxl-ess)
