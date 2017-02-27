(require 'ess-site)
(require 'ivy)

;; TODO:
;; add optional arguments
(defvar yxl-ess-useful-funcs
  '(("sessionInfo() -- info on loaded pacakges" . "sessionInfo()")
    ("lsdf() -- list current dataframes -- user-defined" . "lsdf()")
    ("lsos() -- list object by size -- user-defined" . "lsos()"))
  "list of useful R functions to execute")

(defvar yxl-ess-useful-atpoint-funcs
  '(("str()" . "str")
    ("dplyr::summarize()" . "dplyr::summarize")
    ("summary()" . "summary")
    ("dim()" . "dim")
    ("dimnames()" . "dimnames"))
  "list of useful R functions to execute to the current object atpoint.")

(defun yxl-ess-call-atpoint-func (r-func)
  (let ((objname (current-word)))
    (if objname
        (progn
          (ess-execute (concat r-func "(" objname ")"))))))

(defun yxl-ess-call-atpoint-str ()
  (interactive)
  (yxl-ess-call-atpoint-func "str"))

(defun yxl-ess-call-atpoint-generic (r-func)
  (interactive "sR function to execute: ")
  (yxl-ess-call-atpoint-func r-func))

(defun yxl-ess-call-useful-funcs ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-funcs
            :action (lambda (x) (ess-execute (cdr x)))
            :caller 'yxl-ess-call-useful-funcs))

(defun yxl-ess-call-atpoint-useful-funcs ()
  (interactive)
  (ivy-read "Call useful funcs:"
            yxl-ess-useful-atpoint-funcs
            :action (lambda (x) (yxl-ess-call-atpoint-func (cdr x)))
            :caller 'yxl-ess-call-atpoint-useful-funcs))

(defun yxl-ess-rdired-str ()
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-execute (concat "str(" objname ")\n"))))

(provide 'yxl-ess)
