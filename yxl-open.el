(require 'ivy)
(require 'yxl-dired)



(setq yxl-open-file-external-commands '(("gvim" . (lambda () (shell-command (format "gvim \"%s\"" buffer-file-name))))
                                        ("subl" . (lambda () (shell-command (format "subl \"%s\"" buffer-file-name))))
                                        ("atom" . (lambda () (shell-command (format "atom \"%s\"" buffer-file-name))))
                                        ("desktop" . yxl-dired-open-in-desktop)))

(defun yxl-open-file-external ()
  "open current file in an external command as defined in
`yxl-open-file-external-commands'."
  (interactive)
  (ivy-read "Open in external applications:"
            yxl-open-file-external-commands
            :action (lambda (x)
                      (funcall (cdr x)))
            :caller 'yxl-open-file-external))



(provide 'yxl-open)
