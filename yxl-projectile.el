(require 'projectile)
(require 'counsel-projectile)



(defun yxl-projectile-find-file ()
  "Only find files in git projects."
  (interactive)
  (let* ((projectile-project-root-files-bottom-up '(".git")))
    (counsel-projectile-find-file)))



(provide 'yxl-projectile)
