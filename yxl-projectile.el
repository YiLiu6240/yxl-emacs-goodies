(require 'projectile)
(require 'counsel-projectile)



(defun yxl-projectile-find-file ()
  "Special case of finding file in projects:
Only find files in a git project. Useful when operate in nested projects:
use the normal version to find file in a projectile project, and use this version
to find files in the super project."
  (interactive)
  (let* ((projectile-project-root-files-bottom-up '(".git"))
         (projectile-project-root (projectile-root-bottom-up default-directory))
         (projectile-project-name projectile-project-root))
    (counsel-projectile-find-file)))



(provide 'yxl-projectile)
