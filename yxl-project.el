(require 'yxl-find)
(require 'ivy)
(require 'projectile)
(require 'helm-bibtex)

(defvar yxl-project-todo-global nil
  "Global / fallback todo file, usually your org file.")

(defvar-local yxl-project-main-file nil
  "Project main file")

(defvar-local yxl-project-make-file "Makefile"
  "Project make file")

(defvar-local yxl-project-todo-file "TODO.org"
  "Name of the todo file.")

(defvar-local yxl-project-note-file "NOTE.org")

(defvar-local yxl-project-bib-file nil)

(defun yxl-project-todo-popup ()
  "Popup the project todo file."
  (interactive)
  (if (or current-prefix-arg
          (eq nil (projectile-project-p)))
      (yxl-find-file-popup yxl-project-todo-global)
    (yxl-find-file-popup (concat (projectile-project-root)
                                 yxl-project-todo-file))))

(defun yxl-project-cite (&optional arg)
  (interactive "P")
  (let ((bibtex-completion-bibliography
         (or yxl-project-bib-file
             (bibtex-completion-find-local-bibliography))))
    (helm-bibtex arg)))

(defun yxl-project-find-file (file)
  (find-file (concat (projectile-project-root) file)))

(defun yxl-project-select ()
  (interactive)
  (ivy-read "Open project file:"
            '(("main" . (lambda () (yxl-project-find-file yxl-project-main-file)))
              ("todo" . (lambda () (yxl-project-find-file yxl-project-todo-file)))
              ("note" . (lambda () (yxl-project-find-file yxl-project-note-file)))
              ("make" . (lambda () (yxl-project-find-file yxl-project-make-file)))
              ("bib"  . (lambda () (yxl-project-find-file yxl-project-bib-file))))
            :action (lambda (x) (funcall (cdr x)))
            :caller 'yxl-project-select))

(provide 'yxl-project)
