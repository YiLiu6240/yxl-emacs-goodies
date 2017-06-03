(require 'yxl-find)
(require 'ivy)
(require 'projectile)
(require 'helm-bibtex)

(defvar yxl-project-list nil
  "List of projects to be fed into ivy")

(defvar yxl-project-todo-global nil
  "Global / fallback todo file, usually your org file.")

(defvar-local yxl-project-main-file "README.md"
  "Project main file")

(defvar-local yxl-project-make-file "Makefile"
  "Project make file")

(defvar-local yxl-project-doc-file "README.md"
  "Project documentation file")

(defvar-local yxl-project-todo-file "TODO.org"
  "Name of the todo file.")

(defvar-local yxl-project-note-file "docs/NOTE.org"
  "Name of a note file.")

(defvar-local yxl-project-tmp-file "tmp"
  "Name of a temporary file. Usually this should not be tracked.")

(defvar-local yxl-project-bib-file nil)

(defun yxl-project-open ()
  "Open projects registered in `yxl-project-list'"
  (interactive)
  (ivy-read "Open project:"
            yxl-project-list
            :action (lambda (x) (yxl-find-dir (expand-file-name x)))
            :caller 'yxl-project-open))

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
  (if file
      (find-file (concat (projectile-project-root) file))
    (find-file (projectile-project-root))))

(defun yxl-project-popup-file (file)
  (if file
      (popwin:popup-buffer (find-file-noselect
                            (concat (projectile-project-root) file))
                           :stick t :height 0.4
                           :position 'bottom)
    (popwin:popup-buffer (find-file-noselect
                          (projectile-project-root)
                          :stick t :height 0.4
                          :position 'bottom))))

(defun yxl-project-popup-directory (file)
  (popwin:popup-buffer (find-file-noselect
                        (file-name-directory
                         (concat (projectile-project-root) file)))
                       :stick t :height 0.4
                       :position 'bottom))

(defvar yxl-project-files
  '(("main" . yxl-project-main-file)
    ("root" . nil)
    ("doc" . yxl-project-doc-file)
    ("todo" . yxl-project-todo-file)
    ("note" . yxl-project-note-file)
    ("make" . yxl-project-make-file)
    ("bib"  . yxl-project-bib-file)
    ("tmp"  . yxl-project-tmp-file)))

(defun yxl-project-select ()
  (interactive)
  (ivy-read "Open project file:"
            yxl-project-files
            :action (lambda (x) (yxl-project-find-file (symbol-value (cdr x))))
            :caller 'yxl-project-select))

(ivy-add-actions
 'yxl-project-select
 '(("p" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-popup-file file)))
    "popup")
   ("d" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-popup-directory file)))
    "directory")))

(provide 'yxl-project)
