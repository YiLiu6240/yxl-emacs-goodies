(require 'yxl-find)
(require 'ivy)
(require 'projectile)
(require 'helm)
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

(defvar-local yxl-project-tmp-code-file "tmp.py"
  "Name of a temporary file. Usually this should not be tracked.")

(defvar-local yxl-project-tmp-text-file "tmp.md"
  "Name of a temporary file. Usually this should not be tracked.")

(defvar-local yxl-project-output-directory "output"
  "Name of an output directory. Could be \"output\", or \"build\"
or whatever suits the purpose of the project.")

(defvar-local yxl-project-bib-file nil)

(defvar-local yxl-project-helm-sources
  '(((name . "Project files")
     (candidates . (("root" . nil)
                    ("Makefile" . "Makefile")))
     (action . (("popup" lambda (x) (yxl-project-popup-file x))
                ("visit" lambda (x) (yxl-project-find-file x)))))
    ((name . "Documentations")
     (candidates . (("README" . "README.md")
                    ("todo" . "TODO")))
     (action . (("popup" lambda (x) (yxl-project-popup-file x))
                ("visit" lambda (x) (yxl-project-find-file x))))))
  "The basic structure of sources used in yxl-project-helm.")

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
                          (projectile-project-root))
                         :stick t :height 0.4
                         :position 'bottom)))

(defun yxl-project-popup-directory (file)
  (popwin:popup-buffer (find-file-noselect
                        (file-name-directory
                         (concat (projectile-project-root) file)))
                       :stick t :height 0.4
                       :position 'bottom))

(defun yxl-project-shell-popup ()
  (interactive)
  (let ((buf-name "*shell*")
        (cur-buf (current-buffer))
        (cwd (projectile-project-root)))
    (unless (buffer-live-p (get-buffer buf-name))
      (progn
        (get-buffer-create buf-name)
        (with-current-buffer buf-name
          (shell))
        (switch-to-buffer cur-buf)))
    (popwin:popup-buffer (get-buffer buf-name)
                         :stick t :height 0.4
                         :position 'bottom)
    (unless (equal default-directory cwd)
      (funcall (lambda (cwd)
                 (goto-char (point-max))
                 (comint-kill-input)
                 (insert (concat "cd " cwd))
                 (let ((comint-process-echoes t))
                   (comint-send-input))
                 (recenter 0))
               cwd))))

(defvar yxl-project-files
  '(("main" . yxl-project-main-file)
    ("root" . nil)
    ("output" . yxl-project-output-directory)
    ("doc" . yxl-project-doc-file)
    ("todo" . yxl-project-todo-file)
    ("note" . yxl-project-note-file)
    ("make" . yxl-project-make-file)
    ("bib"  . yxl-project-bib-file)
    ("tmp-code"  . yxl-project-tmp-code-file)
    ("tmp-text"  . yxl-project-tmp-text-file)))

(defun yxl-project-select ()
  (interactive)
  (ivy-read "Visit project file:"
            yxl-project-files
            :action (lambda (x)
                      (yxl-project-find-file (symbol-value (cdr x))))
            :caller 'yxl-project-select))

(defun yxl-project-popup ()
  (interactive)
  (ivy-read "Popup project file:"
            yxl-project-files
            :action (lambda (x)
                      (yxl-project-popup-file (symbol-value (cdr x))))
            :caller 'yxl-project-select))

(ivy-add-actions
 'yxl-project-select
 '(("p" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-popup-file file)))
    "popup")
   ("d" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-popup-directory file)))
    "directory")))

(ivy-add-actions
 'yxl-project-select
 '(("o" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-find-file file)))
    "popup")
   ("d" (lambda (x) (let ((file (symbol-value (cdr x))))
                      (yxl-project-popup-directory file)))
    "directory")))

(defun yxl-project-helm-display (buffer)
  (let ((display-buffer-alist (list '("*.*helm.**"
                                      (display-buffer-in-side-window)
                                      (inhibit-same-window . t)
                                      (side . left)
                                      (window-width . 0.2)
                                      (window-height . 0.4))))
        (helm-split-window-default-side 'left))
    (helm-default-display-buffer buffer)))

(defun yxl-project-helm ()
  "A helm implementation that is more flexible than
`yxl-project-select' and `yxl-project-popup'."
  (interactive)
  (let ((helm-display-function #'yxl-project-helm-display))
    (helm :sources
          yxl-project-helm-sources
          :action (lambda (candidate)
                    (funcall candidate)))))

(provide 'yxl-project)
