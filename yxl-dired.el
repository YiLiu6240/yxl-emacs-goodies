(require 'dired)
(require 'dired-quick-sort)
(require 'ivy)
(require 'yxl-ace-window)
(require 'yxl-dired-highlight)

(defun yxl-dired-popup ()
  (interactive)
  (yxl-find-file-popup default-directory))

(defun yxl-dired--ivy-get-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (buffer-local-value 'default-directory buffer)))
         (buffer-list))))

(defun yxl-dired--ivy-get-non-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (not (eq 'dired-mode (buffer-local-value 'major-mode buffer)))
             (buffer-file-name buffer)))
         (buffer-list))))

(defun yxl-dired-ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to dired buffer: "
            (yxl-dired--ivy-get-dired-buffer-list)
            :action (lambda (x)
                      (find-file x))
            :caller 'yxl-dired-ivy-switch-buffer))

(defun concat-string-list (list)
  "Return a string which is a concatenation of all elements of the list separated by spaces"
  (mapconcat '(lambda (obj) (format "%s" obj)) list " "))

(defun yxl-dired-zip-files (zip-file)
  "Create an archive containing the marked files.
Alternatively, run \"! zip foo.zip * <RET>\" for marked files in dired.
source:
http://stackoverflow.com/questions/1431351/how-do-i-uncompress-unzip-within-emacs"
  (interactive "sEnter name of zip file: ")

  ;; create the zip file
  (let ((zip-file (if (string-match ".zip$" zip-file) zip-file (concat zip-file ".zip"))))
    (shell-command
     (concat "zip -r "
             zip-file
             " "
             (concat-string-list
              (mapcar
               '(lambda (filename)
                  (file-name-nondirectory filename))
               (dired-get-marked-files))))))
  (revert-buffer))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2017-01-02"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x)
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files))
        (revert-buffer))
    (user-error "Not in dired.")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x)
                  (dired-rename-file x (replace-regexp-in-string " " "-" x) nil)))
              (dired-get-marked-files))
        (revert-buffer))
    (user-error "Not in dired")))

(defun yxl-dired-toggle-dwim-target ()
  "toggle the value of dired-dwim-target."
  (interactive)
  (if (equal dired-dwim-target t)
      (setq dired-dwim-target nil)
    (setq dired-dwim-target t))
  (message "dired-dwim-target: %s" dired-dwim-target))

(defun yxl-dired-open-aw ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (yxl-ace-window-open file)))

(defun yxl-dired-open-aw-vert ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (yxl-ace-window-open-vert file)))

(defun yxl-dired-open-aw-horz ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (yxl-ace-window-open-horz file)))

(defun ora-ediff-files ()
  ;; https://oremacs.com/
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

(defun ora-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(provide 'yxl-dired)
