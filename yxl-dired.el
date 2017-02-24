(require 'dired)
(require 'dired-quick-sort)
(require 'ivy)

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

(defhydra yxl-dired-hydra-common (:color blue :hint nil)
  ("." nil "quit"))

(defhydra yxl-dired-hydra-mark
  (:hint nil :color red :inherit (yxl-dired-hydra-common/heads))
  "
 | _q_ ../         | _u_ unmark   |  _!_ unmark all | _s_ files in subdir
 | _m_ mark        | _@_ symlinks |  _/_ dirs       | ^^
 | _*_ executables | ^^           | ^^              | ^^
 | _t_ toggle      | ^^           | ^^              | ^^
 | _%_ regexp      | ^^           | ^^              | ^^
    "
  ("q" yxl-dired-hydra-main/body :color blue "../")
  ("m" dired-mark)
  ("*" dired-mark-executables)
  ("@" dired-mark-symlinks)
  ("/" dired-mark-directories)
  ("s" dired-mark-subdir-files)
  ("u" dired-unmark)
  ("!" dired-unmark-all-marks)
  ("t" dired-toggle-marks)
  ("%" dired-mark-files-regexp))

(defhydra yxl-dired-hydra-main
  (:color pink :inherit (yxl-dired-hydra-common/heads) :hint nil :columns 4)
  ("q" nil "quit" :color blue)
  ("o" yxl-dired-open-in-desktop "open in desktop" :color blue)
  ("s" hydra-dired-quick-sort/body "+sort" :color blue)
  ("T" yxl-hydra-dired-toggle/body "+toggle" :color blue)
  ("*" yxl-dired-hydra-mark/body "+mark" :color blue)
  ("m" dired-mark "mark")
  ("u" dired-unmark "unmark")
  ("U" dired-unmark-all-marks "unmark all")
  ("y" dired-copy-filename-as-kill "filename" :color blue)
  ("Y" dired-copy-filename-as-kill-fullname "full filename" :color blue)
  ("C" dired-do-copy "copy")
  ("D" dired-do-delete "delete")
  ("R" dired-do-rename "rename")
  ("H" dired-do-hardlink "hardlink")
  ("S" dired-do-symlink "symlink")
  ("M" dired-do-chmod "chmod")
  ("G" dired-do-chgrp "chgrp")
  ("O" dired-do-chown "chown")
  ("z" yxl-dired-zip-files "zip")
  ("Z" dired-do-compress "compress/uncompress")
  ("_" xah-dired-rename-space-to-underscore "rename: _")
  ("-" xah-dired-rename-space-to-hyphen "rename: -")
  ("+" dired-create-directory "mkdir"))

(defhydra yxl-hydra-dired-toggle
  (:hint none :color red)
  "
 | _q_ ../             | _T_ ../      | _._ quit
 | _h_ ?h? hide detail | _H_ ?H? omit | _d_ ?d? dwim-target
 | _r_ read only (restore with C-x C-q)
"
  ("." nil)
  ("q" yxl-dired-hydra-main/body :color blue)
  ("T" yxl-dired-hydra-main/body :color blue)
  ("h" dired-hide-details-mode
   (if (bound-and-true-p dired-hide-details-mode)
       "[X]" "[ ]"))
  ("H" dired-omit-mode
   (if (bound-and-true-p dired-omit-mode)
       "[X]" "[ ]"))
  ("d" yxl-dired-toggle-dwim-target
   (if dired-dwim-target
       "[X]" "[ ]"))
  ("r" dired-toggle-read-only :color blue))

(provide 'yxl-dired)
