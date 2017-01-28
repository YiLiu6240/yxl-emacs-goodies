(require 'dired)
(require 'dired-quick-sort)
(require 'ivy)



(defun yxl-dired-popup ()
  (interactive)
  (yxl-find-file-popup default-directory))



(defun yxl-ivy--get-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
             (buffer-local-value 'default-directory buffer)))
         (buffer-list))))

(defun yxl-ivy--get-non-dired-buffer-list ()
  (delq nil
        (mapcar
         (lambda (buffer)
           (when (not (eq 'dired-mode (buffer-local-value 'major-mode buffer)))
             (buffer-file-name buffer)))
         (buffer-list))))

(defun yxl-ivy-switch-dired-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to dired buffer: "
            (yxl-ivy--get-dired-buffer-list)
            :action (lambda (x)
                      (find-file x))
            :caller 'yxl-ivy-switch-dired-buffer))


(defun yxl-dired/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram ".")))))

(defun yxl-dired/toggle-dwim-target ()
  "toggle the value of dired-dwim-target."
  (interactive)
  (if (equal dired-dwim-target t)
      (setq dired-dwim-target nil)
    (setq dired-dwim-target t))
  (message "dired-dwim-target: %s" dired-dwim-target))



(defhydra yxl-dired-hydra-common (:color blue)
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
  (:color pink :inherit (yxl-dired-hydra-common/heads))
  "
 | _T_: +toggle ^^^^                   | _s_: +sort  ^^^^               | _*_: +mark ^^^^                 |
 | _m_/_u_/_U_: mark/unmark/unmark all | _y_: filename ^^^^             | _C_/_D_/_R_: copy/delete/rename | _+_: mkdir |
 | _H_/_S_: hardlink/symlink ^^        | _M_/_G_/_O_: chmod/chgrp/chown |
 | _Z_: compress ^^^^                  |
 | ___/_-_: rename: _/- ^^             |
"
  ("q" nil "quit" :color blue)
  ("o" yxl-dired-open-in-desktop "open in desktop" :color blue)
  ("s" hydra-dired-quick-sort/body "+sort" :color blue)
  ("T" hydra-dired-toggle/body "+toggle" :color blue)
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
  ("Z" dired-do-compress "compress")
  ("_" xah-dired-rename-space-to-underscore "rename: _")
  ("-" xah-dired-rename-space-to-hyphen "rename: -")
  ("+" dired-create-directory "mkdir"))

(defhydra hydra-dired-toggle
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
