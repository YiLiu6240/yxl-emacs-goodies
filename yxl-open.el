(require 'ivy)
(require 'yxl-dired)

(defun yxl-open-in-terminal--iterm ()
  "Go to present working dir and focus iterm.

https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n")))

(defun yxl-open-in-terminal--linux ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (let ((process-connection-type nil))
    (start-process "" nil "x-terminal-emulator"
                   (concat "--working-directory=" default-directory))))

(defun yxl-open-in-terminal ()
  "Open current dir in terminal.
- windows: WIP
- macOS: iterm (need a running iterm instance; and dont manually change path in new profile)
- linux: default terminal"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "WIP"))
   ((string-equal system-type "darwin")
    (call-interactively 'yxl-open-in-terminal--iterm))
   ((string-equal system-type "gnu/linux")
    ;; TODO: test this
    (yxl-open-in-terminal--linux))))


(defun yxl-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
"
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

(defun yxl-open--linux-command (cmd file)
  (let ((process-connection-type nil))
    (start-process "" nil cmd file)))

(defun yxl-open--darwin-command (cmd file)
  (shell-command (format "%s \"%s\"" cmd file)))

(defun yxl-open--windows-command (cmd file)
  (w32-shell-execute cmd (replace-regexp-in-string "/" "\\\\" file)))

(defvar yxl-open-file-external-commands-linux
  '(("default" . (lambda (x) (browse-url x)))
    ("gvim" . (lambda (x) (yxl-open--linux-command "gvim" x)))
    ("subl" . (lambda (x) (yxl-open--linux-command "subl" x)))
    ("atom" . (lambda (x) (yxl-open--linux-command "atom" x)))
    ("zathura" . (lambda (x) (yxl-open--linux-command "zathura" x)))
    ("desktop" . (lambda (x) (yxl-open-in-desktop)))
    ("directory in terminal" . (lambda (x) (yxl-open-in-terminal)))))

(defvar yxl-open-file-external-commands-darwin
  '(("default" . (lambda (x) (browse-url x)))
    ("gvim" . (lambda (x) (yxl-open--darwin-command "gvim" x)))
    ("subl" . (lambda (x) (yxl-open--darwin-command "subl" x)))
    ("atom" . (lambda (x) (yxl-open--darwin-command "atom" x)))
    ("zathura" . (lambda (x) (yxl-open--darwin-command "zathura" x)))
    ("desktop" . (lambda (x) (yxl-open-in-desktop)))
    ("directory in terminal" . (lambda (x) (yxl-open-in-terminal)))))

(defvar yxl-open-file-external-commands-windows
  '(("default" . (lambda (x) (browse-url x)))
    ("gvim" . (lambda (x) (yxl-open--windows-command "gvim" x)))
    ("subl" . (lambda (x) (yxl-open--windows-command "subl" x)))
    ("atom" . (lambda (x) (yxl-open--windows-command "atom" x)))
    ("zathura" . (lambda (x) (yxl-open--windows-command "zathura" x)))
    ("desktop" . (lambda (x) (yxl-open-in-desktop)))
    ("directory in terminal" . (lambda (x) (yxl-open-in-terminal)))))

(defun yxl-open-file-external (&optional file)
  "open current file in an external command as defined in
`yxl-open-file-external-commands'."
  (interactive)
  (let ((file-path (or file
                       (if (derived-mode-p 'dired-mode)
                           (dired-get-file-for-visit)
                         buffer-file-name))))
    (ivy-read "Open in external applications:"
              (cond
               ((eq system-type 'gnu/linux) yxl-open-file-external-commands-linux)
               ((eq system-type 'darwin) yxl-open-file-external-commands-darwin)
               ((eq system-type 'windows-nt) yxl-open-file-external-commands-windows))
              :action (lambda (x)
                        (funcall (cdr x) file-path))
              :caller 'yxl-open-file-external)))

(provide 'yxl-open)
