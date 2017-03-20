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

(setq yxl-open-file-external-commands
      ;; TODO: open in browser:
      ;;       I set `browse-url-browser-function' to sth like "open"
      ;;       and it opens default program instead of a browser when current file
      ;;       is not default to open in a browser.
      ;;       Might need a disgustingly complex way to invoke browser
      '(("default" . (lambda (x) (browse-url x)))
        ("gvim" . (lambda (x) (shell-command (format "gvim \"%s\"" x))))
        ("subl" . (lambda (x) (shell-command (format "subl \"%s\"" x))))
        ("atom" . (lambda (x) (shell-command (format "atom \"%s\"" x))))
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
            yxl-open-file-external-commands
            :action (lambda (x)
                      (funcall (cdr x) file-path))
            :caller 'yxl-open-file-external)))

(provide 'yxl-open)
