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
- macOS: iterm
- linux: WIP"
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
      '(("gvim" . (lambda () (shell-command (format "gvim \"%s\"" buffer-file-name))))
        ("subl" . (lambda () (shell-command (format "subl \"%s\"" buffer-file-name))))
        ("atom" . (lambda () (shell-command (format "atom \"%s\"" buffer-file-name))))
        ("desktop" . yxl-open-in-desktop)
        ("browser" . (lambda () (message "WIP!")))
        ("directory in terminal" . yxl-open-in-terminal)))

(defun yxl-open-file-external ()
  "open current file in an external command as defined in
`yxl-open-file-external-commands'."
  (interactive)
  (ivy-read "Open in external applications:"
            yxl-open-file-external-commands
            :action (lambda (x)
                      (funcall (cdr x)))
            :caller 'yxl-open-file-external))



(provide 'yxl-open)
