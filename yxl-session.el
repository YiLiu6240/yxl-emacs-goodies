(require 'desktop)

;; minimal dekstop setup, from emacswiki

;; use only one desktop
(defvar yxl-session-location "~/Dropbox/inbox/"
  "Location to store sessions")
(defvar yxl-session-file-name "yxl-emacs-desktop"
  "Name of the session file.")
(setq desktop-file-name-format 'tilde)
(setq history-length 100)

(add-to-list 'desktop-modes-not-to-save 'image-mode)
;; (add-to-list 'desktop-modes-not-to-save 'pdf-view-mode)
(add-to-list 'desktop-modes-not-to-save 'csv-mode)
(add-to-list 'desktop-modes-not-to-save 'cfw:calendar-mode)
(add-to-list 'desktop-modes-not-to-save 'elfeed-search-mode)
(add-to-list 'desktop-modes-not-to-save 'elfeed-show-mode)

;; save shell-mode in desktop
;; source: https://bmag.github.io/2015/12/26/desktop.html
(defun sy-save-shell-buffer (desktop-dirname)
  default-directory)

(defun sy-create-shell-buffer (_file-name buffer-name misc)
  "MISC is the value returned by `sy-save-shell-buffer'.
_FILE-NAME is nil."
  (let ((default-directory misc))
    ;; create a shell buffer named BUFFER-NAME in directory MISC
    (shell buffer-name)))
(add-hook 'shell-mode-hook
          (lambda () (setq-local desktop-save-buffer #'sy-save-shell-buffer)))
(add-to-list 'desktop-buffer-mode-handlers '(shell-mode . sy-create-shell-buffer))

(defun yxl-saved-session-p ()
  (file-exists-p (concat yxl-session-location "/"
                         yxl-session-file-name)))

(defun yxl-session-load-1 ()
  "Restore a saved emacs session."
  (interactive)
  (let* ((yxl-session-file-name "yxl-emacs-desktop-1")
         (desktop-base-file-name yxl-session-file-name))
    (if (yxl-saved-session-p)
        (desktop-read yxl-session-location)
      (message "No desktop found."))))

(defun yxl-session-load-2 ()
  "Restore a saved emacs session."
  (interactive)
  (let* ((yxl-session-file-name "yxl-emacs-desktop-2")
         (desktop-base-file-name yxl-session-file-name))
    (if (yxl-saved-session-p)
        (desktop-read yxl-session-location)
      (message "No desktop found."))))

(defun yxl-session-save-1 ()
  "Save an emacs session."
  (interactive)
  (let* ((yxl-session-file-name "yxl-emacs-desktop-1")
         (desktop-base-file-name yxl-session-file-name))
    (if (yxl-saved-session-p)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save yxl-session-location)
          (message "Session not saved."))
      (desktop-save yxl-session-location))))

(defun yxl-session-save-2 ()
  "Save an emacs session."
  (interactive)
  (let* ((yxl-session-file-name "yxl-emacs-desktop-2")
         (desktop-base-file-name yxl-session-file-name))
    (if (yxl-saved-session-p)
        (if (y-or-n-p "Overwrite existing desktop? ")
            (desktop-save yxl-session-location)
          (message "Session not saved."))
      (desktop-save yxl-session-location))))

(provide 'yxl-session)
