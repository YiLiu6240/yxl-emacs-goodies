(require 'yxl-dired)
(require 'yxl-frame)
(require 'yxl-window)
(require 'yxl-buffer)
(require 'yxl-find)
(require 'yxl-web)



(defun yxl-append-to-scratch (&optional file)
  "receive input text and append this text to scratch"
  (interactive)
  (let* ((text (read-string "Enter text:"))
         (scratch-buf (if file
                          (find-file-noselect file)
                        (get-buffer-create "*scratch*")))
         (text-with-newline (concat text "\n")))
    (save-excursion
      (with-current-buffer scratch-buf
        (end-of-buffer)
        (insert text-with-newline)))))

(defun yxl-show-and-copy-buffer-filename-in-projectile ()
  "TODO: document"
  (interactive)
  (let* ((file-name (or (buffer-file-name) list-buffers-directory))
         (proj-root (projectile-project-root))
         (relative-file-name (string-remove-prefix proj-root file-name)))
    (if relative-file-name
        (message (kill-new relative-file-name))
      (error "Buffer not visiting a file"))))



(defun yxl-round-nb-in-region ()
  "http://stackoverflow.com/questions/23636226/how-to-round-all-the-numbers-in-a-region"
  (interactive)
  (let ((round-format (read-string "enter format (%0.4f): ")))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char 1)
      (let ((case-fold-search nil))
        (while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
          (replace-match (format round-format
                                 (string-to-number (match-string 1)))))))))

(defun yxl-copy-to-clipboard ()
  "Copies selection to x-clipboard."
  ;; TODO: add source
  (interactive)
  (let ((cli-cmd (cond ((eq system-type 'darwin) "pbcopy")
                       ((eq system-type 'gnu/linux) "xsel --clipboard --input"))))
    (if (display-graphic-p)
        (progn
          (message "Yanked region to x-clipboard!")
          (call-interactively 'clipboard-kill-ring-save))
      (if (region-active-p)
          (progn
            (shell-command-on-region (region-beginning) (region-end) cli-cmd)
            (message "Yanked region to clipboard!")
            (deactivate-mark))
        (message "No region active; can't yank to clipboard!")))))

(defun yxl-paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (let ((cli-cmd (cond ((eq system-type 'darwin) "pbpaste")
                       ((eq system-type 'gnu/linux) "xsel --clipboard --input"))))
    (if (display-graphic-p)
        (progn
          (clipboard-yank)
          (message "graphics active"))
      ;; TODO: check if the evil package is used
      (if (eq evil-state 'normal)
          (evil-append 1))
      (insert (shell-command-to-string cli-cmd)))))



(define-minor-mode yxl-big-text-mode
  "Bigger text."
  :lighter ""
  (if yxl-big-text-mode
      (let ((scale 1.5))
        (text-scale-increase scale))
    (text-scale-set 0)))



(provide 'yxl-utils)
