(require 'yxl-frame)
(require 'yxl-buffer)
(require 'yxl-find)
(require 'shell-pop)

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

(defun insert-GBP-symbol ()
  (interactive)
  (insert "£"))

(defun insert-EUR-symbol ()
  (interactive)
  (insert "€"))

(defun insert-YEN-symbol ()
  (interactive)
  (insert "¥"))

(define-minor-mode yxl-big-text-mode
  "Bigger text."
  :lighter ""
  (if yxl-big-text-mode
      (let ((scale 1.3))
        (text-scale-increase scale))
    (text-scale-set 0)))

(defun yxl-terminal-transparency ()
  "If in terminal environment, remove bg color of `default' face."
  (interactive)
  (if (display-graphic-p)
      (message "not a terminal client")
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun yxl-shell-invoke ()
  "A poor man's shell-pop, till shell-pop's bugs are fixed."
  (interactive)
  (let ((cwd default-directory)
        (invoke-shell-nix (lambda (cwd)
                            (shell)
                            (goto-char (point-max))
                            (comint-kill-input)
                            (insert (concat "cd " cwd))
                            (let ((comint-process-echoes t))
                              (comint-send-input))
                            (recenter 0)))
        (invoke-shell-win (lambda (cwd)
                            (eshell)
                            (shell-pop--cd-to-cwd-eshell cwd)))
        (invoke-shell (lambda (cwd)
                        (if (eq system-type 'windows-nt)
                            (funcall invoke-shell-win cwd)
                          (funcall invoke-shell-nix cwd)))))
    (if current-prefix-arg
        (funcall invoke-shell cwd)
      (progn
        (split-window-below)
        (windmove-down)
        (funcall invoke-shell cwd)))))

(defun yxl-insert-symbol (symbol)
  "Insert symbol at the current place of the cursor, with proper padding.
When the preceding character is not a whitespace, insert a whitespace; the same
applies to the following character as well."
  (interactive)
  (let* ((char-ahead (string (preceding-char)))
         (char-behind (string (following-char)))
         (left-pad (unless (equal char-ahead " ") " "))
         (right-pad (unless (equal char-behind " ") " ")))
    (insert (concat left-pad symbol right-pad))))

(provide 'yxl-utils)
