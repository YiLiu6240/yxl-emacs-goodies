(require 'dired)
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



(provide 'yxl-dired)
