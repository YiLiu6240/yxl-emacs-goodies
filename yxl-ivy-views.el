(require 'ivy)
(require 'hydra)

(defvar yxl-ivy-views-storage-location nil
  "File to store ivy views.")

(defun buffer-file-name-relative-home ()
  (format "~/%s" (file-relative-name (buffer-file-name) (getenv "HOME"))))

(defun default-directory-relative-home ()
  (format "~/%s" (file-relative-name default-directory (getenv "HOME"))))

(defun yxl-ivy-push-view ()
  "Push the current window tree on `ivy-views'.
Currently, the split configuration (i.e. horizonal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
  (interactive)
  (let* ((view (cl-labels ((ft (tr)
                             (if (consp tr)
                                 (if (eq (car tr) t)
                                     (cons 'vert
                                           (mapcar #'ft (cddr tr)))
                                   (cons 'horz
                                         (mapcar #'ft (cddr tr))))
                               ;; HACK: record path as relative to ${HOME}
                               (with-current-buffer (window-buffer tr)
                                 (cond ((buffer-file-name)
                                        (list 'file (buffer-file-name-relative-home) (point)))
                                       ((eq major-mode 'dired-mode)
                                        (list 'file (default-directory-relative-home) (point)))
                                       (t
                                        (list 'buffer (buffer-name) (point))))))))
                 (ft (car (window-tree)))))
         (view-name (ivy-read "Name view: " nil
                              :initial-input (ivy-default-view-name))))
    (when view-name
      (push (list view-name view) ivy-views))))

;; http://stackoverflow.com/questions/30568113/result-value-of-elisp-code-stored-in-a-file
(defun yxl-ivy-views-load ()
  (interactive)
  (when (y-or-n-p (format "Load ivy views from %s?" yxl-ivy-views-storage-location))
   (setq ivy-views (with-current-buffer
                      (find-file-noselect yxl-ivy-views-storage-location)
                    (goto-char (point-min))
                    (read (current-buffer))))))

(defun yxl-ivy-views-save ()
  (interactive)
  (when (y-or-n-p (format "Save ivy views to %s?" yxl-ivy-views-storage-location))
    (write-region (prin1-to-string ivy-views) nil yxl-ivy-views-storage-location)))

(defun yxl-ivy-views-switch ()
  (interactive)
  (let ((ivy-use-virtual-buffers t))
    ;; TODO: only read ivy-views
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy--switch-buffer-matcher
              :preselect "{}"
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

(provide 'yxl-ivy-views)
