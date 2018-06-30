(require 'ace-window)
(require 'ivy)

(defvar yxl-ace-window--delete-orig-window nil
  "If t, then all window commands will delete the original window.")

(defun yxl-ace-window--push-window (new-window)
  (let ((new-frame (window-frame new-window))
        (orig-buffer (window-buffer (selected-window)))
        (orig-window (selected-window))
        (window-pos (window-point)))
    (when (and (frame-live-p new-frame)
               (not (eq new-frame (selected-frame))))
      (select-frame-set-input-focus new-frame))
    (if (window-live-p new-window)
        (progn
          (select-window new-window)
          (set-window-buffer new-window orig-buffer)
          (set-window-point new-window window-pos)
          (if yxl-ace-window--delete-orig-window
              (delete-window orig-window)))
      (error "Got a dead window %S" new-window))))

(defun yxl-ace-window--fetch-window (new-window)
  (let ((new-frame (window-frame new-window))
        (orig-window (selected-window))
        (orig-buffer (window-buffer (selected-window)))
        (new-buffer (window-buffer new-window))
        (window-pos (window-point new-window)))
    (if (window-live-p new-window)
        (progn
          (set-window-buffer orig-window new-buffer)
          (if yxl-ace-window--delete-orig-window
              (delete-window new-window))
          (set-window-point window-pos))
      (error "Got a dead window %S" new-window))))

(defun yxl-ace-window-push-window ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg))
    (aw-select " Ace - push current window to destination"
               #'yxl-ace-window--push-window)))

(defun yxl-ace-window-fetch-window ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg))
    (aw-select " Ace - fetch current window from destination"
               #'yxl-ace-window--fetch-window)))

(defun yxl-ace-window-open (file)
  (interactive)
  (aw-select " Ace - open current file in destination window"
             (lambda (new-window)
               (let ((new-frame (window-frame new-window)))
                 (when (and (frame-live-p new-frame)
                            (not (eq new-frame (selected-frame))))
                   (select-frame-set-input-focus new-frame))
                 (if (window-live-p new-window)
                     (progn
                       (select-window new-window)
                       (find-file file))
                   (error "Got a dead window %S" new-window))))))

(defun yxl-ace-window-open-vert (file)
  (interactive)
  (aw-select " Ace - open current file in destination window"
             (lambda (new-window)
               (let ((new-frame (window-frame new-window)))
                 (when (and (frame-live-p new-frame)
                            (not (eq new-frame (selected-frame))))
                   (select-frame-set-input-focus new-frame))
                 (if (window-live-p new-window)
                     (progn
                       (select-window new-window)
                       (split-window-right)
                       (windmove-right)
                       (find-file file))
                   (error "Got a dead window %S" new-window))))))

(defun yxl-ace-window-open-horz (file)
  (interactive)
  (aw-select " Ace - open current file in destination window"
             (lambda (new-window)
               (let ((new-frame (window-frame new-window)))
                 (when (and (frame-live-p new-frame)
                            (not (eq new-frame (selected-frame))))
                   (select-frame-set-input-focus new-frame))
                 (if (window-live-p new-window)
                     (progn
                       (select-window new-window)
                       (split-window-below)
                       (windmove-down)
                       (find-file file))
                   (error "Got a dead window %S" new-window))))))

(provide 'yxl-ace-window)
