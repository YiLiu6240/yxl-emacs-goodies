(require 'ace-window)
(require 'ivy)

(defvar yxl-ace-window--delete-orig-window nil
  "If t, then all window commands will delete the original window.")

(defvar yxl-ace-window--split-direction nil
  "If horz, split horizontally; if vert, split vertically.")

(defun yxl-ace-window--push-window (new-window)
  (let ((target-frame (window-frame new-window))
        (target-window new-window)
        (orig-buffer (window-buffer (selected-window)))
        (orig-window (selected-window))
        (window-pos (window-point)))
    (when (and (frame-live-p target-frame)
               (not (eq target-frame (selected-frame))))
      (select-frame-set-input-focus target-frame))
    (if (window-live-p new-window)
        (progn
          (select-window new-window)
          (if (eq yxl-ace-window--split-direction 'horz)
              (progn
                (split-window-below)
                (windmove-down)
                (setq target-window (selected-window))))
          (if (eq yxl-ace-window--split-direction 'vert)
              (progn
                (split-window-right)
                (windmove-right)
                (setq target-window (selected-window))))
          (set-window-buffer target-window orig-buffer)
          (set-window-point target-window window-pos)
          (if yxl-ace-window--delete-orig-window
              (delete-window orig-window)))
      (error "Got a dead window %S" new-window))))

(defun yxl-ace-window--fetch-window (new-window)
  (let ((target-frame (window-frame new-window))
        (target-window (selected-window))
        (orig-window (selected-window))
        (orig-buffer (window-buffer (selected-window)))
        (new-buffer (window-buffer new-window))
        (window-pos (window-point new-window)))
    (if (window-live-p new-window)
        (progn
          (if (eq yxl-ace-window--split-direction 'horz)
              (progn
                (split-window-below)
                (windmove-down)
                (setq target-window (selected-window))))
          (if (eq yxl-ace-window--split-direction 'vert)
              (progn
                (split-window-right)
                (windmove-right)
                (setq target-window (selected-window))))
          (set-window-buffer target-window new-buffer)
          (if yxl-ace-window--delete-orig-window
              (delete-window new-window))
          (set-window-point window-pos))
      (error "Got a dead window %S" new-window))))

(defun yxl-ace-window-push-window ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg))
    (aw-select " Ace - push current window to destination"
               #'yxl-ace-window--push-window)))

(defun yxl-ace-window-push-window-horz ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg)
        (yxl-ace-window--split-direction 'horz))
    (aw-select " Ace - push current window to destination"
               #'yxl-ace-window--push-window)))

(defun yxl-ace-window-push-window-vert ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg)
        (yxl-ace-window--split-direction 'vert))
    (aw-select " Ace - push current window to destination"
               #'yxl-ace-window--push-window)))

(defun yxl-ace-window-fetch-window ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg))
    (aw-select " Ace - fetch current window from destination"
               #'yxl-ace-window--fetch-window)))

(defun yxl-ace-window-fetch-window-horz ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg)
        (yxl-ace-window--split-direction 'horz))
    (aw-select " Ace - fetch current window from destination"
               #'yxl-ace-window--fetch-window)))

(defun yxl-ace-window-fetch-window-vert ()
  (interactive)
  (let ((yxl-ace-window--delete-orig-window current-prefix-arg)
        (yxl-ace-window--split-direction 'vert))
    (aw-select " Ace - fetch current window from destination"
               #'yxl-ace-window--fetch-window)))

(defun yxl-ace-window-open (file)
  (interactive)
  (aw-select " Ace - open current file in destination window"
             (lambda (new-window)
               (let ((target-frame (window-frame new-window)))
                 (when (and (frame-live-p target-frame)
                            (not (eq target-frame (selected-frame))))
                   (select-frame-set-input-focus target-frame))
                 (if (window-live-p new-window)
                     (progn
                       (select-window new-window)
                       (find-file file))
                   (error "Got a dead window %S" new-window))))))

(defun yxl-ace-window-open-vert (file)
  (interactive)
  (aw-select " Ace - open current file in destination window"
             (lambda (new-window)
               (let ((target-frame (window-frame new-window)))
                 (when (and (frame-live-p target-frame)
                            (not (eq target-frame (selected-frame))))
                   (select-frame-set-input-focus target-frame))
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
               (let ((target-frame (window-frame new-window)))
                 (when (and (frame-live-p target-frame)
                            (not (eq target-frame (selected-frame))))
                   (select-frame-set-input-focus target-frame))
                 (if (window-live-p new-window)
                     (progn
                       (select-window new-window)
                       (split-window-below)
                       (windmove-down)
                       (find-file file))
                   (error "Got a dead window %S" new-window))))))

(provide 'yxl-ace-window)
