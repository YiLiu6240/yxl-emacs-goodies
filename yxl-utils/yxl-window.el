(defvar-local yxl-line-width 80
  "preferred line width for a window, useful when setting window centering")

(defvar yxl-window-stored-layout nil)


(defun yxl-window-popwin-width ()
  "calculate the size of the popwin window, base on current frame width"
  (let ((win-width (round (* (frame-width) 0.20)))
        (min-width 20))
    (cond ((> (frame-width) 100)
           win-width)
          (t
           min-width))))

(defun yxl-window-get-ratio (&optional small)
  "Return the a window width/height corresponding to a ratio to current frame:
- golden ratio by default; if `small' then 1 - golden-ratio;
- 0.75 with C-u uni prefix; if `small' then 0.25;
- C-u C-u: reverse version of C-u;
- 0.X with uni prefix being single digit;
- X% otherwise."
  (let* ((golden-ratio 0.618))
    (cond
     ((equal current-prefix-arg nil)
      (if small (- 1 golden-ratio) golden-ratio))
     ((equal current-prefix-arg  '(4))
      (if small 0.25 0.75))
     ((equal current-prefix-arg  '(16))
      (if small 0.75 0.25))
     (t (cond
         ((< current-prefix-arg 10)(/ current-prefix-arg 10.0))
         (t (/ current-prefix-arg 100.0)))))))

(defun yxl-window-adjust-width-ratio ()
  "get the width size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((main-width (round (* (yxl-window-get-ratio) (frame-width)))))
    (evil-resize-window main-width t)))

(defun yxl-window-adjust-height-ratio ()
  "get the height size of a window that should be expanded to gold ratio"
  (interactive)
  (let* ((main-height (round (* (yxl-window-get-ratio) (frame-height)))))
    (evil-resize-window main-height nil)))



(defun yxl-window-custom-layout1 ()
  "window layout 1 | 2/3"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left))

(defun yxl-window-custom-layout2 ()
  "window layout 1/2 | 3/4"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right)
  (split-window-below)
  (windmove-left)
  (split-window-below))

(defun yxl-window-custom-layout3 ()
  "window layout 1/2 | 3/4/5"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (windmove-right)
  (split-window-below)
  (split-window-below)
  (windmove-left))

(defun yxl-window-vertical-3 ()
  (interactive)
  (split-window-below)
  (split-window-below)
  (progn
    (let* ((main-height (round (* 0.6 (frame-height)))))
      (evil-window-down 1)
      (evil-resize-window main-height nil))))



;; NOTE: split window logic:
;; default: split windows downward/rightward and make the new split window size
;;          the smaller part of the golden ratio of the orig window
;; focus/stay: obvious
;; C-u: 1/4 ratio instead of golden ratio
;; C-u C-u: split upward/leftward

(defun yxl-window-split-horizontal-focus ()
  (interactive)
  (let* ((win-size (- (round (* (yxl-window-get-ratio t)
                                (window-total-height))))))
    (split-window-below win-size)
    (evil-window-down 1)))

(defun yxl-window-split-horizontal-stay ()
  (interactive)
  (let* ((win-size (- (round (* (yxl-window-get-ratio t)
                             (window-total-height))))))
    (split-window-below win-size)))

(defun yxl-window-split-vertical-focus ()
  (interactive)
  (let* ((win-size (- (round (* (yxl-window-get-ratio t)
                             (window-total-width))))))
    (split-window-right win-size)
    (evil-window-right 1)))

(defun yxl-window-split-vertical-stay ()
  (interactive)
  (let* ((win-size (- (round (* ((yxl-window-get-ratio t))
                             (window-total-width))))))
    (split-window-right win-size)))



(defun yxl-window-change-width (width)
  (interactive "nwindow width: ")
  (evil-resize-window width t))

(defun yxl-window-center-margins ()
  "center current window"
  (interactive)
  (let* ((margin (max 0 (/ (- (window-width) yxl-line-width) 2))))
    (set-window-margins nil margin margin)))



(defun yxl-window-get-buffer-previous-window ()
  "open in the current window the buffer in previous window."
  (interactive)
  (set-window-buffer (selected-window) (window-buffer (previous-window))))

(defun yxl-window-record-layout ()
  (interactive)
  (progn
    (setq yxl-window-stored-layout (window-state-get nil t))
    (message "stored window layout.")))

(defun yxl-window-load-layout ()
  (interactive)
  (progn
    (window-state-put yxl-window-stored-layout (frame-root-window))
    (message "load stored window layout.")))



;; support vertical split at the moment
(defun yxl-window-popwin (buffer &optional width position)
  (unless width (setq width 0.3))
  (unless position (setq position 'left))
  (popwin:create-popup-window width position t)
  (select-window (get-buffer-window " *popwin-dummy*"))
  (switch-to-buffer (get-buffer buffer)))



(provide 'yxl-window)
