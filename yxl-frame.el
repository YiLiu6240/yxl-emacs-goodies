(defun yxl-frame-set (&optional frame-name)
  (unless frame-name (setq frame-name "Main"))
  (set-frame-name frame-name)
  (message "Frame name set to %s" frame-name))

(defun yxl-frame-select (frame-name)
  (select-frame-by-name frame-name)
  (message "select Frame %s" frame-name))

;; TODO: accept user input
(defun yxl-frame-select-or-set (frame-name)
  (interactive)
  (cond
   ((equal current-prefix-arg nil)
    (yxl-frame-select frame-name))
   (t
    (yxl-frame-set frame-name))))

(defun yxl-frame-terminal-init (&optional frame)
  "Configs to setup when a terminal frame spawns:
- set bg to be transparent;
- remove menu bar;"
  (when (not (display-graphic-p))
    (progn
      (set-face-background 'default "unspecified-bg" (selected-frame))
      (menu-bar-mode -1))))



(provide 'yxl-frame)
