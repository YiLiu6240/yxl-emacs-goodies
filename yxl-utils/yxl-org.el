(require 'org)



(define-minor-mode yxl-org-task-mode
  "Features for org buffers that are essentially todo list."
  :lighter ""
  (if yxl-org-task-mode
      (let ((scale 0.5))
        (text-scale-decrease scale)
        (org-indent-mode 1))
    (text-scale-set 0)))

(defun yxl-org-task-file-p (&optional file)
  "Return non-nil, if FILE is in `yxl-env-org-task-files'.
If FILE is omitted, use the file associated with the current
buffer."
  (let ((fname (or file (buffer-file-name))))
    (and fname
         (member (file-truename fname)
                 (mapcar #'file-truename yxl-env-org-task-files)))))

(defun yxl-org-format-task-files ()
  "If the current file is in `org-agenda-files',
then turn on `yxl-org-task-mode'"
  (if (yxl-org-task-file-p)
      (yxl-org-task-mode 1)))

(defun yxl-org-open-all-task-files ()
  (interactive)
  (yxl-find-file-open-all yxl-env-org-task-files)
  (if (equal 0 (% (length yxl-env-org-task-files) 2))
      (split-window-below-and-focus)
    (split-window-right-and-focus))
  (org-agenda-list)
  (split-window-right-and-focus)
  (org-todo-list))



(defun yxl-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
    (format "%4d-%02d-%02d %s %s"
            year month day dayname weekstring)))



(defun yxl-org--get-visible-buffers ()
  (let* ((cur-mode 'org-mode))
    (delq nil
          (mapcar
           (lambda (buffer)
             (when (and (equal cur-mode (buffer-local-value 'major-mode buffer))
                        ;; detect visible buffer
                        ;; http://emacs.stackexchange.com/questions/2959/how-to-know-my-buffers-visible-focused-status
                        (get-buffer-window buffer))
               `(,(buffer-file-name buffer) :maxlevel . 1)))
           (buffer-list)))))

(defun yxl-org-refile-visible ()
  (interactive)
  (let* ((visible-org-files (yxl-org--get-visible-buffers))
         (org-refile-targets visible-org-files))
    (call-interactively #'org-refile)))



(provide 'yxl-org)
