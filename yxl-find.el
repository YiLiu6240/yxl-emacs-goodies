(require 'popwin)
(require 'counsel)
(require 'yxl-open)

(defun yxl-find-file-popup (file)
  (interactive)
  (let ((pop-width (yxl-window-popwin-width)))
    (cond
     ((equal current-prefix-arg '(4))
      (find-file file))
     ((equal current-prefix-arg '(16))
      (find-file-other-window file))
     (t
      (yxl-window-popwin (find-file-noselect file) pop-width 'left)))))

(defun yxl-find-file-stay (file)
  (interactive)
  (let ((pop-width (yxl-window-popwin-width)))
    (cond
     ((equal current-prefix-arg '(4))
      (yxl-window-popwin (find-file-noselect file) pop-width 'left))
     ((equal current-prefix-arg '(16))
      (find-file-other-window file))
     (t
      (find-file file)))))

(defun yxl-find-file-open-all (file-list)
  "TODO: add doc"
  (let* ((file-len (length file-list))
         (action-list1 '(split-window-right-and-focus
                         (lambda ()
                           (split-window-below-and-focus)
                           (evil-window-move-very-bottom))))
         (action-list2 (mapcar (lambda (x)
                                 (if (/= (% x 2) 0)
                                     (car action-list1)
                                   (car (last action-list1))))
                               (number-sequence 1 (- file-len 1))))
         (action-list3 (cons nil action-list2))
         (final-alist (mapcar* 'cons file-list action-list3)))
    (delete-other-windows)
    (mapc (lambda (x)
            (when (cdr x)
              (funcall (cdr x)))
            (find-file (car x)))
          final-alist)))

(defun yxl-find-dir (path)
  "Instead of directly going to `path', feed files in `path'
to `counsel-find-file'."
  (let ((default-directory path))
    (if current-prefix-arg
        (find-file path)
      (counsel-find-file))))

(defun yxl-find-file-counsel (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion.

Differ with `counsel-find-file' that this function preselect current file."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (let ((find-file-hook (if (and
                                           counsel-find-file-speedup-remote
                                           (file-remote-p ivy--directory))
                                          nil
                                        find-file-hook)))
                  (find-file (expand-file-name x ivy--directory)))))
            :preselect (when buffer-file-name (file-name-nondirectory buffer-file-name))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file))

(provide 'yxl-find)
