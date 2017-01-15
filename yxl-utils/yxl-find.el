(require 'popwin)



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

(provide 'yxl-find)
