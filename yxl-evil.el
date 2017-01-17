(require 'evil)
(require 'evil-indent-plus)



(evil-define-command yxl-evil-quit (&optional force)
  "Only delete window when there is multiple window open,
otherwise prompt message to confirm to delete frame."
  :repeat nil
  (interactive "<!>")
  (if (one-window-p)
      (let ((delete-p (yes-or-no-p "Delete frame?")))
        (when delete-p (delete-frame)))
    (delete-window)))

(defun evil-insert-newline-around ()
  (interactive)
  (evil-insert-newline-above)
  (forward-line)
  (evil-insert-newline-below)
  (forward-line -1))

(defun evil-insert-space ()
  "identical to vim: i SPC <escape> l"
  (interactive)
  (insert " "))

(defun evil-apend-space ()
  "identical to vim: a SPC <escape> hh"
  (interactive)
  (forward-char 1)
  (insert " ")
  (forward-char -2))

(evil-define-motion evil-sentence-comma-forward (count)
  "Move to next comma"
  :jump t
  :type exclusive
  ;; (evil-find-char (or count 1) ?,)
  (evil-forward-chars "," (or count 1))
  (evil-forward-char 1 t))

(evil-define-motion evil-sentence-comma-backward (count)
  "Move to next comma"
  :jump t
  :type exclusive
  (evil-forward-chars "," (- (or count 1))))



(defun evil-indent-plus--higher-indent-range (&optional point)
  "Return the point at the begin and end of the text block with greater
indentation. If `point' is supplied and non-nil it will return the begin and
end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (point-at-bol))
      (setq end (evil-indent-plus--seek end 1 t t
                                        #'evil-indent-plus--g-or-empty-p))
      (message "begin (%s) end (%s)" begin end)
      (list begin end base))))

(evil-define-text-object evil-indent-chains (&optional count
                                                       beg
                                                       end
                                                       type)
  "Text object describing the block with the same (or greater) indentation
as the current line, skipping empty lines.

Useful for R's magrittr chains.
Select the entire chain when at the root of the chain."
  :type line
  (evil-indent-plus--linify (evil-indent-plus--higher-indent-range)))

(defun yxl-evil-go-up-indent ()
  "Go to the line with less indent level."
  (interactive)
  (let ((base (current-indentation))
        (begin (point)))
    (setq begin (evil-indent-plus--seek begin -1 nil t
                                        #'evil-indent-plus--geq-or-empty-p))
    (goto-char begin)))



(provide 'yxl-evil)
