(require 'magit)

(defvar magit-todo-status-keyword-regexp
  "\\<\\(FIXME\\|TODO\\|BUG\\|ISSUE\\|WIP\\|NEXT\\|REVIEW\\):"
  "Regexp pattern used to search for todo keywords.")

(defvar magit-todo-status-ignore-regexp
  ":\\(exclude\\)*.html :\\(exclude\\)*.csv :\\(exclude\\)*.csv.gz"
  "Regexp pattern used to ignore / exlucde things.")

(defvar magit-todo-status-grep-cmd
  "git --no-pager grep --full-name -n --no-color -e \"%s\" -- . %s"
  "git grep command pattern.

The first %s will be substituted by `magit-todo-status-keyword-regexp',
and the send %s will be substituted by `magit-todo-status-ignore-regexp'")

(defun magit-todo-status--get-todos ()
  (let* ((default-directory (locate-dominating-file default-directory ".git")))
    (split-string (shell-command-to-string
                   (format magit-todo-status-grep-cmd
                           magit-todo-status-keyword-regexp
                           magit-todo-status-ignore-regexp))
                  "\n" t)))

(defun magit-todo-status--visit-pos ()
  (interactive)
  (let ((item (string-trim (thing-at-point 'line t))))
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" item)
      (let ((file-name (match-string-no-properties 1 item))
            (line-number (match-string-no-properties 2 item)))
        (find-file file-name)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))))))

(defun magit-todo-status-propertize-item (item)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" item)
    (let ((filename (match-string-no-properties 1 item))
          (line-number (match-string-no-properties 2 item))
          (content (match-string-no-properties 3 item)))
      (concat (propertize filename 'face 'font-lock-type-face)
              ":"
              (propertize line-number 'face 'linum)
              ":"
              (propertize content 'face 'font-lock-comment-face)))))

(defvar magit-todo-status-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] #'magit-todo-status--visit-pos)
    map))

(defun magit-todo-status-insert-items ()
  (let ((items (magit-todo-status--get-todos)))
    (magit-insert-section (todo-status nil t)
      (magit-insert-heading "Todo status:")
      (dolist (item items)
        (magit-insert-section (todo-status item))
        (insert (magit-todo-status-propertize-item item))
        (insert ?\n))
      (insert ?\n))))

(defun magit-todo-stautus-toggle ()
  "Toggle magit todo status section.

This is useful if the git repo is a messy one and it takes too long
to query for the grep."
  (interactive)
  (if (member 'magit-todo-status-insert-items magit-status-sections-hook)
      (progn
        (remove-hook 'magit-status-sections-hook 'magit-todo-status-insert-items)
        (message "magit-todo-status-insert-items has been removed"))
    (progn
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-todo-status-insert-items
       nil
       t)
      (message "magit-todo-status-insert-items has been added"))))

(provide 'magit-todo-status)
