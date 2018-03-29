(require 'magit)

(defvar magit-todo-status-keyword-regexp
  "\\<\\(FIXME\\|TODO\\|BUG\\|ISSUE\\|WIP\\|NEXT\\):"
  "Regexp pattern used to search for todo keywords.")

(defvar magit-todo-status-ignore-regexp
  ":!*.html"
  "Regexp pattern used to ignore / exlucde things.")

(defvar magit-todo-status-grep-cmd
  "git --no-pager grep --full-name -n --no-color -e \"%s\" \"%s\""
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
        (insert item)
        (insert ?\n))
      (insert ?\n))))

(provide 'magit-todo-status)
