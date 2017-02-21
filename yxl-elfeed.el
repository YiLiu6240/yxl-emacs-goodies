(require 'elfeed)
(require 'yxl-elfeed-patch)
(require 'helm)
(require 'ivy)

(defvar yxl-elfeed-tag-alist nil
  "Tag list for search. Refer to `yxl-elfeed-tag-builtin-alist' as an example
of how tag list is constructed.")

(defvar yxl-elfeed-tag-builtin-alist '(("relevant" . ("relevant" t))
                                       ("important" . ("important" t))
                                       ;; this one is not appended
                                       ("star" . ("+star" nil)))
  "Built-in tag alist.")

(defvar yxl-elfeed-score-alist nil "list for keyword scores")

(defface elfeed-search-star-entry
  `((t :weight bold :underline ,(face-attribute
                                 'font-lock-function-name-face :foreground)))
  "Marks a starred Elfeed entry.")

(push '(star elfeed-search-star-entry) elfeed-search-face-alist)

;; http://kitchingroup.cheme.cmu.edu/blog/category/elfeed/
(defface elfeed-search-relevant-entry
  `((t :weight bold :underline ,(face-attribute
                                 'font-lock-type-face :foreground)))
  "Marks a relevant elfeed entry.")

(defface elfeed-search-important-entry
  `((t :weight bold :underline ,(face-attribute
                                 'font-lock-variable-name-face :foreground)))
  "Marks a relevant elfeed entry.")

(push '(relevant elfeed-search-relevant-entry) elfeed-search-face-alist)

(push '(important elfeed-search-important-entry) elfeed-search-face-alist)

(defun zilong/elfeed-mark-all-as-read ()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun yxl-elfeed-mark-as-read ()
  (interactive)
  (elfeed-search-untag-all 'unread))

(defun yxl-elfeed-mark-as-unread ()
  (interactive)
  (elfeed-search-tag-all 'unread))

(defun elfeed-toggle-shr-inhibit-images ()
  "toggle the value of shr-inhibit-images"
  (interactive)
  (if (equal shr-inhibit-images t)
      (setq shr-inhibit-images nil)
    (setq shr-inhibit-images t))
  (message "shr-inhibit-images: %s" shr-inhibit-images))

(defalias 'elfeed-toggle-star
  (elfeed-expose #'elfeed-search-toggle-all 'star))

;; http://heikkil.github.io/blog/2015/02/24/custom-elfeed-filter-functions/
(defun elfeed--read-tag (filter &optional append)
  "Template for filtering feed categories.

FILTER is the filter string to apply.

The cursor is moved to the beginning of the first feed line."
  (if append
      (setq elfeed-search-filter (concat (default-value 'elfeed-search-filter)
                                         " +"
                                         filter))
    (setq elfeed-search-filter filter))
  (elfeed-search-update :force)
  (goto-char (point-min))
  (forward-line)
  (message (concat "elfeed-search-filter: " elfeed-search-filter)))

;; http://kitchingroup.cheme.cmu.edu/blog/category/elfeed/
(defun score-elfeed-entry (entry)
  (let ((title (elfeed-entry-title entry))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))
    (loop for (pattern n) in yxl-elfeed-score-alist
          if (string-match pattern title)
          do (incf score n)
          if (string-match pattern content)
          do (incf score n))
    (message "%s - %s" title score)

    ;; ;; store score for later in case I ever integrate machine learning
    ;; (setf (elfeed-meta entry :my/score) score)

    (cond
     ((= score 1)
      (elfeed-tag entry 'relevant))
     ((> score 1)
      (elfeed-tag entry 'important)))
    entry))

(add-hook 'elfeed-new-entry-hook #'score-elfeed-entry)

(defun yxl-elfeed-rm-tag ()
  "rm tag from entries
If no prefix arg: select tags from database; otherwise asks for input"
  (interactive)
  (if current-prefix-arg
      (elfeed-search-untag-all)
      (ivy-read "tag to rm from entry: "
            (elfeed-db-get-all-tags)
            :action (lambda (x)
                      (elfeed-search-untag-all (intern x)))
            :caller 'yxl-elfeed-counsel-rm-tag)))

(defun yxl-elfeed-add-tag ()
  "add tag from entries
If no prefix arg: select tags from database; otherwise asks for input"
  (interactive)
  (if current-prefix-arg
      (elfeed-search-tag-all)
    (ivy-read "tag to add to entry: "
              (elfeed-db-get-all-tags)
              :action (lambda (x)
                        (elfeed-search-tag-all (intern x)))
              :caller 'yxl-elfeed-counsel-rm-tag)))

(defun yxl-elfeed-helm-search ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
               "User provided tags"
             :candidates yxl-elfeed-tag-alist
             ;; NOTE: use `apply' instead of `funcall' for passing arg list
             :action (lambda (x) (apply #'elfeed--read-tag x)))
          ,(helm-build-sync-source
               "Built-in tags"
             :candidates yxl-elfeed-tag-builtin-alist
             :action (lambda (x) (apply #'elfeed--read-tag x)))
          ,(helm-build-sync-source
               "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("Default filter" .
                            (lambda (x) (elfeed--read-tag
                                         (default-value 'elfeed-search-filter))))
                           ("Default filter: append" .
                            (lambda (x) (elfeed--read-tag
                                         (concat (default-value 'elfeed-search-filter) " " x))))
                           ("Current filter: append" .
                            (lambda (x) (elfeed--read-tag (concat elfeed-search-filter " " x))))
                           ("Manual filter" . (lambda (x) (elfeed--read-tag x))))
             :action (lambda (x) (funcall x helm-pattern))))
        :buffer "*Helm Elfeed Search*"))

(provide 'yxl-elfeed)
