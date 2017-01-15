(require 'elfeed)
(require 'helm)



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

(defface elfeed-search-star-title-face
  `((t :foreground ,(face-attribute 'bold :foreground)))
  "Marks a starred Elfeed entry.")

(push '(star elfeed-search-star-title-face) elfeed-search-face-alist)

(defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
  "Insert the yanked text from x-selection to kill ring"
  (kill-new (x-get-selection)))

(ad-activate 'elfeed-show-yank)


(defun yxl-elfeed-patch ()
  ;; HACK: highlight current entry name in header
  (defun elfeed-goodies/entry-header-line ()
    (let* ((title (elfeed-entry-title elfeed-show-entry))
           (title-faces 'font-lock-string-face)
           (tags (elfeed-entry-tags elfeed-show-entry))
           (tags-str (mapconcat #'symbol-name tags ", "))
           (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
           (feed (elfeed-entry-feed elfeed-show-entry))
           (entry-author (elfeed-meta elfeed-show-entry :author))
           (feed-title (if entry-author
                           (concat entry-author " (" (elfeed-feed-title feed) ")")
                         (elfeed-feed-title feed)))

           (separator-left (intern (format "powerline-%s-%s"
                                           elfeed-goodies/powerline-default-separator
                                           (car powerline-default-separator-dir))))
           (separator-right (intern (format "powerline-%s-%s"
                                            elfeed-goodies/powerline-default-separator
                                            (cdr powerline-default-separator-dir))))
           (lhs (list
                 (powerline-raw (concat " " (propertize tags-str 'face 'elfeed-search-tag-face) " ") 'powerline-active2 'r)
                 (funcall separator-left 'powerline-active2 'powerline-active1)
                 (powerline-raw (concat " " (propertize title 'face title-faces) " ") 'powerline-active1 'l)
                 (funcall separator-left 'powerline-active1 'mode-line)))
           (rhs (list
                 (funcall separator-right 'mode-line 'powerline-active1)
                 (powerline-raw (concat " " (propertize feed-title 'face 'elfeed-search-feed-face) " ") 'powerline-active1)
                 (funcall separator-right 'powerline-active1 'powerline-active2)
                 (powerline-raw (format-time-string "%Y-%m-%d %H:%M:%S %z " date) 'powerline-active2 'l))))
      (concat
       (powerline-render lhs)
       (powerline-fill 'mode-line (powerline-width rhs))
       (powerline-render rhs))))

  ;; HACK: implement date column as discussed in
  ;; https://github.com/algernon/elfeed-goodies/issues/15
  (defun elfeed-goodies/search-header-draw ()
    "Returns the string to be used as the Elfeed header."
    (if (zerop (elfeed-db-last-update))
        (elfeed-search--intro-header)
      (let* ((separator-left (intern (format "powerline-%s-%s"
                                             elfeed-goodies/powerline-default-separator
                                             (car powerline-default-separator-dir))))
             (separator-right (intern (format "powerline-%s-%s"
                                              elfeed-goodies/powerline-default-separator
                                              (cdr powerline-default-separator-dir))))
             (db-time (seconds-to-time (elfeed-db-last-update)))
             (stats (-elfeed/feed-stats))
             (search-filter (cond
                             (elfeed-search-filter-active
                              "")
                             (elfeed-search-filter
                              elfeed-search-filter)
                             (""))))
        (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
            (search-header/draw-wide separator-left separator-right search-filter stats db-time)
          (search-header/draw-tight separator-left separator-right search-filter stats db-time)))))

  (defun elfeed-goodies/entry-line-draw (entry)
    "Print ENTRY to the buffer."

    (let* ((title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat "[" (mapconcat 'identity tags ",") "]"))
           (title-width (- (window-width) elfeed-goodies/feed-source-column-width
                           elfeed-goodies/tag-column-width 4))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 title-width)
                          :left))
           (tag-column (elfeed-format-column
                        tags-str (elfeed-clamp (length tags-str)
                                               elfeed-goodies/tag-column-width
                                               elfeed-goodies/tag-column-width)
                        :left))
           (feed-column (elfeed-format-column
                         feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width
                                                  elfeed-goodies/feed-source-column-width)
                         :left)))

      (if (>= (window-width) (* (frame-width) elfeed-goodies/wide-threshold))
          (progn
            (insert (propertize date 'face 'elfeed-search-date-face) " ")
            (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
            (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
            (insert (propertize title 'face title-faces 'kbd-help title)))
        (insert (propertize title 'face title-faces 'kbd-help title))))))



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

(defun yxl-helm-elfeed-search ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source "Helm Elfeed Search"
             :candidates yxl-env-elfeed-tag-alist
             ;; NOTE: use `apply' instead of `funcall' for passing arg list
             :action (lambda (x) (apply #'elfeed--read-tag x)))
          ,(helm-build-sync-source "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("Default filter" .
                            (lambda (x) (elfeed--read-tag
                                         (default-value 'elfeed-search-filter))))
                           ("Manual filter" . (lambda (x) (elfeed--read-tag x))))
             :action (lambda (x) (funcall x helm-pattern))))
        :buffer "*Helm Elfeed Search*"))



(provide 'yxl-elfeed)
