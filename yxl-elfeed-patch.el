(defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
  "Insert the yanked text from x-selection to kill ring"
  (kill-new (x-get-selection)))

(ad-activate 'elfeed-show-yank)

(defun yxl-elfeed-patch ()

  (defun search-header/draw-wide (separator-left separator-right search-filter stats db-time)
    (let* ((update (format-time-string "%Y-%m-%d %H:%M:%S %z" db-time))
           (lhs (list
                 (powerline-raw (-pad-string-to "Date" (- elfeed-goodies/feed-source-column-width 10)) 'powerline-active1 'l)
                 (powerline-raw (-pad-string-to "Feed" (- elfeed-goodies/feed-source-column-width 4)) 'powerline-active1 'l)
                 (funcall separator-left 'powerline-active1 'powerline-active2)
                 (powerline-raw (-pad-string-to "Tags" (- elfeed-goodies/tag-column-width 6)) 'powerline-active2 'l)
                 (funcall separator-left 'powerline-active2 'mode-line)
                 (powerline-raw "Subject" 'mode-line 'l)))
           (rhs (search-header/rhs separator-left separator-right search-filter stats update)))

      (concat (powerline-render lhs)
              (powerline-fill 'mode-line (powerline-width rhs))
              (powerline-render rhs))))

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

(provide 'yxl-elfeed-patch)
