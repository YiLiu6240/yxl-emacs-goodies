(require 'elfeed)
(require 'yxl-elfeed-patch)
(require 'helm)



(defvar yxl-elfeed-tag-alist nil "tag list for search.")



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

(defun yxl-elfeed-helm-search ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "Helm Elfeed Search"
            :candidates yxl-elfeed-tag-alist
            ;; NOTE: use `apply' instead of `funcall' for passing arg list
            :action (lambda (x) (apply #'elfeed--read-tag x)))
          ,(helm-build-sync-source
            "Fallback"
            :match (lambda (_candidate) t)
            :candidates '(("Default filter" .
                           (lambda (x) (elfeed--read-tag
                                        (default-value 'elfeed-search-filter))))
                          ("Manual filter" . (lambda (x) (elfeed--read-tag x))))
            :action (lambda (x) (funcall x helm-pattern))))
        :buffer "*Helm Elfeed Search*"))



(provide 'yxl-elfeed)
