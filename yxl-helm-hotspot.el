(require 'helm)

(defvar yxl-hhs-org-files nil
  "org files")

(defvar yxl-hhs-file-local-list nil
  "local files")

(defvar yxl-hhs-file-web-list nil
  "local files")

(defvar yxl-hhs-file-reading-list-local nil
  "local files")

(defvar yxl-hhs-file-reading-list-webpages nil
  "local files")

(defun yxl-helm-org-files ()
  "
A list of file paths.
"
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "Org files"
             :candidates yxl-hhs-org-files
             :action  (helm-make-actions
                       "open" #'find-file
                       "open other window" #'find-file-other-window))
          ,(helm-build-sync-source
            "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("open all files" .
                            (lambda (x)
                              (yxl-find-file-open-all yxl-hhs-org-files))))
             :action (lambda (candidate) (funcall candidate helm-pattern))))
        :buffer "*helm org agenda*"))

(defun yxl-helm-shortcuts ()
  "
An assoc list with elements as (ALIAS . FILE-PATH).
"
  (interactive)
  (let ((helm-full-frame t))
    (helm :sources
          `(,(helm-build-in-file-source
                 "Files and Directories"
               yxl-hhs-file-local-list
               :action (helm-make-actions
                        "open" (lambda (x) (find-file x))))
            ,(helm-build-in-file-source
                 "Websites"
               yxl-hhs-file-web-list
               :action (helm-make-actions
                        "open" (lambda (x) (browse-url-generic x))
                        "open-w3m" (lambda (x) (w3m-goto-url-new-session x))))
            ,(helm-build-sync-source
                 "Fallback"
               :match (lambda (_candidate) t) ;; persistent
               :candidates '(("open: local" .
                              (lambda ()
                                (find-file yxl-hhs-file-local-list)))
                             ("open: webpages" .
                              (lambda ()
                                (find-file yxl-hhs-file-web-list)))
                             ("yxl-helm-hotspot" . yxl-helm-hotspot)
                             ("Google search" .
                              (lambda ()
                                (let* ((google-base "http://www.google.com/search?q=%s")
                                       (query-string (replace-regexp-in-string " " "\+" helm-pattern))
                                       (url-string (format google-base query-string)))
                                  (browse-url-generic url-string)))))
               :action (lambda (candidate) (funcall candidate)))))))

(defun yxl-helm-reading-list ()
  "
reading list.
"
  (interactive)
  (let ((helm-full-frame t))
   (helm :sources
        `(,(helm-build-in-file-source
               "Reading: local files"
             yxl-hhs-file-reading-list-local
             :action (helm-make-actions
                      "open" #'find-file
                      "open-alt" (lambda (x)
                                   (spacemacs//open-in-external-app
                                    (expand-file-name x)))))
          ,(helm-build-in-file-source
               "Reading: web pages"
             yxl-hhs-file-reading-list-webpages
             :action (helm-make-actions
                      "browse" #'browse-url-generic
                      "browse in w3m" #'w3m-goto-url-new-session))
          ,(helm-build-sync-source
               "Fallback"
             :match (lambda (_candidate) t)  ;; persistent
             :candidates '(("open: local" .
                            (lambda ()
                              (find-file yxl-hhs-file-reading-list-local)))
                           ("open: webpages" .
                            (lambda ()
                              (find-file yxl-hhs-file-reading-list-webpages)))
                           ("yxl-helm-hotspot" . yxl-helm-hotspot))
             :action (lambda (candidate) (funcall candidate)))))))

(setq yxl-hhs--entry-list
      '(("yxl-helm-org-files" . yxl-helm-org-files)
        ("yxl-helm-shortcuts" . yxl-helm-shortcuts)
        ("yxl-helm-reading-list" . yxl-helm-reading-list)
        ("calendar" . cfw-open-calendar)
        ("calculator" . (lambda ()
                          (helm-calcul-expression)))
        ("rss" . elfeed)
        ("helm-github-stars" . helm-github-stars)
        ("helm-show-kill-ring" . helm-show-kill-ring)
        ("helm-all-mark-rings" . helm-all-mark-rings)))

(defun yxl-helm-hotspot ()
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "My hotspot"
            :candidates yxl-hhs--entry-list
            :action (helm-make-actions
                     "open" (lambda (x) (funcall x)))))
        :buffer "*helm yxl hotspot*"))

(provide 'yxl-helm-hotspot)
