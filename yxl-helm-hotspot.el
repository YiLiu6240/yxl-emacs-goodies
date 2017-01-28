(require 'helm)



(defun yxl-helm-org-files ()
  "
Dependency: `yxl-env-org-files'.
A list of file paths.
"
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "Org files"
             :candidates yxl-env-org-files
             :action  (helm-make-actions
                       "open" #'find-file
                       "open other window" #'find-file-other-window))
          ,(helm-build-sync-source
            "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("open all task files" .
                            (lambda (x)
                              (yxl-find-file-open-all yxl-env-org-task-files)))
                           ("open all org files" .
                            (lambda (x)
                              (yxl-find-file-open-all yxl-env-org-files))))
             :action (lambda (candidate) (funcall candidate helm-pattern))))
        :buffer "*helm org agenda*"))

(defun yxl-helm-shortcuts ()
  "
Dependency: `yxl-env-files-alist'.
An assoc list with elements as (ALIAS . FILE-PATH).
"
  (interactive)
  (let ((helm-full-frame t))
    (helm :sources
          `(,(helm-build-in-file-source
                 "Files and Directories"
               yxl-file-sites-local
               :action (helm-make-actions
                        "open" (lambda (x) (find-file x))))
            ,(helm-build-in-file-source
                 "Websites"
               yxl-file-sites-web
               :action (helm-make-actions
                        "open" (lambda (x) (browse-url-generic x))
                        "open-w3m" (lambda (x) (w3m-goto-url-new-session x))))
            ,(helm-build-sync-source
                 "Helm Quick"
               :match (lambda (_candidate) t) ;; persistent
               :candidates '(("yxl-helm-hotspot" . yxl-helm-hotspot)
                             ("Google search" .
                              (lambda (x)
                                (let* ((google-base "http://www.google.com/search?q=%s")
                                       (query-string (replace-regexp-in-string " " "\+" x))
                                       (url-string (format google-base query-string)))
                                  (browse-url-generic url-string)))))
               :action (lambda (candidate) (funcall candidate)))))))

(defun yxl-helm-reading-list ()
  "
Dependency:
- `yxl-file-reading-list-files'
- `yxl-file-reading-list-webpages'
"
  (interactive)
  (let ((helm-full-frame t))
   (helm :sources
        `(,(helm-build-in-file-source
               "Reading: local files"
             yxl-file-reading-list-files
             :action (helm-make-actions
                      "open" #'find-file
                      "open-alt" (lambda (x)
                                   (spacemacs//open-in-external-app
                                    (expand-file-name x)))))
          ,(helm-build-in-file-source
               "Reading: web pages"
             yxl-file-reading-list-webpages
             :action (helm-make-actions
                      "browse" #'browse-url-generic
                      "browse in w3m" #'w3m-goto-url-new-session))
          ,(helm-build-sync-source
               "Helm Quick"
             :match (lambda (_candidate) t)  ;; persistent
             :candidates '(("yxl-helm-hotspot" . yxl-helm-hotspot))
             :action (lambda (candidate) (funcall candidate)))))))

(setq yxl-env-helm-hotspot-alist
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
            :candidates yxl-env-helm-hotspot-alist
            :action (helm-make-actions
                     "open" (lambda (x) (funcall x)))))
        :buffer "*helm yxl hotspot*"))



(provide 'yxl-helm-hotspot)
