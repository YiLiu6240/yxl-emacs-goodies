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

(defun yxl-helm-websites ()
  "
Dependency: `yxl-env-websites-alist'.
An assoc list with elements as (ALIAS . FILE-PATH).
"
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "Websites"
             :candidates yxl-env-websites-alist
             :action (helm-make-actions
                      "open" (lambda (x) (browse-url-generic x))
                      "open-w3m" (lambda (x) (w3m-goto-url-new-session x))))
          ,(helm-build-sync-source
            "Fallback"
             :match (lambda (_candidate) t)
             :candidates '(("yxl-helm-hotspot" . (lambda (x) (yxl-helm-hotspot)))
                           ("Google search" .
                            (lambda (x)
                              (let* ((google-base "http://www.google.com/search?q=%s")
                                     (query-string (replace-regexp-in-string " " "\+" x))
                                     (url-string (format google-base query-string)))
                                (browse-url-generic url-string))))
                           ("Direct Input" . (lambda (x) (browse-url-generic x))))
             :action (lambda (candidate) (funcall candidate helm-pattern))))))

(defun yxl-helm-files ()
  "
Dependency: `yxl-env-files-alist'.
An assoc list with elements as (ALIAS . FILE-PATH).
"
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
            "Files and Directories"
             :candidates yxl-env-files-alist
             :action (helm-make-actions
                      "open" (lambda (x) (find-file x))))
          ,(helm-build-sync-source
            "Helm Quick"
             :match (lambda (_candidate) t)  ;; persistent
             :candidates '(("yxl-helm-hotspot" . yxl-helm-hotspot))
             :action (lambda (candidate) (funcall candidate))))))

(defun yxl-helm-reading-list ()
  "
Dependency:
- `yxl-personal-reading-files-alist': local files (ALIAS . FILE-PATH)
- `yxl-personal-reading-webpages-alist': webpages (ALIAS . FILE-PATH)
"
  (interactive)
  (helm :sources
        `(,(helm-build-sync-source
               "Reading: local files"
             :candidates yxl-personal-reading-files-alist
             :action (helm-make-actions
                      "open" #'find-file
                      "open-alt" #'spacemacs//open-in-external-app))
          ,(helm-build-sync-source
               "Reading: web pages"
             :candidates yxl-personal-reading-webpages-alist
             :action (helm-make-actions
                      "browse" #'browse-url-generic
                      "browse in w3m" #'w3m-goto-url-new-session))
          ,(helm-build-sync-source
               "Helm Quick"
             :match (lambda (_candidate) t)  ;; persistent
             :candidates '(("yxl-helm-hotspot" . yxl-helm-hotspot))
             :action (lambda (candidate) (funcall candidate))))))

(setq yxl-env-helm-hotspot-alist
      '(("yxl-helm-org-files" . yxl-helm-org-files)
        ("yxl-helm-files" . yxl-helm-files)
        ("yxl-helm-websites" . yxl-helm-websites)
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
