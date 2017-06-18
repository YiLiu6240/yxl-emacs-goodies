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
  "A list of file paths."
  (interactive)
  (helm :sources
        `(((name . "Org files")
           (candidates . ,yxl-hhs-org-files)
           (action . (("open" lambda (x) (find-file x))
                      ("open other window" lambda (x) (find-file-other-window x)))))
          ((name . "Others")
           (match . (lambda (_candidate) t))
           (candidates
            . (("Open all files"
                . (lambda (x)
                    (yxl-find-file-open-all yxl-hhs-org-files)))
               ("Hotspot"
                . (lambda (x)
                    (yxl-helm-hotspot)))))
           (action . (("open" lambda (x) (funcall x helm-pattern))))))
        :buffer "*helm org agenda*"))

(defun yxl-helm-shortcuts ()
  "Shortcuts."
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
  "Reading list."
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
               :candidates '(("edit: local" .
                              (lambda ()
                                (find-file yxl-hhs-file-reading-list-local)))
                             ("edit: webpages" .
                              (lambda ()
                                (find-file yxl-hhs-file-reading-list-webpages)))
                             ("yxl-helm-hotspot" . yxl-helm-hotspot))
               :action (lambda (candidate) (funcall candidate)))))))

(defvar yxl-hhs--entry-list
  '(((name . "My Own Hotspot")
     (candidates . (("yxl-helm-org-files" . yxl-helm-org-files)
                    ("yxl-helm-shortcuts" . yxl-helm-shortcuts)
                    ("yxl-helm-reading-list" . yxl-helm-reading-list)))
     (action . (("open" lambda (x) (funcall x)))))
    ((name . "Emacs")
     (candidates . (("calendar" . cfw-open-calendar)
                    ("calculator" . (lambda ()
                                      (helm-calcul-expression)))
                    ("rss" . elfeed)))
     (action . (("open" lambda (x) (funcall x)))))
    ((name . "Other Helm")
     (candidates . (("helm-github-stars" . helm-github-stars)
                    ("helm-show-kill-ring" . helm-show-kill-ring)
                    ("helm-all-mark-rings" . helm-all-mark-rings)
                    ("helm-chrome-bookmarks" . helm-chrome-bookmarks)))
     (action . (("open" lambda (x) (funcall x)))))))

(defun yxl-helm-hotspot ()
  (interactive)
  (helm :sources yxl-hhs--entry-list
        :buffer "*helm yxl hotspot*"))

(provide 'yxl-helm-hotspot)
