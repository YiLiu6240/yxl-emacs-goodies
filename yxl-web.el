(require 'browse-url)

(defvar yxl-web-browser-func-alist
  '(("browse-url-generic" . browse-url-generic)
    ("firefox" . browse-url-firefox)
    ("chrome" . browse-url-chrome)
    ("chromium" . browse-url-chromium)
    ("w3m" . w3m-goto-url-new-session)
    ("qutebrowser" . browse-url-qutebrowser))
  "Alist of browse-url funcs.")

;; TODO: test this in windows and mac
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "open")
(if (eq system-type 'gnu/linux)
    (setq browse-url-generic-program "xdg-open"))

(defun yxl-web-switch-browser ()
  (interactive)
  (ivy-read "Switch browser:"
            yxl-web-browser-func-alist
            :action (lambda (x)
                      (setq browse-url-browser-function (cdr x))))
  (message "browse-url-browser-function: %s" browse-url-browser-function))

(defun browse-url-qutebrowser (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "qutebrowser " url) nil
           "qutebrowser"
           (list url))))

(provide 'yxl-web)
