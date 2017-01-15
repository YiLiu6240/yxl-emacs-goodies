(require 'browse-url)



;; TODO: test this in windows and mac
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "open")
(if (eq system-type 'gnu/linux)
  (setq browse-url-generic-program "xdg-open"))

(setq-default yxl-web-primary-browser-func browse-url-browser-function)
(setq-default yxl-web-secondary-browser-func #'w3m-goto-url-new-session)



(defun yxl-web-switch-browser ()
  (interactive)
  (if (not (equal browse-url-browser-function yxl-web-primary-browser-func))
      (setq browse-url-browser-function yxl-web-primary-browser-func)
    (setq browse-url-browser-function yxl-web-secondary-browser-func))
  (message "browse-url-browser-function: %s" browse-url-browser-function))



(provide 'yxl-web)
