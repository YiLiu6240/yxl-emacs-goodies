;; provide a helm interface for searching in pdf-tools
;; Author: Yi Liu <https://github.com/YiLiu6240>

(require 'helm)
(require 'pdf-tools)



(setq yxl-hpo-preset-list
      '("previous\|traditional\|critici\|drawback"
        "hypothes\|propose\|question\|problem\|attempt\|issue\|approach\|method"
        "result\|stat\|result\|show\|support"
        "regulat\|supervis\|prudential\|systemic\|policy"
        "contagion\|spillover\|shock"))



(setq yxl-hpo--sources-preset
      '((name . "Preset Keywords")
        (candidates . yxl-hpo-preset-list)
        (action . (("Search" . (lambda (x)
                                 (interactive)
                                 (pdf-occur x t)))))))

(setq yxl-hpo--sources-all
      '((name . "All Keywords")
        (candidates . (("Search all" . yxl-pdf-occur-all-keywords)))
        (action . (("open" . (lambda (x) (call-interactively x)))))))


(setq yxl-hpo--sources-fallback
      (helm-build-sync-source "Fallback"
                              :match (lambda (_candidate) t)
                              :candidates '("Search Pattern")
                              :action (lambda (cand)
                                        (pdf-occur helm-pattern t))))



(defun yxl-pdf-occur-all-keywords ()
  (interactive)
  (pdf-occur (mapconcat 'identity yxl-hpo-preset-list "\|") t))

(defun yxl-helm-pdf-occur ()
  "Three sources:
- `yxl-hpo--sources-preset':
  sources obtained from `yxl-hpo-preset-list', search each preset entry.
- `yxl-hpo--sources-all':
  search a combined list of all entries in `yxl-hpo-preset-list'.
- `yxl-hpo--sources-fallback':
  search patterns provided by user input
"
  (interactive)
  (helm :sources '(yxl-hpo--sources-preset
                   yxl-hpo--sources-all
                   yxl-hpo--sources-fallback)
        :buffer "*helm pdf occur"))



(provide 'yxl-helm-pdf-occur)
