(require 'fussy)

(fussy-setup)

(setq fussy-score-fn 'fussy-flx-rs-score)
;; (setq fussy-filter-fn 'fussy-filter-orderless-flex)

(flx-rs-load-dyn)

(fussy-company-setup)
(setq helm-completion-style 'emacs)

(provide 'fussy_init)

;;; fussy_init.el ends here
