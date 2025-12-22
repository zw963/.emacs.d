;; -*- lexical-binding: t; -*-

(require 'smartrep)

;; used by back-button and diff-hl
;; hacked for work with emacs 30.1

;; (smartrep-define-key
;;     global-map "C-x"
;;   '(("{" . shrink-window-horizontally)
;;     ("}" . enlarge-window-horizontally)))

(smartrep-define-key
    global-map "C-c z"
  '(("=" . text-scale-increase)
    ("-" . text-scale-decrease)
    ("0" . my/text-scale-reset)
    ))

(provide 'smartrep_init)

;;; smartrep_init.el ends here
