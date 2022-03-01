(require 'smart-jump)

(defun smart-jump-ruby-robe-available-p ()
  "Return whether or not `robe' is available."
  (bound-and-true-p robe-mode))

(defun smart-jump-ruby-mode-register ()
  "Register `smart-jump' for `ruby-mode'."
  (smart-jump-register :modes 'robe-mode
                       :jump-fn #'smart-jump-ruby-robe-available-p
                       :should-jump t
                       :heuristic 'point
                       :order 1
                       )

  ;; (smart-jump-register :modes '(ruby-mode enh-ruby-mode)
  ;;                      :jump-fn 'helm-etags-plus-select
  ;;                      :pop-fn 'helm-etags-plus-history-go-back
  ;;                      :should-jump t
  ;;                      :heuristic 'point
  ;;                      :order 2
  ;;                      )
  )

(smart-jump-ruby-mode-register)

;; (smart-jump-register :modes '(ruby-mode enh-ruby-mode robe-mode)
;;                      :jump-fn #'robe-jump
;;                      :refs-fn 'smart-jump-simple-find-references
;;                      :should-jump #'smart-jump-ruby-robe-available-p
;;                      :heuristic 'point
;;                      :order 1
;;                      )

(provide 'smart-jump_init)

;;; smart-jump_init.el ends here
