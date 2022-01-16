(require 'rvm)

;; 因为使用 rbtagger, 这个 hack 不再需要了
;; (defadvice rvm-use (after set-tags-table-list activate)
;;   "RVM set \"tags-table-list\" value."
;;   (add-to-list 'tags-table-list (rvm--locate-file "TAGS"))
;;   ;; 下面的两个 env 都是 RVM 自动设定的.
;;   ;; (add-to-list 'tags-table-list (concat (getenv "MY_RUBY_HOME") "/TAGS") t)
;;   ;; (add-to-list 'tags-table-list (concat (getenv "GEM_HOME") "/TAGS") t)
;;   (rvm--message (concat "Ruby: " new-ruby " Gemset: " new-gemset)))

;; 这个有用到吗？暂时先注释掉
;; (require 'goto-gem)
;; (defadvice goto-gem (before switch-ruby-version activate)
;;   "Switch rvm before goto gem directory."
;;   (unless (and rvm--current-ruby rvm--current-gemset)
;;         (rvm-activate-corresponding-ruby)))

(provide 'rvm_init)
;;; rvm_init.el ends here
