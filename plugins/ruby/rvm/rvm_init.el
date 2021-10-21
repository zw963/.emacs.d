(require 'rvm)

(defadvice rvm-use (after set-tags-table-list activate)
  "RVM set \"tags-table-list\" value."
  (add-to-list 'tags-table-list (rvm--locate-file "TAGS"))
  ;; 下面的两个 env 都是 RVM 自动设定的.
  ;; (add-to-list 'tags-table-list (concat (getenv "MY_RUBY_HOME") "/TAGS") t)
  ;; (add-to-list 'tags-table-list (concat (getenv "GEM_HOME") "/TAGS") t)
  (rvm--message (concat "Ruby: " new-ruby " Gemset: " new-gemset)))

(defun use ()
  "RVM use proxy."
  (interactive)
  (rvm-activate-corresponding-ruby)
  )

(require 'goto-gem)

(defadvice goto-gem (before switch-ruby-version activate)
  "Switch rvm before goto gem directory."
  (unless (and rvm--current-ruby rvm--current-gemset)
        (rvm-activate-corresponding-ruby)))

(provide 'rvm_init)
;;; rvm_init.el ends here
