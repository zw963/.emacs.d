(require 'string-inflection)

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for ruby
   ((member major-mode '(ruby-mode enh-ruby-mode))
    (string-inflection-ruby-style-cycle))
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((member major-mode '(java-mode dart-mode js2-mode))
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-all-cycle)
    )))

(global-set-key [(meta u)] 'my-string-inflection-cycle-auto)

(require 'sequential-command)
(require 'subword)
(defun backward-capitalize-word ()
  (interactive)
  (subword-capitalize (- (1+ (seq-count*)))))

(defun backward-upcase-word ()
  (interactive)
  (subword-upcase (- (1+ (seq-count*)))))

(global-set-key [(shift meta u)] 'backward-upcase-word) ; 连续标记光标前面的单词大写 Alt-u
;; (global-set-key [(meta U)] 'backward-capitalize-word) ;连续标记光标前面单词首字母大写 Alt-U

(provide 'string-inflection_init)

;;; string-inflection_init.el ends here
