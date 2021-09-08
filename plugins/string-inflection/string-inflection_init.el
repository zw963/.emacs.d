(require 'string-inflection)

(defun my-string-inflection-cycle-auto ()
  "switching by major-mode"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(global-set-key [(shift meta u)] 'my-string-inflection-cycle-auto)

;; (require 'sequential-command)
;; (defun backward-capitalize-word ()
;;   (interactive)
;;   (subword-capitalize (- (1+ (seq-count*)))))

;; (defun backward-upcase-word ()
;;   (interactive)
;;   (subword-upcase (- (1+ (seq-count*)))))

;; (global-set-key [(meta u)] 'backward-upcase-word) ; 连续标记光标前面的单词大写 Alt-u
;; (global-set-key [(meta U)] 'backward-capitalize-word) ;连续标记光标前面单词首字母大写 Alt-U

(provide 'string-inflection-init)

;;; string-inflection_init.el ends here
