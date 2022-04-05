(require 'eglot_init)
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(add-hook 'go-mode-hook 'eglot-ensure)

(provide 'eglot-go_init)

;;; eglot-go_init.el ends here
