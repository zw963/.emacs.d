(require 'js2-mode)
(require 'xref-js2)

;; (setq js2-basic-offset 2)
;; (setq js-indent-level 2)
;; (setq js2-mode-show-parse-errors nil)
;; (setq js2-mode-show-strict-warnings nil)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.\\(js\\.erb\\|js\\|es6\\)$" . js2-mode))

;; M-, runs the command xref-pop-marker-stack
;; M-. runs the command xref-find-definitions
;; M-? runs the command xref-find-references

(add-hook 'js2-mode-hook
          (lambda ()
            ;;; unbind js2-mode default M-.
            (define-key js2-mode-map (kbd "M-.") nil)
            ;;; add xref backend.
            (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)
            ))

;; (setq inferior-js-program-command "js") ;; default node
;; (setq inferior-js-program-arguments '("--interactive"))

(require 'js2-imenu-extras)
(js2-imenu-extras-setup)

;; 运行 js-commit-repl
(require 'js-comint)
(add-hook 'js2-mode-hook
          (lambda ()
            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
            (local-set-key "\C-cb" 'js-send-buffer)
            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
            (local-set-key "\C-cl" 'js-load-file-and-go)
            ))

(provide 'js2-mode_init)
;;;  js2-mode_init ends here
