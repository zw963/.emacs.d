(require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
;; (setq gofmt-command "goimports")
;; (add-hook 'before-save-hook 'gofmt-before-save)

;; ;; Set up before-save hooks to format buffer and add/delete imports.
;; ;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

(defun my-go-mode-hook ()
  (lsp-deferred)
  (lsp-go-install-save-hooks)
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; (if (not (string-match "go" compile-command))
;;     (set (make-local-variable 'compile-command)
;;          "go build -v && go test -v && go vet"))

;; (require 'go-autocomplete)

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)
;; mode-compile support

(with-eval-after-load 'mode-compile
  (add-to-list 'mode-compile-modes-alist '(go-mode . (go-compile nil)))
  (setq go-command "go")
  (setq go-dbg-flags "run")
  (defun go-compile ()
    (interactive)
    (mc--shell-compile go-command go-dbg-flags nil)))

;; The Go Oracle will blow your mind! It can do things like find all the callers of a
;; given function/method. It can also show you all the functions that read or write
;; from a given channel. In short, it rocks.
;; http://tleyden.github.io/blog/2014/05/27/configure-emacs-as-a-go-editor-from-scratch-part-2/

(provide 'go-mode_init)
;;; go-mode_init.el ends here
