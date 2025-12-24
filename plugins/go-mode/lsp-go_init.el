;; -*- lexical-binding: t; -*-

(require 'lsp-mode_init)
(require 'lsp-go)

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'lsp-mode-hook
                      (lambda ()
                        (add-hook 'before-save-hook #'lsp-organize-imports nil t))
                      nil t)
            (lsp-mode-common-hooks)
            ))

;; 为了让远程 debug 成功，需要在项目下面提供一个 .dir-locals.el 文件。
;; 大概下面的样子，替换 "/app" 为容器内 app 的路径。

;; (
;;  (go-mode  . ((eval . (progn
;;                         (dap-register-debug-template
;;                          "Launch Executable trimmed path"
;;                          (list :type "go"
;;                                :request "attach"
;;                                :name "Launch Executable trimmed path"
;;                                :mode "remote"
;;                                :substitutePath (vector (ht ("from" (file-name-directory (buffer-file-name))) ("to" "/app")))))
;;                         ))))
;;  )


;; (require 'dap-mode_init)
;; (require 'dap-dlv-go)

(provide 'lsp-go_init)

;;; lsp-go_init.el end
