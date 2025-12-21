;; -*- lexical-binding: t; -*-

(require 'smartparens-config)

(sp-local-pair 'web-mode "<#" "#>")
(sp-local-pair 'web-mode "<%" "%>")
(sp-pair "`" "'" :when '(sp-in-comment-p))

;; sp 有一个全局设定，如果单引号 ' 在一个 word 之后，就不会自动 pair
;; 这里在 shell 模式将其关闭。
(sp-local-pair 'sh-mode "'" "'" :unless nil)
(sp-local-pair 'bash-ts-mode "'" "'" :unless nil)

(sp-local-pair 'org-mode "`" "'" :when nil :unless nil) ;; 这个不生效？

;; ;; 关闭 = 自动匹配的行为
;; (sp-local-pair 'org-mode "=" nil :actions nil)

;; (sp-local-pair 'org-mode "`" "'" :actions '(insert wrap))

;; (let ((pairs '(("{" nil) ("[" nil) ("(" nil))))
;;   (mapc
;;    (lambda (pair)
;;      (sp-pair (car pair)
;;               (cadr pair)
;;               :post-handlers
;;               '(:add ("||\n[i]" "RET"))))
;;    pairs))

;; ;; https://github.com/rdallasgray/graphene/blob/master/lib/graphene-smartparens-config.el
;; (sp-local-pair
;;  '(markdown-mode gfm-mode) "*" "*" :unless '(sp-in-string-p) :actions '(insert wrap))

;; (add-hook 'prog-mode-hook #'smartparens-mode)

(smartparens-global-mode)

(provide 'smartparens_init)

;;; smartparens_init.el ends here
