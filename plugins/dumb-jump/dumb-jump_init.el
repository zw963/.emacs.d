;; -*- lexical-binding: t; -*-

(require 'dumb-jump)

(setq dumb-jump-prefer-searcher 'rg)
(setq dumb-jump-quiet t)

(defun zw/xref--lsp-active-p ()
  "Non-nil if this buffer is managed by LSP (lsp-mode or eglot)."
  (or (bound-and-true-p lsp-mode)
      (and (fboundp 'eglot-managed-p) (eglot-managed-p))))

(defun zw/dumb-jump-xref-maybe ()
  "Use dumb-jump as xref backend only when LSP is not active."
  (unless (zw/xref--lsp-active-p)
    (dumb-jump-xref-activate)))

(defun zw/etags-xref-backend-maybe ()
  "Enable etags backend only when a TAGS table is already configured."
  (when (and (fboundp 'etags--xref-backend)
             (or tags-file-name
                 (and (boundp 'tags-table-list) tags-table-list)))
    (etags--xref-backend)))
;; 2) 用“条件 etags”替换默认全局 etags，避免没 TAGS 时弹窗
(setq-default xref-backend-functions (list #'zw/etags-xref-backend-maybe))

;; 另一个更暴力、简单的解决办法是, 将 dumb-jump 加入到默认的 etags 的前面。
;; 即，修改下面的 t t 为 nil t
;; (add-hook 'xref-backend-functions #'zw/dumb-jump-xref-maybe nil t)

(defun zw/setup-xref-backend ()
  ;; 关键点：
  ;; 1) 用我们自己的 maybe 函数，而不是直接 dumb-jump-xref-activate
  ;; 2) append=t：把 dumb-jump 放到 backend 列表末尾，作为兜底
  ;; 3) local=t：只影响当前 buffer
  (add-hook 'xref-backend-functions #'zw/dumb-jump-xref-maybe t t)
  (unless (local-variable-p 'xref-show-definitions-function)
    (setq-local xref-show-definitions-function
                #'xref-show-definitions-completing-read)))

(add-hook 'prog-mode-hook 'zw/setup-xref-backend)

(provide 'dumb-jump_init)
;;; dumb-jump_init.el ends here
