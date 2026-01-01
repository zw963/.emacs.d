;; -*- lexical-binding: t; -*-

(require 'project_init)
(require 'dumb-jump)

;; Citre（只需要 xref 集成即可）
;; 如果你还想要 citre 的 capf/imenu，把 nil 改回 t。
(require 'citre nil t)
(setq-default citre-enable-xref-integration t)
(setq-default citre-enable-capf-integration nil)
(setq-default citre-enable-imenu-integration nil)
(setq-default citre-definition-missing-file-mark "")

(setq dumb-jump-prefer-searcher 'rg)
(setq dumb-jump-quiet t)

(defun zw/xref--lsp-active-p ()
  "Non-nil if this buffer is managed by LSP (lsp-mode or eglot)."
  (or (bound-and-true-p lsp-mode)
      (and (fboundp 'eglot-managed-p) (eglot-managed-p))))

(defun zw/project-ctags-file ()
  "Return absolute path to <project-root>/CTAGS if it exists, else nil."
  (when-let ((root (zw/project-root)))
    (let ((p (expand-file-name "CTAGS" root)))
      (when (file-readable-p p) p))))

(defun zw/citre-xref-backend-maybe ()
  "Use Citre as xref backend when LSP is not active and CTAGS exists."
  (unless (zw/xref--lsp-active-p)
    (when (and (featurep 'citre) (fboundp 'citre-xref-backend))
      (when-let ((ctags (zw/project-ctags-file)))
        ;; 明确告诉 Citre 用这个 tags 文件（而不是依赖它的自动搜索逻辑）
        (setq-local citre-tags-file ctags)
        (citre-xref-backend)))))

(defun zw/dumb-jump-xref-backend-maybe ()
  "Use dumb-jump as xref backend when LSP is not active and no CTAGS."
  (unless (zw/xref--lsp-active-p)
    (unless (zw/project-ctags-file)
      (dumb-jump-xref-activate))))

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
  "Install buffer-local xref backends with priority:
1) LSP (by returning nil here when active)
2) Citre (when CTAGS exists)
3) dumb-jump (fallback)"
  ;; Citre 优先于 dumb-jump
  (add-hook 'xref-backend-functions #'zw/citre-xref-backend-maybe nil t)
  (add-hook 'xref-backend-functions #'zw/dumb-jump-xref-backend-maybe t t)

  (unless (local-variable-p 'xref-show-definitions-function)
    (setq-local xref-show-definitions-function
                #'xref-show-definitions-completing-read)))

(add-hook 'prog-mode-hook #'zw/setup-xref-backend)

(provide 'dumb-jump_init)
;;; dumb-jump_init.el ends here
