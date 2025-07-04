;; -*- lexical-binding: t; -*-

;; 使用 emacs 编辑 textarea 中的内容, 需要插件支持, 支持 Firefox 和 Chrome
(require 'edit-server)
;; (setq edit-server-url-major-mode-alist '(("github\\.com" . markdown-mode)
;;                                          ("ruby-china" . markdown-mode)
;;                                          ("gitlab" . markdown-mode)))
(setq edit-server-port 9289)
(setq edit-server-default-major-mode 'markdown-mode)
(when (and (require 'edit-server nil t) (daemonp))
  (edit-server-start)
  (add-hook 'edit-server-start-hook (lambda ()
                                      (local-set-key [(control c) (return)] 'org-ctrl-c-ret)
                                      (local-set-key [(control c) (?\r)] 'org-ctrl-c-ret)
                                      (define-key edit-server-edit-mode-map [(control x) (control s)] 'edit-server-done)
                                      (define-key edit-server-edit-mode-map [(control c) (control c)] 'nil)
                                      ))
  )

;; (require 'atomic-chrome)
;; (setq atomic-chrome-default-major-mode 'markdown-mode)
;; (setq atomic-chrome-buffer-open-style 'frame)
;; ;; (setq atomic-chrome-extension-type-list '(ghost-text))
;; (atomic-chrome-start-server)

(provide 'edit-server_init)
;;;  edit-server_init.el ends here
