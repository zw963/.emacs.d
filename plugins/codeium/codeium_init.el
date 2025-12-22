;; -*- lexical-binding: t; -*-

(setq codeium/metadata/api_key `,(getenv "CODEIUM_API_KEY"))

(require 'codeium)

;; 运行 codeium-install 安装 binary 到 ~/.emacs.d/codeium/codeium_language_server

;; (setq use-dialog-box nil)

;; get codeium status in the modeline
;; (setq codeium-mode-line-enable
;;       (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
;; (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

(setq codeium-api-enabled
      (lambda (api)
        (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

(defun my-codeium/document/text ()
  (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
(setq codeium/document/text 'my-codeium/document/text)

;; warning: this is measured by UTF-8 encoded bytes
(defun my-codeium/document/cursor_offset ()
  (codeium-utf8-byte-length
   (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
(setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)

;; 下面的这个是自定义激活方案, cape_init.el 里面还有一个混合 lsp 和 codeium 一起输出的方案。
;; 如果确实慢，可以考虑使用这个。

(with-eval-after-load 'codeium
  (defun zw/codeium-complete ()
    "Call Codeium completion only (does not affect other CAPFs)."
    (interactive)
    (let ((completion-at-point-functions '(codeium-completion-at-point)))
      (completion-at-point)))

  ;; 例如：给一个你顺手的键
  (global-set-key (kbd "C-c i") #'zw/codeium-complete))

(
(provide 'codeium_init)

;;; codeium_init.el ends here
