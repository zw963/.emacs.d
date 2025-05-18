;; -*- lexical-binding: t; -*-

(require 'codeium)

;; 运行 codeium-install 安装 binary 到 ~/.emacs.d/codeium/codeium_language_server

(add-to-list 'completion-at-point-functions #'codeium-completion-at-point)

;; (setq use-dialog-box nil)

;; get codeium status in the modeline
(setq codeium-mode-line-enable
      (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
(add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)

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

(setq codeium/metadata/api_key `,(getenv "CODEIUM_API_KEY"))

(provide 'codeium_init)

;;; codeium_init.el ends here
