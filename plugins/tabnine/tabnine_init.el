;; (require 'tabnine-capf)
;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)

(require 'company-tabnine)
;; (setq company-tabnine-binaries-folder (expand-file-name "plugins/company-mode/plugins/company-tabnine/"))

;; 注意：在编辑器内，输入 Tabnine::config 使用浏览器打开配置页面。
(defun add-tabnine-backend ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'company-tabnine)
  )

(add-hook 'sh-mode-hook 'add-tabnine-backend)
(add-hook 'makefile-gmake-mode-hook 'add-tabnine-backend)

;; (with-eval-after-load 'rust-mode (add-hook 'rust-mode-hook 'add-tabnine-backend))
;; (with-eval-after-load 'rustic-mode (add-hook 'rustic-mode-hook 'add-tabnine-backend))
(with-eval-after-load 'crystal-mode (add-hook 'crystal-mode-hook 'add-tabnine-backend))
;; (with-eval-after-load 'ruby-mode (add-hook 'ruby-mode-hook 'add-tabnine-backend))
;; dart-mode 开启的话，很卡
;; (with-eval-after-load 'dart-mode (add-hook 'dart-mode-hook 'add-tabnine-backend))

;; (run-ruby-mode-hook '(add-tabnine-backend))

(provide 'tabnine_init)

;;; tabnine_init.el ends here
