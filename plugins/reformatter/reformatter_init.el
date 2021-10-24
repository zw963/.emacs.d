(require 'reformatter)

(with-eval-after-load 'dart-mode
  (reformatter-define dart-format
                      :program "dart"
                      :args '("format"))
  ;; 自动定义了以下方法:
  ;; dart-format-buffer dart-format-region
  ;; 以及一个 minor-mode
  ;; dart-format-on-save-mode
  (add-hook 'dart-mode-hook (lambda ()
                               (dart-format-on-save-mode 1)
                               (define-key dart-mode-map (kbd "C-c C-c") 'dart-format-buffer)
                              ))
  )

(provide 'reformatter_init)

;;; reformatter_init.el ends here
