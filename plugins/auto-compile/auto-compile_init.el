;;; -*- no-byte-compile: t -*-

;; 首先， 应该运行 /home/zw963/Dropbox/common/.emacs.d/update_elc 脚本生成 elc
;; 文件, 然后， auto-compile-mode 才会生效.

(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq auto-compile-native-compile t)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)
(setq comp-deferred-compilation t)

(provide 'auto-compile_init)

;;; auto-compile_init.el ends here
