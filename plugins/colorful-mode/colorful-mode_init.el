;; -*- lexical-binding: t; -*-

(require 'colorful-mode)

(setq colorful-use-prefix t)
;; 这个变量来自于 css-mode 派生的模式, 关闭 Emacs 默认背景颜色显示，
;; 才会让 colorful-use-prefix 变量生效
(setq css-fontify-colors nil)

(setq colorful-only-strings 'only-prog)

(add-to-list 'global-colorful-modes 'helpful-mode)
(add-to-list 'global-colorful-modes 'scss-mode)
(add-to-list 'global-colorful-modes 'scss-css-mode)
(add-to-list 'global-colorful-modes 'less-css-mode)
(add-to-list 'global-colorful-modes 'web-mode)
(add-to-list 'global-colorful-modes 'mint-mode)
(add-to-list 'global-colorful-modes 'js-mode)
(add-to-list 'global-colorful-modes 'js2-mode)
(add-to-list 'global-colorful-modes 'js3-mode)
(add-to-list 'global-colorful-modes 'js3-mode)
(add-to-list 'global-colorful-modes 'crystal-mode)

(global-colorful-mode t)

(provide 'colorful-mode_init)

;;; colorful-mode_init.el ends here
