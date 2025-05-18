;; -*- lexical-binding: t; -*-

(require 'color-rg)

(setq color-rg-search-ignore-rules "\
-g \"!*~\" \
-g \"!#*#\" \
-g \"!*.min.*\" \
-g \"!TAGS\" \
-g \"!tags\" \
-g \"!.git/\" \
-g \"!node_modules\" \
-g \"!dist\" \
-g \"!*.log\" \
")

(setq color-rg-search-no-ignore-file nil)
(setq color-rg-show-lines-before-match 5)
(setq color-rg-show-lines-after-match 5)

(add-hook 'color-rg-mode-hook
          (lambda ()
            (define-key color-rg-mode-map (kbd "n") 'color-rg-jump-next-file)
            (define-key color-rg-mode-map (kbd "p") 'color-rg-jump-prev-file)
            (define-key color-rg-mode-map [(f3)] 'color-rg-switch-to-edit-mode)
            ))

;; 默认搜索当前目录下文件，可以按下 o 在上一级目录搜索。
;; 按下 O 在项目根目录下搜索。
(global-set-key [(control meta r)] 'color-rg-search-input)

(provide 'color-rg_init)

;;; color-rg_init.el ends here
