(require 'iedit)
(setq-default iedit-occurrence-context-lines 0)
(setq iedit-auto-narrow t)
(require 'iedit-rect)
;; 开启 iedit-rectangle-mode 之后, 可以直接编辑, 编辑完记住要再次快捷键退出模式.
(global-set-key [(control meta \7)] 'iedit-rectangle-mode)

(provide 'iedit_init)
;;; iedit_init.el ends here
