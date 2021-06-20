;; (setq auto-indent-on-visit-file nil) ;; 访问文件时，不自动 indent. (默认)
;; (setq auto-indent-untabify-on-visit-file nil) ;; 当打开一个文件时，不转化 TAB 为空格。(默认)
;; (setq auto-indent-indent-style 'moderate) ;; 如果文件不在 repo 中，才自动的 indent. (默认)

;; 打开 org-indent-mode, 这是默认值。(记不得为什么了, 之前关闭了.)
;; (setq auto-indent-start-org-indent t)

;; (setq auto-indent-fix-org-return t) ;; 当在 org-mode 中的代码中时, return 换为 newline-and-indent.(默认)
;; (setq auto-indent-fix-org-backspace t) ;; 当在 org-mode 中的代码中时, 修复 backspace.(默认)
;; (setq auto-indent-fix-org-move-beginning-of-line t) ;; 当在 org-mode 代码块中时，修复 beginning-of-line .(默认)
;; (setq auto-indent-fix-org-yank t) ;; 当在 org-mode 代码块中时，修复 yank .(默认)
;; (setq auto-indent-fix-org-auto-fill t) ;; 当在 org-mode 代码块中时，关闭 auto-fill(默认)
;; (setq auto-indent-mode-untabify-on-yank-or-paste t) ;; yank 的时候，替换 TAB 为空格。(默认)

;; auto-indent-mode 的帮助中写到，这个必须在 yasnippet, autopair 之前。
(require 'auto-indent-mode)
;; (setq auto-indent-next-pair t)
(setq auto-indent-backward-delete-char-behavior 'hungry) ;; Backspace 将会删除任何白空格, 除了换行.
(setq auto-indent-untabify-on-save-file nil) ;; 关闭保存文件时，替换 TAB 为空格.
(auto-indent-global-mode)

;; 执行黏贴操作之前，对内容执行的 hook, 看源码例子。
;; (add-hook 'auto-indent-after-yank-hook 'kicker-ess-fix-path t t)

;; 记住，auto-indent-mode backspace 有问题, 先屏蔽掉。 ????

;; 这些 model, auto-indent-mode 开启会造成复制粘贴格式化缩进错误.
(add-list-to-list '
 auto-indent-disabled-modes-list
 '(
   python-mode
   coffee-mode
   haml-odme
   yaml-mode
   slim-mode
   feature-mode
   web-mode
   gfm-mode
   inf-ruby-mode
   ))

(advice-remove 'beginning-of-visual-line 'ad-Advice-move-beginning-of-line)

(provide 'auto-indent-mode_init)
;;;  auto-indent-mode_init.el ends here
