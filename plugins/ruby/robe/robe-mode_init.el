;; -*- lexical-binding: t; -*-

;; 注意，robe 的一个坑时，有时候环境需要某些 env 被正确设定，才能正确启动 irb.
;; 因此，为了让这个环境变量生效，我不得不在当前项目文件夹内，重新启动 Emacs.

;; M-. robe-jump, 查找指定的方法. (替代了 helm-ctags+)
;; M-, pop-tag-mark 返回查找前的点.

(require 'inf-ruby_init)
(require 'robe)

;; C-c C-l, 重新载入当前文件。
;; C-c C-k, 重新载入整个 Rails 项目。
;; 自动判断, 快捷键: C-c C-l
(defun robe-reload-enhanced ()
  (interactive)
  (if (rinari-root)
      (robe-rails-refresh)
    (ruby-load-file (buffer-file-name))))

(define-key inf-ruby-minor-mode-map (kbd "C-c C-l") 'robe-reload-enhanced)

(run-ruby-mode-hook '(robe-mode))

(with-eval-after-load 'company
  (require 'company-robe)
  (run-ruby-mode-hook '(progn
                         (make-local-variable 'company-backends)
                         (push 'company-robe company-backends)
                         )))

(provide 'robe-mode_init)
;;; robe-mode_init.el ends here
