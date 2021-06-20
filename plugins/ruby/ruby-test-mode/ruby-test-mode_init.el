;; 注意: ruby-test-mode 是高度 hack 的一个 model, 不要随便删除.

(require 'ruby-test-mode)

(setq ruby-test-default-library "test")
(add-to-list 'ruby-test-file-name-extensions "axlsx")

(defun ruby-test-toggle ()
  (interactive)
  ;; basic-save-buffer-1 不会调用 save-hook.
  (when (buffer-modified-p) (basic-save-buffer-1))
  (ruby-test-toggle-implementation-and-specification))

(global-set-key [(control t)] 'ruby-test-toggle)

(provide 'ruby-test-mode_init)
;;;  ruby-test-mode_init.el ends here
