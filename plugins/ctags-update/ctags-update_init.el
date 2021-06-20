(require 'ctags-update)

(setq
 ctags-update-command "~/utils/ruby_tools/bin/ripper-tags"
 ctags-update-delay-seconds 60 ;; 超过 60 秒，如果有 save 操作，立即升级TAGS, 默认 300 秒.
 ;; 会自动添加 -R, -e 参数以及 -f 完整文件名.
 ctags-update-other-options '("--exclude='*.elc'" "--exclude='*.class'" "--exclude='.git'"
                              "--exclude='.svn'" "--exclude='SCCS'" "--exclude='RCS'"
                              "--exclude='CVS'" "--exclude='EIFGEN'" "--exclude='vendor'"
                              "--exclude='.#*.rb'" "--force" "--extra=q")
 ctags-update-prompt-create-tags nil
 )

;; ctags-auto-update-mode only enable in ruby-mode.
(defun turn-on-ctags-auto-update-mode()
  "turn on `ctags-auto-update-mode'."
  (interactive)
  (when (member major-mode '(ruby-mode enh-ruby-mode))
    (ctags-auto-update-mode 1)))

(add-hook 'ruby-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'enh-ruby-mode-hook 'turn-on-ctags-auto-update-mode)

;;;------------------------------ helm etags+ ------------------------------
;; 第一次使用之前，激活 rvm.
;; (add-hook 'helm-etags-plus-select-hook 'rvm-activate-corresponding-ruby)

(provide 'ctags-update_init)
;;; browse-kill-ring_init.el ends here
