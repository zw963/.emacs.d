(require 'org)
(require 'org-table)
;; 允许将 org-mode 导出到 markdown, yep!
;; 命令: org-md-export-as-markdown
(require 'ox-md)

;; 听说可以让性能好一些?
(remove-hook 'org-mode-hook #'org-superstar-mode)

(setq org-hide-leading-stars nil)

;; (setq org-fontify-quote-and-verscmee-blocks nil
;;       org-fontify-whole-heading-line nil
;;       org-hide-leading-stars nil)

(setq org-completion-use-ido t)
(setq org-hide-leading-stars t) ;隐藏刚开始的*符号
(setq org-table-auto-blank-field nil)

(add-hook 'org-mode-hook
          (
           lambda ()
           (local-set-key [(return)] 'org-return-indent)
           (local-set-key (kbd "RET") 'org-return-indent)
           ))


;; 当 export org-table 到 markdown 时, 不要转义 _ 字符, 但是表格内输出仍然是 \_
;; (setq org-export-with-sub-superscripts nil)

;; org-mode 下面,  `?????' 和 emacs-lisp-mode 一样, 高亮暗绿色显示
(font-lock-add-keywords
 'org-mode '(("`\\([^`'\n]+\\)'" (1 font-lock-constant-face))))

;; 这个用来复制 org 里面的代码块。
(defun org-copy-src-block-link()
  (forward-line -2)
  (interactive)
  (org-edit-src-code)
  (mark-whole-buffer)
  (with-no-warnings
    (kill-ring-save (region-beginning) (region-end) t))
  (org-edit-src-abort))
(setq org-link-elisp-confirm-function nil)
;; (setq org-confirm-babel-evaluate nil)
(add-to-list 'org-babel-load-languages '(ruby . t))
(add-to-list 'org-babel-load-languages '(rust . t))

(require 'ox-publish)
(setq org-publish-project-alist
      '(
        ("org-notes"
         :base-directory "~/Dropbox/common/.emacs.d/org/publish"
         :base-extension "org"
         :publishing-directory "~/Dropbox/common/.emacs.d/org/publish"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ("org-static"
         :base-directory "~/Dropbox/common/.emacs.d/org/publish"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/Dropbox/common/.emacs.d/org/publish"
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("resume"
         :base-directory "~/Dropbox/common/.emacs.d/org/简历"
         :base-extension "org"
         :publishing-directory "~/Dropbox/common/.emacs.d/org/简历/resume"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4             ; Just the default for this project.
         :auto-preamble t
         )
        ("org" :components ("org-notes" "org-static"))
        ;; ... add all the components here (see below)...
        ))

(require 'org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(provide 'org-mode_init)
;;; org-mode_init.el ends here
