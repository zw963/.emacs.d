(require 'org)
(require 'org-table)
;; 允许将 org-mode 导出到 markdown, yep!
;; 命令: org-md-export-as-markdown
(require 'ox-md)

;; (setq org-fontify-quote-and-verscmee-blocks nil
;;       org-fontify-whole-heading-line nil
;;       org-hide-leading-stars nil)

(setq org-startup-with-inline-images t)

;; (setq org-completion-use-ido t)
(setq org-hide-leading-stars t) ;隐藏刚开始的*符号
(setq org-table-auto-blank-field nil)

(add-hook 'org-mode-hook
          (
           lambda ()
           (local-set-key [(return)] 'org-return-indent)
           (local-set-key (kbd "RET") 'org-return-indent)
           (outline-minor-mode 1)
           ))


;; 当 export org-table 到 markdown 时, 不要转义 _ 字符, 但是表格内输出仍然是 \_
;; (setq org-export-with-sub-superscripts nil)

;; org-mode 下面,  `?????' 和 emacs-lisp-mode 一样, 高亮暗绿色显示
(font-lock-add-keywords
 'org-mode '(("`\\([^`'\n]+\\)'" (1 font-lock-constant-face))))

;; 这个用来复制 org 里面的代码块。
(defun org-copy-src-block-link()
  (interactive)
  (forward-line -2)
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

(require 'org-superstar)
(setq org-hide-leading-stars nil)
(setq org-superstar-leading-bullet ?\s)
(setq org-indent-mode-turns-on-hiding-stars nil)
(add-hook 'org-mode-hook #'org-superstar-mode)

;; 快捷键：C-c C-j 打开新的 entry, C-c C-k 保存并关闭
(require 'org-journal)
(setq org-journal-dir (expand-file-name "org/journal" default-directory))
;; (setq org-journal-date-prefix "#+TITLE: ")
;; (setq org-journal-time-prefix "* ")
(setq org-journal-date-format "%F, %A")
(setq org-journal-time-format "%T ")
(setq org-journal-file-format "%Y/%m.org")
(setq org-journal-file-type 'Monthly)
(setq org-journal-enable-agenda-integration t)
(setq org-journal-enable-cache t)

(provide 'org-mode_init)
;;; org-mode_init.el ends here
