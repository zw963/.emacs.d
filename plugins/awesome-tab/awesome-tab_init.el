(require 'awesome-tab)

(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*flycheck" name)
     (string-prefix-p "*etags-plus:" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.

Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (cond
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode
                            )))
     "Emacs")
    ((derived-mode-p 'eshell-mode)
     "EShell")
    ((derived-mode-p 'emacs-lisp-mode)
     "Elisp")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode")
    ((derived-mode-p 'eaf-mode)
     "EAF")

    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common")
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help")
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail")
    ((memq major-mode
           '(dev-mode
             ruby-mode enh-ruby-mode rhtml-mode web-mode feature-mode
             js-mode js2-mode yaml-mode
             haml-mode slim-mode css-mode scss-mode coffee-mode
             sh-mode emacs-lisp-mode makefile-bsdmake-mode
             sql-mode nxml-mode markdown-mode conf-unix-mode-hook
             ))
     "Develop")
    (t
     (awesome-tab-get-group-name (current-buffer))))))

(awesome-tab-mode t)

(global-set-key [(meta \0)] 'awesome-tab-forward-tab)
(global-set-key [(control tab)] 'awesome-tab-forward-tab)
(global-set-key [(meta \9)] 'awesome-tab-backward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'awesome-tab-backward-tab)

(global-set-key [(control meta \9)] 'awesome-tab-move-current-tab-to-left)
(global-set-key [(control meta \0)] 'awesome-tab-move-current-tab-to-right)

(setq awesome-tab-show-tab-index t)
;; (setq awesome-tab-index-format-str '')

;; 这些数字是 visible 数字，不是所有 tab 的实际索引。
(global-set-key [(meta \1)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \2)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \3)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \4)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \5)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \6)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \7)] 'awesome-tab-select-visible-tab)
(global-set-key [(meta \8)] 'awesome-tab-select-visible-tab)

(with-eval-after-load 'helm
  (awesome-tab-build-helm-source))

(provide 'awesome-tab_init)

;;; awesome-tab_init.el ends here
