;; php-mode
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))

;; nginx-mode
(autoload 'nginx-mode "nginx-mode" nil t)
(add-to-list 'auto-mode-alist '("/nginx/.*\\.conf" . nginx-mode))

;; crontab-mode
(autoload 'crontab-mode "crontab-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;; dockerfile-mode
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\.dockerfile\\'" . dockerfile-mode))

;; apache-mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

(autoload 'systemd-mode "systemd" nil t)
(add-to-list 'auto-mode-alist '("\\.service\\'"  . systemd-mode))

;; markdown-mode
;; (require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode" nil t)

(add-hook 'markdown-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-x n"))
            (define-key markdown-mode-map [(control c) (return)] 'markdown-preview)
            (define-key markdown-mode-map [(meta c) (n)] 'markdown-narrow-to-subtree)
            ))
(setq markdown-command "pulldown-cmark"
      markdown-open-command "pulldown-cmark"
      markdown-gfm-use-electric-backquote nil
      markdown-indent-on-enter 'indent-and-new-item
      markdown-content-type "text/html"
      markdown-coding-system 'utf-8
      markdown-gfm-uppercase-checkbox t
      )

(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.mkdn$\\|\\.mkd$\\|\\.mdown$" . gfm-mode))

(autoload 'web-mode "web-mode_init" nil t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.wxml$" . web-mode))

(autoload 'yaml-mode "yaml-mode_init" nil t)

(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\.j2" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-to-list 'auto-mode-alist '("Procfile\\|Procfile.dev\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("Procfile.options\\|Procfile.dev.options\\'" . yaml-mode))

;; (provide 'markdown-mode_init)
;;; markdown-mode_init.el ends here
