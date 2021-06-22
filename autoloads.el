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

;; graphql-mode
(autoload 'graphql-mode "graphql-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))

(autoload 'systemd-mode "systemd" nil t)
(add-to-list 'auto-mode-alist '("\\.service\\'"  . systemd-mode))
