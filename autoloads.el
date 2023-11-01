;; php-mode
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))

;; nginx-mode
(autoload 'nginx-mode "nginx-mode" nil t)
(add-to-list 'auto-mode-alist '("/nginx/.*\\.conf" . nginx-mode))

;; crontab-mode
(autoload 'crontab-mode "crontab-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cron\\(tab\\)?\\'" . crontab-mode))
(add-to-list 'auto-mode-alist '("cron\\(tab\\)?\\."    . crontab-mode))

;; ;; dockerfile-mode
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

(autoload 'web-mode "web-mode_init" nil t)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ecr\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.wxml$" . web-mode))

(add-hook 'yaml-ts-mode-hook 'display-line-numbers-mode)
(add-to-list 'auto-mode-alist '("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\.j2" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("Procfile\\|Procfile.dev\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("Procfile.options\\|Procfile.dev.options\\'" . yaml-ts-mode))

;; (add-to-list 'auto-mode-alist '("\\.json" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js" . js-ts-mode))

(autoload 'toml-mode "toml-mode_init" nil t)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\(\\.d/.*\\.conf\\)?\\'" . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\(\\.d/.*\\.conf\\)?\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

(autoload 'graphql-mode "graphql-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.graphql\\'" . graphql-mode))
(add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode))

(autoload 'just-mode "just-mode" nil t)
(add-to-list 'auto-mode-alist '("/[Jj]ustfile\\'" . just-mode))
(add-to-list 'auto-mode-alist '("\\.[Jj]ust\\(file\\)?\\'" . just-mode))

(autoload 'mint-mode "mint-mode_init" nil t)
(add-to-list 'auto-mode-alist '("\\.mint\\'" . mint-mode))

(autoload 'lua-mode "lua-mode" nil t)
(setq lua-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
