;; ------------------------------ Theme ------------------------------

;; popular themes: https://emacsthemes.com/popular/index.html
;; (setq zenburn-use-variable-pitch t)
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)
;; (load-theme 'zenburn t)

(setq custom-theme-directory (expand-file-name "plugins/themes" default-directory))
;; (load-theme 'material t) ;; 这个是原来的
;; (load-theme 'vscode-dark-plus t)
;; (load-theme 'material-light t) ;; 这个是原来的
;; (require 'spacemacs-common)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'dracula t)

(require 'doom-themes)
(load-theme 'doom-one t)
(setq doom-theme 'doom-material)

;; (require 'doom-themes-ext-visual-bell)
;; (doom-themes-visual-bell-config)

;; (require 'doom-themes-ext-treemacs)
;; ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
;; (doom-themes-treemacs-config)
;; (require 'doom-themes-ext-org)
;; (doom-themes-org-config)

;; (load-theme 'atom-one-dark t)
;; (load-theme 'one-light t)

;; (load-theme 'leuven t)

;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-blue t)

;; (load-theme 'danneskjold t)
;; (load-theme 'monokai t)

;; (load-theme 'zerodark t)

;; Optionally setup the modeline, 需要 magit 才可以用.
;; (zerodark-setup-modeline-format)

(provide 'themes_init)

;;; themes_init.el ends here
