;; ------------------------------ Theme ------------------------------

;; popular themes: https://emacsthemes.com/popular/index.html

(setq custom-theme-directory (expand-file-name "plugins/themes"))
;; (load-theme 'vscode-dark-plus t)

;; (load-theme 'borland-blue t)

;; (setq zenburn-use-variable-pitch t)
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)
;; (load-theme 'zenburn t)
(require 'modus-themes)
;; (load-theme 'modus-operandi-tinted t)
;; (load-theme 'modus-operandi-deuteranopia t)
(load-theme 'modus-vivendi-tritanopia t)

;; (require 'nano-theme)
;; (load-theme 'nano-dark t)

;; (require ' doom-ayu-moonlight-theme)
;; (load-theme 'doom-ayu-moonlight t)

;; (load-theme 'material t) ;; 这个是原来的
;; (load-theme 'material-light t) ;; 这个是原来的
;; (load-theme 'soothe t)
;; (require 'spacemacs-common)
;; (load-theme 'spacemacs-dark t)
;; (load-theme 'dracula t)                 ;

;; (require 'doom-themes)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t) ; if nil, italics is universally disabled
;; (load-theme 'doom-solarized-dark-high-contrast t)
;; ;; (setq doom-theme 'doom-material)

;; (require 'doom-themes-ext-visual-bell)
;; (doom-themes-visual-bell-config)

;; (require 'doom-themes-ext-org)
;; (doom-themes-org-config)

;; (require 'doom-themes-ext-treemacs)
;; (doom-themes-treemacs-config)

;; (load-theme 'atom-one-dark t)
;; (load-theme 'one-light t)

;; (load-theme 'leuven-dark t)

;; (load-theme 'sanityinc-tomorrow-night t)

;; (load-theme 'danneskjold t)
;; (load-theme 'monokai t)

;; (load-theme 'zerodark t)

;; Optionally setup the modeline, 需要 magit 才可以用.
;; (zerodark-setup-modeline-format)

(provide 'themes_init)

;;; themes_init.el ends here
