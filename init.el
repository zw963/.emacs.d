;; -*-Emacs-Lisp-*-

(require 'dash)
(require 'f)
(require 'ht)
(require 's)

;; ==============================开启的功能==============================
(require 'org-mode_init)
(require 'ibuffer_init)
(require 'dired_init)
(require 'shell_init)
(require 'treemacs_init)

(require 'fancy-narrow_init)
(require 'zoom-frm_init)
(require 'back-button_init)
;; (require 'helpful_init)
;; (require 'winum_init)
;; (require 'bm_init)
;; (require 'which-key_init)
(require 'anzu_init)
(require 'beacon_init)
(require 'avy_init)
(require 'doom-modeline_init)
(require 'rich-minority_init)
(require 'show-point-mode_init)
(require 'hydra_init);; depend by treemacs
(require 'edit-server_init)
(require 'iedit_init)
(require 'undo-tree_init)
(require 'toggle-quotes_init)
(require 'beyond-compare_init)
;; (require 'ido_init)
(require 'helm_init)
(require 'helm-bindings_init) ; 和 ivy_init 冲突， 二者选其一.
;; (require 'ivy_init)
;; (require 'prescient_init) ;; 这个必须放在 ivy_init 后面.
(require 'yasnippet_init)
(require 'wgrep_init)
(require 'quickrun_init)
(require 'amx_init)
;; (require 'spaceline_init)

;; (require 'valign_init)
(require 'cnfonts_init)
;; (require 'find-file-in-project_init)
(require 'vterm_init)
(require 'async_init)
(require 'mu4e_init)
(require 'volatile-highlights_init)
(require 'buffer-move_init)
(require 'goto-chg_init)
(require 'symbol-overlay_init)
(require 'color-rg_init)
(require 'markdown-mode_init)

;; ============================== 编程相关 ==============================

;; (require 'awesome-tab_init)
(require 'prog-mode_init)
;; (require 'apheleia_init)
(require 'git_init)
;; (require 'reformatter_init)
(require 'flycheck_init)
(require 'company_init)
(require 'dart-mode_init)
(require 'smartparens_init)
(require 'string-inflection_init)
(require 'highlight-indentation_init)
(require 'highlight-escape-sequences_init)
(require 'rainbow-delimiters_init)
(require 'expand-region_init)
(require 'ruby_init)
(require 'crystal-mode_init)
;; (require 'mini-frame_init)
(require 'rainbow-mode_init)
(require 'scss-css-mode_init)
;; (require 'tree-sitter_init)
(require 'format-all_init)

(require 'dpaste_init)
(require 'ws-butler_init)
;; (require 'solaire-mode_init) ;; 这个似乎没见有用过

(require 'whitespace-cleanup-mode_init)

;; 关闭一些有用，但最近不常用的 mode
(require 'go-mode_init)
(require 'rust_init)
;; (require 'lsp-bridge_init)
(require 'haskell-mode_init)
;; (require 'js2-mode_init)
;; (require 'lua-mode_init)
;; (require 'tide_init)

;; 加载 dotfiles 时，阻止 gc.
;; (when (file-exists-p (expand-file-name ".emacs" config))
;;   (let ((gc-cons-threshold 20000000))
;;    (load ".emacs")))

;; 这个似乎要放到最后面？
(require 'posframe_init)
(require 'shackle_init)
(require 'popper_init)
;; (require 'burly_init)
(require 'all-the-icons_init)
;; (require 'dashboard_init)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'sgml-mode-hook 'display-line-numbers-mode)
(add-hook 'feature-mode-hook 'display-line-numbers-mode)
(add-hook 'yaml-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
;; (setq display-line-numbers 'relative)


(setenv "LOAD_INIT" "true")
