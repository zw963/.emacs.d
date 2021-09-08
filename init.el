;; -*-Emacs-Lisp-*-

;; ------------------------------显示相关设置------------------------------
;; popular themes: https://emacsthemes.com/popular/index.html
;; (setq zenburn-use-variable-pitch t)
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)
;; (load-theme 'zenburn t)

(load-theme 'material t)
;; (require 'doom-themes)
;; (load-theme 'doom-one t)

;; (load-theme 'atom-one-dark t)
;; (load-theme 'one-light t)

;; (load-theme 'leuven-dark t)

;; (require 'color-theme-sanityinc-tomorrow)
;; (load-theme 'sanityinc-tomorrow-blue t)

;; (require 'spacemacs-common)
;; (load-theme 'spacemacs-dark t)

;; (load-theme 'danneskjold t)
;; (load-theme 'monokai t)

;; Optionally setup the modeline, 需要 magit 才可以用.
;; (load-theme 'zerodark t)
;; (zerodark-setup-modeline-format)
;; ==============================开启的功能==============================

(require 'flyspell_init)
(require 'org-mode_init)
(require 'ibuffer_init)
(require 'dired_init)
(require 'all-the-icons_init)
(require 'shell_init)
(require 'flycheck_init)
(require 'quickrun_init)
(require 'treemacs_init)
;; 下面两个库都针对 kill-ring-save, kill-region 等函数添加了 device.
;; 因为判断选区时, 总是首先判断 rect-mark, 因此要确保 rect-mark_init 在
;; browse-kill-ring 之后 require, 行为才正确.
(require 'browse-kill-ring_init)
(require 'rect-mark_init)
(require 'fancy-narrow_init)
(require 'yafolding_init)
(require 'avy_init)
(require 'beacon_init)
(require 'which-key_init)
(require 'zoom-frm_init)
(require 'smart-mode-line_init)
;; (require 'eyebrowse_init)
(require 'winum_init)
(require 'hydra_init);; depend by treemacs
(require 'move-dup_init)
(require 'auto-indent-mode_init)
;; (require 'aggressive-indent_init)
(require 'edit-server_init)
(require 'markdown-mode_init)
(require 'iedit_init)
(require 'multiple-cursors_init)
(require 'undo-tree_init)
(require 'toggle-quotes_init)
(require 'shackle_init)
(require 'posframe_init)
(require 'beyond-compare_init)
(require 'helm_init) ;; helm 放到 ivy 后面，可以覆盖某些 ivy 覆盖的快捷键。
(require 'helm-bindings_init) ; 和 ivy_init 冲突， 二者选其一.
(require 'ivy_init)
(require 'company_init)
(require 'yasnippet_init)
(require 'prescient_init) ;; 这个必须放在 ivy_init 后面.
(require 'anzu_init)
(require 'pangu-spacing_init)

;; (require 'ido_init)
;; (require 'popwin_init)
;; (require 'amx_init)
;; (require 'eglot_init) ----------
;; (require 'solargraph_init) -----------
;; (require 'quickrun_init)
;; (require 'spaceline_init)
;; (require 'awesome-tray_init)
;; (require 'awesome-tab_init)
;; (require 'awesome-pair_init)
;; (require 'super-save_init)
;; (require 'workgroups_init)
;; (require 'pdf-tools_init)
;; (require 'valign_init)
(require 'find-file-in-project_init)
(require 'vterm_init)
;; (require 'auto-complete_init)
;; (require 'english-teacher_init)
(require 'async_init)
(require 'mu4e_init)
(require 'zoom_init)

;; ============================== 编程相关 ==============================

(require 'highlight-indentation_init)
(require 'highlight-escape-sequences_init)
(require 'rainbow-delimiters_init)
(require 'yaml-mode_init)
(require 'toml-mode_init)
(require 'lua-mode_init)
(require 'expand-region_init)
;; git related packages
(require 'git-emacs_init)
(require 'git-timemachine_init)
(require 'diff-hl_init)
;; (require 'git-gutter_init)
(require 'git-messenger_init)
(require 'gist_init)
(require 'ruby_init)
(require 'coffee-mode_init)
(require 'ctags-update_init)
(require 'web-mode_init)
(require 'rainbow-mode_init)
;; (require 'smart-jump_init)
(require 'scss-css-mode_init)
(require 'js2-mode_init)
(require 'json-mode_init)
;; (require 'tree-sitter_init)
(require 'dap-mode_init)
(require 'lsp-mode_init)
(require 'dart-mode_init)
(require 'reformatter_init)
(require 'rust-mode_init)
;; (require 'rustic_init)
(require 'haskell-mode_init)
(require 'go-mode_init)
(require 'dpaste_init)
(require 'ws-butler_init)
;; 这个老是运行失败。
;; (require 'wakatime-mode_init)

;; (require 'symbol-overlay_init)
;; (require 'feature-mode_init)
;; (require 'mode-compile_init)
;; (require 'fluentd-mode)
;; (require 'disable-mouse_init)
(require 'telega_init)

;; 加载 dotfiles 时，阻止 gc.
;; (when (file-exists-p (expand-file-name ".emacs" config))
;;   (let ((gc-cons-threshold 20000000))
;;    (load ".emacs")))

(setenv "LOAD_INIT" "true")
