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
;; (require 'sort-tab_init)

(require 'dimmer_init)
(require 'fancy-narrow_init)
(require 'zoom-frm_init)
(require 'back-button_init)
(require 'winum_init)
(require 'which-key_init)
;;(require 'anzu_init)
(require 'beacon_init)
(require 'avy_init)
(require 'doom-modeline_init)
(require 'rich-minority_init)
(require 'show-point-mode_init)
;; (require 'hydra_init);; depend by treemacs
(require 'edit-server_init)
(require 'undo-tree_init)
(require 'toggle-quotes_init)
(require 'beyond-compare_init)
;; (require 'ido_init)

(require 'flx_init)
(require 'company_init)
(require 'helm_init)
(require 'helm-bindings_init) ; 和 ivy_init 冲突， 二者选其一.
;; (require 'ivy_init)
(require 'prescient_init) ;; 这个必须放在 ivy_init 或 company_init 后面.

(require 'yasnippet_init)
(require 'wgrep_init)
(require 'quickrun_init)
;; (require 'amx_init) ;; 似乎运行 dap-debug 时出错?
(require 'spatial-navigate_init)

(require 'vterm_init)
(require 'async_init)
(require 'mu4e_init)
(require 'volatile-highlights_init) ;; 这个现在看不出效果？
(require 'buffer-move_init)
(require 'goto-chg_init)
(require 'symbol-overlay_init)
(require 'iedit_init) ;; 确保放到 symbol-overlay 后面
(require 'color-rg_init)
;; (require 'eee_init) ;; 这个打开，覆盖 color-rg 的 Ctrl + Alt + R
(require 'markdown-mode_init)
;; (require 'easy-kill_init)
(require 'burly_init)
(require 'sis_init)
(require 'page-break-lines_init)

;; ============================== 编程相关 ==============================

(require 'prog-mode_init)
(require 'tree-sitter_init)
;; (require 'combobulate_init)
(require 'snap-indent_init)
;; (require 'indentinator_init)
(require 'dumb-jump_init)
(require 'apheleia_init) ;; 怀疑这个跟 lsp-mode formatter 冲突?
(require 'git_init)
(require 'flycheck_init)
(require 'smartparens_init)
(require 'string-inflection_init)
(require 'highlight-indentation_init)
(require 'highlight-escape-sequences_init)
(require 'rainbow-delimiters_init)
(require 'expand-region_init)
(require 'ruby_init)
(require 'crystal-mode_init)
(require 'rainbow-mode_init)
(require 'scss-css-mode_init)
;; (require 'format-all_init)
;; (require 'wakatime-mode_init)
(require 'webpaste_init)
(require 'ws-butler_init)
(require 'yafolding_init)
;; (require 'fira-code-mode_init)
(require 'ligature_init)

;; 关闭一些有用，但最近不常用的 mode
;; (require 'dart-mode_init)
;; (require 'go-mode_init)
(require 'rust_init)
(require 'go-ts-mode_init)
;; (require 'rust-ts-mode_init)

;; (require 'lsp-bridge_init)
;; (require 'tabnine_init) ;; lsp-bridge 默认会开启 tabnine
(require 'codeium_init)
;; (require 'haskell-mode_init)
;; (require 'js2-mode_init)
;; (require 'lua-mode_init)

(require 'elixir_init)

;; 这个似乎要放到最后面？
(require 'posframe_init)
(require 'shackle_init)
(require 'popper_init)
(require 'nerd-icons_init)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'sgml-mode-hook 'display-line-numbers-mode)
(add-hook 'feature-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)

;; emacs 29 support
;; 打开这个的话，如果两个 window/frame 打开同一个 buffer
;; 总是会自作聪明的同步两个 window/frame 光标所在位置。
;; 打开这个模式，应避免同时多个 window 打开同一个 buffer。
(pixel-scroll-precision-mode 1)

;; (setq display-line-numbers 'relative)

(setenv "LOAD_INIT" "true")
