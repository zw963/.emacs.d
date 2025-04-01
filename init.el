;; -*-Emacs-Lisp-*-

(require 'dash)
(require 'f)
(require 'ht)
(require 's)

;; ==============================没有专门绑定快捷键的 mode==============================
(require 'org-mode_init)
(require 'ibuffer_init)
(require 'dimmer_init)
(require 'which-key_init)
(require 'beacon_init)

;; (require 'anzu_init)
(require 'visual-regexp_init)

(require 'edit-server_init)
(require 'doom-modeline_init)
(require 'flx_init)
(require 'fussy_init); 放到 flx_init 后面
;; (require 'hotfuzz_init)
(require 'company_init)
(require 'breadcrumb_init)

;; / 然后键入关键字，可以快速过滤。
;; S, 可以调出 hydra 菜单，快速排序。
;; 暂时关闭了 dired+, 因为，C-o 是在另一个 window 查看当前文件（光标不切换）
(require 'dired_init)

;; (require 'hydra_init);; depend by treemacs
;; C-d, treemacs-remove-project-from-workspace
(require 'treemacs_init)


;; C-x n n (narrow), C-x n w (widen)
(require 'fancy-narrow_init)

;; C-+,  C--
(require 'zoom-frm_init)

;; M-1~M-7, select tab
;; M-9 left-tab, M-0 right-tab.
(require 'sort-tab_init)

;; C-x 然后左右方向键, 本文件历史，
;;或 C-x 然后 Ctrl + 左右方向键, 全局历史
(require 'back-button_init)

;; C-j 然后输入一个关键字，然后根据提示，跳转到匹配的位置。
;; isearch 的时候，出来结果后，也可以 C-j 跳转。
;; M-z 可以直接删除到某个字符。
;; C-x o, 跳转到指定 window，不过更好的选择是内置的 windmove 包, 使用 M + vim 快捷键快速移动。
;; M-h 左，M-j 下，-- M-k 上，M-l 右
;; M-g c, 跳转到指定编号的字符, 多用于调试代码，跳转到指定位置（取代之前的 show-point-mode)
;; 进入帮助界面时，按下 o, 可以跳转
;; 运行 char-menu，可以输入一些特殊字符。
(require 'avy_init)

;; C-x u    => undo-tree-visualize
;; C-/      => undo-tree-or-undo-pp
;; C-M-/    => undo-tree-redo
(require 'undo-tree_init)
;; (require 'vundo_init)

;; C-' (回车键左边的按键）
(require 'toggle-quotes_init)

;; 在 dired，ibuffer 以及正常打开的 buffer 中，C-f12 bc1, M-f12 bc2还提供了针对选区的 diff C-f11 bc1 选区 M-f11, bc2 选区 (这个有用，忘记用了）
;; 某些 VC 模式中，哈希上按 = 和当前 hash 比较，d 就是比较当前 commit 的修改
;; C-x v = 直接 diff 当前文件的未提交修改
;; treemacs 鼠标右键，新增了 bc1, bc2
;; C-x v h local diff + index 修改
(require 'beyond-compare_init)

;; (require 'ido_init)
(require 'helm_init)
;; C-c l imenu
;; C-x b helm-buffer-list
;; C-r helm-ag, 可以连续使用 C-r，第一次当前文件，下一次上一级文件夹，支持 f3 编辑
;; M-r helm-git-grep, 第一次在目录，第二次项目，支持 f3 编辑
;; C-x C-d, helm-browse-project
(require 'helm-bindings_init) ; 和 ivy_init 冲突， 二者选其一.
;; (require 'ivy_init)
;; (require 'prescient_init) ;; 这个必须放在 ivy_init 或 company_init 后面.

(require 'yasnippet_init)
(require 'wgrep_init)
(require 'quickrun_init)
(require 'amx_init) ;; 似乎运行 dap-debug 时出错?
(require 'spatial-navigate_init)
(require 'shell_init)
(require 'vterm_init)
(require 'async_init)
(require 'mu4e_init)
(require 'volatile-highlights_init) ;; 这个现在看不出效果？
(require 'buffer-move_init) ;; ????
(require 'goto-chg_init)
(require 'symbol-overlay_init) ????
(require 'iedit_init) ;; 确保放到 symbol-overlay 后面
;; (require 'color-rg_init)
(require 'deadgrep_init)
;; (require 'eee_init) ;; 这个打开，覆盖 color-rg 的 Ctrl + Alt + R
(require 'markdown-mode_init)
;; (require 'easy-kill_init)
;; (require 'burly_init)
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
(require 'git_init) ;; ????
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
