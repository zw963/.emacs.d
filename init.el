;; -*- lexical-binding: t; -*-
;; -*-Emacs-Lisp-*-

(require 'dash)
(require 'f)
(require 'ht)
(require 's)

;; ==============================没有专门绑定快捷键的 mode==============================
(require 'org-mode_init)
(require 'ibuffer_init)

;; 下面的二选一
(require 'dimmer_init)
;; (require 'golden-ratio_init)

;; 二选一
;; (require 'highlight-indentation_init)
(require 'indent-bars_init)

(require 'which-key_init)
(require 'beacon_init)

;; (require 'visual-regexp_init)

(require 'edit-server_init)
(require 'all-the-icons_init) ;; 放到 helm 前面


;; hotfuzz 或 fussy 二选一
(require 'flx_init)
(require 'fussy_init); 放到 flx_init 后面
;; (require 'hotfuzz_init)

(require 'company_init)
(require 'breadcrumb_init)
(require 'async_init)
(require 'volatile-highlights_init)

(require 'sis_init)

;; 记得这个最大的问题是, 无法在 daemon 模式下恢复保存的 workspace?
;; Ctrl-x w s 可以执行一个 Emacs 默认提供的的函数 window-toggle-side-windows，非常好用。
;; (require 'burly_init)

;; ---------------- 编程相关 ----------------

(require 'indentation_init)
(require 'tree-sitter_init)
;; (require 'apheleia_init) ;; 怀疑这个跟 lsp-mode formatter 冲突?
(require 'dumb-jump_init)
(require 'smartparens_init)
(require 'highlight-escape-sequences_init)
(require 'rainbow-delimiters_init)
(require 'colorful-mode_init)
;; (require 'scss-css-mode_init) ;; 使用默认的 css-ts-mode
(require 'ws-butler_init)
(require 'ligature_init)
(require 'tailwindcss_init)
(require 'electric-operator_init)

;; (require 'tabnine_init) ;; lsp-bridge 默认会开启 tabnine
;; (require 'codeium_init)

;; =============== 下面是绑定快捷键的模式 ===============


;; / 然后键入关键字，可以快速过滤。
;; S, 可以调出 hydra 菜单，快速排序。
;; Shift + F2, 重命名当前文件
;; Backspace, 返回上一级目录
;; C-s 文件名 isearch
;; o 在另一个 window 打开文件(光标切换)
;; C-o 是在另一个 window 查看当前文件（光标不切换）
;; C-c o, 另一个 frame 打开当前文件 (光标切换)
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
;; (require 'sort-tab_init)

;; C-x 然后连续按下左右方向键, 本文件历史，
;;或 C-x Ctrl 不放, 连续按下左右方向键, 全局历史
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

;; 记住 aya 前缀的 command，例如：aya-create, aya-expand
(require 'yasnippet_init)

;; 记住，C-c C-c 是编辑完成，我自己修改了快捷键，F3 通常是进入 wgrep 编辑模式。
;; 通常会和 iedit-mode 的 Ctrl + ; 一起使用。
(require 'wgrep_init)

;; C-c 回车，或者 C-x C-e 执行 region 
(require 'quickrun_init)

;; 基于空格的智能跳转，Alt + 上下左右
(require 'spatial-navigate_init)

;; 主要是 vterm，有个 Ctrl + ~ 快捷键
;; 但是一个报错：awk: cmd. line:1: warning: escape sequence `\[' treated as plain `['，很恼火。
(require 'shell_init)

;; 快捷键很多，见相关笔记
(require 'mu4e_init)

;; Ctrl + Shift + 上下左右
(require 'buffer-move_init)

;; Ctrl + . 以及 , （就是源代码跳转时，使用 Alt, 改成 Ctrl 即可）
;; 注意, Ctrl + . 在中文状态下, 和 fcitx 冲突, 需要关闭相应快捷键
(require 'goto-chg_init)

;; 目前是 F7, 实在找不到好的快捷键
;; TODO: 找找有没有什么适合前缀的快捷键, 占用了更加适合连续键入的非前缀快捷键
(require 'symbol-overlay_init)

;; Ctrl + ;
(require 'iedit_init) ;; 确保放到 symbol-overlay 后面

;; 二选一
;; Ctrl + Alt + r, F3 使用 wgrep 编辑
(require 'color-rg_init)
;; (require 'eee_init) ;; 这个打开，覆盖 color-rg 的 Ctrl + Alt + R
;; (require 'deadgrep_init)

;; 记住 C-c ' 编辑代码块中的代码.
(require 'markdown-mode_init)

;; ---------------- 编程相关 ----------------

;; M-c . 输入一个 `=> '
;; C-c C-c 执行 format-buffer
;; 还有个 Alt + Return,  某些模式下有用, 用来变换 do ... end block 到 { ... }
(require 'prog-mode_init)

;; 包含 hideshow
;; 鼠标右键找到 fold/unfold 展开或收起代码块 (或者 Ctrl + TAB)
;; C-c / 收起所有代码块
;; C-c \ 展开所有代码块
(require 'context-menu-mode_init)
;; (require 'treesit-fold_init)

;; 快捷键见 git_init.el
(require 'git_init) ;; ????

;; 注意两点:
;; 如果 flycheck 检测到错误, 整个 mode-line 都是红色的
;; 当前使用的 mode line, 右侧可以看到 flycheck 的状态, 鼠标点击, 出来一个菜单, 可以选择下一个错误.
(require 'flycheck_init)
;; (require 'flyover_init)

;; Alt + u, 变换光标前后的符号
;; Shift + Alt + u, 仅仅变换光标前符号到全部大写
(require 'string-inflection_init)

;; C-M-h
(require 'expand-region_init)

;; C-c C-l 运行 robe repl
;; C-x C-e 运行 seeing-is-believing
;; (require 'ruby_init)

(require 'crystal-mode_init)
;; (require 'wakatime-mode_init)

;; C-c p
(require 'webpaste_init)

(require 'shackle_init)
;; popper-cycle 绑定到 Ctrl + Shift + ~
(require 'popper_init)

;; 关闭一些有用，但最近不常用的 mode
;; (require 'dart-mode_init)
;; (require 'go-mode_init)
(require 'rust_init)
;; (require 'go-ts-mode_init)
;; (require 'rust-ts-mode_init)

;; (require 'lsp-bridge_init)
;; (require 'haskell-mode_init)
;; (require 'js2-mode_init)
;; (require 'lua-mode_init)

(require 'elixir_init)

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
