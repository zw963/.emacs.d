(require 'ido-other-window)
(require 'ido)
(add-list-to-list 'ido-ignore-buffers boring-buffer-regexp-list)
(add-list-to-list 'ido-ignore-files boring-file-regexp-list)
(ido-mode t)

;; ido-everywhere 和 helm 冲突, 不要开启.
;; (ido-everywhere t)

;; FIXME: 这两个关掉看看效果, 貌似有 helm 不需要了.
;; (require 'icomplete)
;; (icomplete-mode t)

(setq ido-enable-flex-matching t)     ; 打开高级匹配。例如: foo 将会匹配 froo
(setq ido-use-faces nil)              ; 关闭 ido 默认的高亮, 使用 flx-ido 高亮
(setq flx-ido-threshold 6000)         ; 默认值 6000, 是一个比较保守的阈值.

(global-set-key [remap switch-to-buffer] 'ido-switch-buffer)

(setq
 ido-show-dot-for-dired t       ; 第一个目录总是 . (允许快速打开当前目录)
 ido-use-filename-at-point 'guess ; 猜测当前 point 所在字符是否为文件名。
 ;; ido 执行深层自动匹配 auto-marge 之前，等待的时间。
 ;; 注意，auto-merge 是基于之前的访问记录自动完成，而非 find 之类的工具。
 ;; ido-auto-merge-delay-time 0.7
 ;;
 ;; 当找不到文件时，自动在历史中查找, 允许你通过 ido-switch-buffer 打开之前打开过的文件。
 ido-use-virtual-buffers t
 ;; ido-create-new-buffer 'always  ; 如果 buffer 不存在，总是建立新的 buffer.
 ;; confirm-nonexistent-file-or-buffer nil ;并且不会确认.
 ;; ido-max-prospects 15
 ;; ido-max-directory-size 100000
 ;; ido-enable-tramp-completion nil
 )

;; 下面的两个快捷键默认为: C-, C-., 感觉方向相反，很怪，换绑为滑动。
;; C-M-j, C-M-l 选择候选的文件，C-M-n, C-M-p, 选择候选的目录。
(define-key ido-common-completion-map [(control meta j)] 'ido-next-match)
(define-key ido-common-completion-map [(control meta l)] 'ido-prev-match)
(define-key ido-common-completion-map [(control meta p)] 'ido-prev-match-dir)
(define-key ido-common-completion-map [(control meta n)] 'ido-next-match-dir)
(define-key ido-common-completion-map [(\?)] 'self-insert-command)
(define-key ido-common-completion-map [(meta return)] 'ido-select-text)
(define-key ido-common-completion-map [(control return)] 'ido-make-directory)
(define-key ido-file-dir-completion-map [(meta b)] nil)

;; 两个最有用的快捷键: C-o, 另一个窗口打开, M-o, 另一个 frame 打开.
(define-key ido-common-completion-map [(meta o)] 'ido-invoke-in-new-frame)

(provide 'ido_init)
;;; ido_init.el ends here
