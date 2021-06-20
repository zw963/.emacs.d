;; ============================================================================
;;
;; Auto complete
;;
;; ============================================================================

(require 'auto-complete-config)
(ac-config-default)

;; 也可以手动加字符串到用户词典, 会立即生效.
(setq ac-user-dictionary '("vil963@gmail.com" "zw963@163.com"))
(setq
 ac-dictionary-directories (expand-file-name "plugins/ac-dict")
 ac-user-dictionary-files (expand-file-name "plugins/user-dict")

 ;; ac-auto-start 4                        ; 至少输入 4 个字符, 才启动 ac, 提高性能和与 yas 的兼容性.
 ;; ac-use-quick-help nil                  ; 关闭 quick help
 ac-ignore-case nil                     ; 大小写总是敏感, 否则候选结果太多, 默认值是 smart
 ;; ac-dwim nil                              ; 默认是 t
 ;; ac-delay 1                           ; 默认 0.1
 ac-stop-words '("_" "," "{" ":" "do" "end")

 ac-use-menu-map t                      ; popup 菜单使用 ac-menu-map,  如果希望使用 C-s, 必须开启此项.
 ac-auto-show-menu 1.0                  ; 默认 0.8 秒
 ac-candidate-menu-min 2                ; 默认值 1
 ac-candidate-limit 20
 )

;; 当修改了用户词典文件的内容, 运行这个快捷键让词典立即生效.
(global-set-key [(control meta g)] 'ac-clear-dictionary-cache)

(add-hook 'delete-frame-functions (lambda (frame) (ac-comphist-save)))


;; AC 里面各种 map 简述:
;; ac-menu-map, 用于设定当 popup 弹出后的快捷键
;; 但是如果某些键在这个模式中没有被设定, 会继承它的 parent map (ac-complete-mode-map)
;; ac-completing-map 的键值, 无论是单行匹配的 overlay 阴影,
;; 还是多行匹配的菜单, 它们都使用的是 ac-completing-map,
;; ac-complete-mode-map 是 ac-completing-map 的别名

;; 下面的设定确保:
;; 1. TAB 一定完成 snippets 或者仅仅取消 overlay.
;; 2. RET 一定完成 auto-complete.
;; Yeah!

(define-key ac-complete-mode-map [(tab)] nil)
(define-key ac-complete-mode-map "\t" nil)
(define-key ac-complete-mode-map (kbd "RET") 'ac-complete)
(define-key ac-complete-mode-map [(return)] 'ac-complete)

;; 当弹出 menu 后, TAB 默认行为是 ac-expand, 会选择下一个候选者.
;; 这里换绑 TAB 为 ac-complete, 直接选择当前候选者, 和 RET 一样的行为.
(define-key ac-menu-map [(tab)] 'ac-complete)
(define-key ac-menu-map "\t" 'ac-complete)

(require 'popup)
(define-key popup-menu-keymap [(tab)] 'popup-select)
(define-key popup-menu-keymap [(?\t)] 'popup-select)

;; (defvar nasy/prev-whitespace-mode nil)
;; (make-variable-buffer-local 'nasy/prev-whitespace-mode)
;; (defvar nasy/show-trailing-whitespace nil)
;; (make-variable-buffer-local 'nasy/show-trailing-whitespace)
;; (defun pre-popup-draw ()
;;   "Turn off whitespace mode before showing company complete tooltip"
;;   (if whitespace-mode
;;       (progn
;;         (gsetq my-prev-whitespace-mode t)
;;         (whitespace-mode -1)))
;;   (gsetq nasy/show-trailing-whitespace show-trailing-whitespace)
;;   (gsetq show-trailing-whitespace nil))
;; (defun post-popup-draw ()
;;   "Restore previous whitespace mode after showing company tooltip"
;;   (if nasy/prev-whitespace-mode
;;       (progn
;;         (whitespace-mode 1)
;;         (gsetq nasy/prev-whitespace-mode nil)))
;;   (gsetq show-trailing-whitespace nasy/show-trailing-whitespace))
;; (advice-add 'company-pseudo-tooltip-unhide :before #'pre-popup-draw)
;; (advice-add 'company-pseudo-tooltip-hide :after #'post-popup-draw)

(provide 'auto-complete_init)
;;; auto-complete_init.el ends here
