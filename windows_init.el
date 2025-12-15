;; -*- lexical-binding: t; -*-

;; -------------------------- 中英文字体对齐 -----------------------------
(global-set-key [(f5)] (lambda ()
                         (interactive)
                         (toggle-frame-maximized)
                         (set-frame-position (selected-frame) 0 0)
                         (set-frame-size (selected-frame) 120 63)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(95 . 80) '(100 . 100)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name))
                 "%b")) " - Emacs " emacs-version))

;; 中英文逗号 ，,
(dolist (param `(
                 (menu-bar-lines . 0)
                 (tool-bar-lines . 0)
                 (vertical-scroll-bars . nil)
                 (left-fringe . 1)
                 (right-fringe . 1)
                 (alpha 92 85) ;; 设置透明度, 默认设置微透明，使用 toggle-transparency 函数关闭
                 ;; 当以 daemon 启动时， 光标使用系统光标黑色， 这里改为浅白色。
                 ;; (cursor-color . "AntiqueWhite3")
                 ;; (cursor-color . ,zw/cursor-color-default)
                 ;; (fullscreen . nil)
                 ))
  (add-to-list 'default-frame-alist param)
  (add-to-list 'initial-frame-alist param)
  )

(provide 'windows_init)
;;; windows_init.el ends here



;; =============== 不用的函数 ===============

;; ;; ------------------------------ 启动最大化 ------------------------------
;; (defun initialize-frame-delay (&optional frame)
;;   "Maximize FRAME when create a new FRAME."
;;   ;; (when (display-graphic-p)
;;   ;;   )

;;   ;; (member this-command '(eval-last-sexp))
;;   ;; (require 'demap)
;;   ;; (setq demap-minimap-window-width 15)
;;   (run-with-idle-timer 0.2 nil '(lambda (f)
;;                                   (unless (member
;;                                            this-command
;;                                            '(
;;                                              make-frame-command
;;                                              ace-window
;;                                              make-frame
;;                                              popper-toggle-latest
;;                                              popper-cycle
;;                                              ))
;;                                     ;; (demap-open nil nil)
;;                                     (when (frame-live-p f)
;;                                       (toggle-frame-maximized f))
;;                                     ))))

;; ;; NOTICE: 传送给aftar-make-frame-function的函数必须有且只能有一个参数用来表示新建立的frame.
;; ;; 升级最新版本 igc 之后，不知道是不是 arch 的问题，启动窗口后会最小化
;; ;; 需要将下面的代码加回来才可以。
;; ;; 启动窗口最大化我在 emacsclient 命令行中通过 -F 参数传入
;; (when (and (fboundp 'daemonp) (daemonp))
;;   (add-hook 'after-make-frame-functions 'initialize-frame-delay t)
;;   )
