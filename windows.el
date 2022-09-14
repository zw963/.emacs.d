;; -------------------------- 中英文字体对齐 -----------------------------
;; 下面的代码，用来在放大字体时，保持英文中文比例一致，让表格总是对齐

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil
    t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defvar zh-font-list '("LXGW WenKai Mono" "HanaMinB"))
;; (defvar en-font-list '("Fira Code Retina" "Iosevka" ))
(defvar en-font-list '("IBM Plex Mono" "Iosevka" ))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-scale)

  (setq chinese-font-scale (or chinese-font-scale 1.2))

  (setq face-font-rescale-alist
        (cl-loop for x in zh-font-list
                 collect (cons x chinese-font-scale)))

  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-scale to nil, it will follow english-font-size"

  (let ((en-font (qiang-make-font-string
                  (cl-find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (cl-find-if #'qiang-font-existsp chinese-fonts))))

    ;; Set the default English font
    (message "Set English Font to %s" en-font)
    (set-face-attribute 'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the English font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset zh-font))))

;; ------------------------------ 启动最大化 ------------------------------
;; 可以通过运行 fc-list 来查看字体的名称.
(defun initialize-frame-delay (&optional frame)
  "Maximize FRAME when create a new FRAME."
  (when (display-graphic-p)
    ;; 这个设定应该是设定所有的 unicode 字体.
    ;; (set-fontset-font "fontset-default" "unicode" ("SHS Monaco Adjusted Medium-12"))

    ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
    ;;   (set-fontset-font (frame-parameter nil 'font)
    ;;                     charset
    ;;                     (font-spec :family "SHS Monaco Adjusted Medium-12")))
    ;; (qiang-set-font en-font-list 13 zh-font-list)
    )

  ;; (set-fontset-font "fontset-default" 'unicode'("yaheiInconsolata-15"))

  ;; (set-fontset-font "fontset-default" '(#x20a0 . #x2a3b)
  ;;                   (font-spec :family "等距更纱黑体 SC"
  ;;                              :size 24) nil 'prepend)

  ;; (member this-command '(eval-last-sexp))
  (unless (member
           this-command
           '(
             make-frame-command
             ace-window
             make-frame
             popper-toggle-latest
             popper-cycle
             ))
    (run-with-idle-timer 0 nil 'toggle-frame-maximized)
    )
  )

;; NOTICE: 传送给aftar-make-frame-function的函数必须有且只能有一个参数用来表示新建立的frame.

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions 'initialize-frame-delay t)
  )

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
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
                 ;; (alpha 95 80) ;; 设置透明度, 默认设置微透明，使用 toggle-transparency 函数关闭
                 ;; 当以 daemon 启动时， 光标使用系统光标黑色， 这里改为浅白色。
                 ;; (cursor-color . "AntiqueWhite3")
                 ;; (cursor-color . ,zw/cursor-color-default)
                 ;; (fullscreen . nil)
                 ))
  (add-to-list 'default-frame-alist param)
  (add-to-list 'initial-frame-alist param)
  )

(require 'cnfonts_init)

(provide 'fonts_init)
;;; base_init.el ends here
