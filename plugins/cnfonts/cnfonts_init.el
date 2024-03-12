(require 'cnfonts)

(setq cnfonts-use-face-font-rescale t)
(setq cnfonts-directory (expand-file-name "configs_retina" (file-name-directory (or load-file-name buffer-file-name))))

(defconst cnfonts-personal-fontnames
  '(;; 英文字体
    ("Fira Code")
    ;; 中文字体
    ("霞鹜文楷等宽")
    ;; EXT-B 字体
    ()
    ;; Symbol 字符字体
    ("Noto Color Emoji")
    ;; Emacs 社区配置中，用于装饰的字符使用的字体
    ()
    ))

(cnfonts-mode 1)

;; (setq use-default-font-for-symbols nil)
;; (when (member "Noto Color Emoji" (font-family-list))
;;   (set-fontset-font
;;    t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;; ;; 有关设置字体的一些参考代码
;; (defun my-better-font()
;;   (interactive)
;;   (defvar my-english-font "Fira Code:style=Regular")
;;   (defvar my-english-font-height (* 15 10))

;;   (defvar my-chinese-font "LXGW WenKai Mono,霞鹜文楷等宽:style=Regular")
;;   (defvar my-chinese-font-size 15)

;;   (defvar +font-rescale '((tall . 1.0) (wide . 1.0)))
;;   (defvar +font-wide-or-tall 'tall)
;;   ;; 设置英文字体
;;   (set-face-font 'default my-english-font)
;;   ;; (set-face-attribute 'default nil :height my-english-font-height :weight 'regular :family my-english-font)
;;   (let ((rescale (alist-get +font-wide-or-tall +font-rescale)))
;;     (setq face-font-rescale-alist
;;           `((,my-chinese-font . ,rescale))))
;;   (when (display-graphic-p)
;;     ;; 设置中文字体
;;     (dolist (charset '(kana han hangul cjk-misc bopomofo symbol))
;;       (set-fontset-font
;;        (frame-parameter nil 'font)
;;        charset
;;        (font-spec :family my-chinese-font :size my-chinese-font-size)))
;;     ;; instruct Emacs to use emoji fonts,
;;     (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend)
;;     ))

;; (defun my-better-hybird-font ()
;;   ;; 设置混合字体
;;   (dolist (param '(
;;                    ;; (font . "yaheiInconsolata-15")
;;                    ;; (font . "Sarasa Mono SC Nerd-15")
;;                    ;; (font . "YaHei Monaco Hybird:style=Regular-15")
;;                    (font . "SHS Monaco Adjusted Medium:style=Medium-14")
;;                    ))
;;     (add-to-list 'default-frame-alist param)
;;     (add-to-list 'initial-frame-alist param)
;;     ))

;; ;; 获取当前使用的字体：(face-attribute 'default :font)
;; ;; (set-frame-font "Fira Code")
;; ;; (set-frame-font "Inconsolata")
;; ;; (set-frame-font "SHS Monaco Adjusted Medium:style=Medium-12")
;; ;; ，, '' " 1 l i o 0
;; ;; (my-better-font)
;; ;; (my-better-hybird-font)

;; (defun set-cnfonts-fonts(&optional frame)
;;   (interactive)
;;   ;; (when (display-graphic-p)
;;     (set-face-attribute
;;      'default nil
;;      :font (font-spec :name "Fira Code"
;;                       :weight 'normal
;;                       :slant 'normal
;;                       :size 14))
;;     (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;       (set-fontset-font
;;        "fontset-default"
;;        charset
;;        (font-spec :name "霞鹜文楷等宽"
;;                   :weight 'normal
;;                   :slant 'normal
;;                   :size 18.0))))
;; )

;; (add-hook 'window-setup-hook 'set-cnfonts-fonts t)

;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions 'set-cnfonts-fonts t)
;;   )

(provide 'cnfonts_init)

;;; cnfonts_init.el ends here
