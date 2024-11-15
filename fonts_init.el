;; Stolen from https://github.com/DogLooksGood/meomacs/blob/master/laf.org

;; 一个emoji 例子 👩‍🚒
;; Inconsolata Nerd Font Mono 字体 12, 15 才能对齐, 更小没测试.
(defvar meomacs-font-size 12
  "Current font size.")

;; (defvar meomacs-font-name "终端更纱黑体-简 Nerd"
;;   "Current font name.")

;; 我发现我还是喜欢永远的, 经典的 Inconsolata
;; (defvar meomacs-font-name "Inconsolata Nerd Font Mono" "Current font name.")

(defvar meomacs-font-name "Fira Code" "Current font name.")

;; 可以针对 org table 单独设定字体
;; (custom-set-faces
;;  '(org-table ((t (:family "Sarasa Mono SC")))))

(defvar meomacs-fonts `((default . ,meomacs-font-name)
                        (cjk . ,meomacs-font-name)
                        (symbol . "Noto Color Emoji")
                        (fixed . ,meomacs-font-name)
                        (fixed-serif . ,meomacs-font-name)
                        (variable . "Sans Serif")
                        (wide . ,meomacs-font-name)
                        (tall . ,meomacs-font-name)
                        )
  "Fonts to use.")

(defun meomacs--get-font-family (key)
  (let ((font (alist-get key meomacs-fonts)))
    (if (string-empty-p font)
        (alist-get 'default meomacs-fonts)
      font)))

(defun meomacs-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s"
                              (meomacs--get-font-family 'default)
                              meomacs-font-size)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun meomacs-load-face-font ()
  "Load face font configuration."
  (let ((variable-font (meomacs--get-font-family 'variable))
        (fixed-font (meomacs--get-font-family 'fixed))
        (fixed-serif-font (meomacs--get-font-family 'fixed-serif)))
    (set-face-attribute 'variable-pitch nil :family variable-font)
    (set-face-attribute 'fixed-pitch nil :family fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :family fixed-serif-font)))

(defun meomacs-load-charset-font (&optional font)
  "Load charset font configuration."
  (let ((default-font (or font (format "%s-%s"
                                       (meomacs--get-font-family 'default)
                                       meomacs-font-size)))
        (cjk-font (meomacs--get-font-family 'cjk))
        (symbol-font (meomacs--get-font-family 'symbol)))
    (set-frame-font default-font)
    (dolist (charset '(kana han hangul cjk-misc bopomofo))
      (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)))

(defvar meomacs-font-current-variant nil)

(defun meomacs-dynamic-set-font (&rest ignore)
  (interactive)
  (when window-system
    (when (or (frame-root-window-p (get-buffer-window))
              (frame-root-window-p (window-parent)))
      (let* ((prev-font-style meomacs-font-current-variant)
             (wl (seq-filter (lambda (w) (not (string-prefix-p " " (buffer-name (window-buffer w))))) (window-list)))
             (def (meomacs--get-font-family 'default))
             (new-variant (cond
                           ((= 1 (length wl))
                            (meomacs--get-font-family 'default))

                           ((window-combined-p)
                            (meomacs--get-font-family 'tall))

                           (t
                            (meomacs--get-font-family 'wide)))))
        (unless (equal prev-font-style new-variant)
          (setq meomacs-font-current-variant new-variant)
          (set-frame-font new-variant)
          (meomacs-load-charset-font new-variant))))))

(setq frame-inhibit-implied-resize t)

(defun load-my-fonts ()
  (meomacs-load-default-font)
  (meomacs-load-face-font)

  ;; Run after startup
  (add-hook 'after-init-hook
            (lambda ()
              (when window-system
                (meomacs-load-charset-font))))

  (add-hook 'window-state-change-hook 'meomacs-dynamic-set-font)
  )

;; (load-my-fonts)
(require 'cnfonts_init)

(provide 'fonts_init)

;;; name_init.el ends here
