;; Stolen from https://github.com/DogLooksGood/meomacs/blob/master/laf.org

(defvar meomacs-font-size 15
  "Current font size.")

(defvar meomacs-fonts '((default . "终端更纱黑体-简 Nerd")
                        (cjk . "终端更纱黑体-简 Nerd")
                        (symbol . "Noto Color Emoji")
                        (fixed . "终端更纱黑体-简 Nerd")
                        (fixed-serif . "终端更纱黑体-简 Nerd")
                        (variable . "Sans Serif")
                        ;; (wide . "终端更纱黑体-简 Nerd")
                        ;; (tall . "终端更纱黑体-简 Nerd")
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

(meomacs-load-default-font)
(meomacs-load-face-font)

;; Run after startup
(add-hook 'after-init-hook
          (lambda ()
            (when window-system
              (meomacs-load-charset-font))))

(add-hook 'window-state-change-hook 'meomacs-dynamic-set-font)

;; (require 'cnfonts_init)

(provide 'fonts_init)

;;; name_init.el ends here
