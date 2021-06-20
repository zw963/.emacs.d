;; C-x r y, yank-rectangle 粘帖选区内容
;; C-x r k, kill-rectangle 剪切选区内容
;; C-x r M-w, copy-rectangle-as-kill 复制选区内容
;; C-x r d, delete-rectangle 删除选区, 文字左移
;; C-x r N, rectangle-number-lines  选区前面插入数字
;; C-x r t, string-rectangle 在指定的选区之前添加内容.

;; 开启 rectangle-mark-mode, 快捷键: C-x SPC

(require 'rect)

(defadvice kill-ring-save (around rect-mark activate)
  "Let 'kill-ring-save support rect-mark."
  (if rectangle-mark-mode
      (call-interactively 'copy-rectangle-as-kill)
    ad-do-it
    ))

(defadvice kill-region (around rect-mark activate)
  "Let 'kill-ring-save support rect-mark."
  (if rectangle-mark-mode
      (call-interactively 'kill-rectangle)
    ad-do-it
    ))

(defadvice delete-char (around rect-mark activate)
  "Let 'delete-char support rect-mark."
  (if rectangle-mark-mode
      (delete-rectangle (region-beginning) (region-end))
    ad-do-it
    ))

(provide 'rect-mark_init)
;;; rect-mark_init.el ends here
