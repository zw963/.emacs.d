(require 'repeat)

(setq repeat-exit-key (kbd "RET"))
(setq repeat-exit-timeout nil)

(repeat-mode 1)

;; gpt 问了很久，也没找到一个稳定的定义自定义命令的写法

(defvar my/text-scale-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "=") #'text-scale-increase)
    (define-key map (kbd "-") #'text-scale-decrease)
    (define-key map (kbd "0") #'my/text-scale-reset)
    map))

(defun my/text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun my/text-scale-repeat ()
  (interactive)
  (message "Text scale: = - 0"))

;; 关键：入口命令 + 实际被重复执行的命令，都要带 repeat-map
(dolist (cmd '(my/text-scale-repeat
               text-scale-increase
               text-scale-decrease
               my/text-scale-reset))
  (put cmd 'repeat-map 'my/text-scale-repeat-map))  ; repeat-mode 就靠这个属性 :contentReference[oaicite:1]{index=1}

(global-set-key (kbd "C-c z") #'my/text-scale-repeat)


(provide 'repeat-mode_init)
;;; repeat-mode_init.el ends here
