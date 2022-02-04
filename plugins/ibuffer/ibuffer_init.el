(require 'ibuffer)
(require 'ibuf-ext)                     ;隐藏系统 buffer.
(setq ibuffer-expert t                  ;ibuffer 危险操作不确认提示.
      ibuffer-show-empty-filter-groups nil ;隐藏空组
      )
(add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)
(add-list-to-list 'ibuffer-never-show-predicates boring-buffer-regexp-list)

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 35 35 :left :nil) " "
              (size-h 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)))
(setq desktop-clear-preserve-buffers '())
;; 用来清楚桌面上所有的 buffer.
(define-key ibuffer-mode-map [(f2)] 'desktop-clear)

(add-hook 'ibuffer-mode-hook (lambda () (setq-local display-buffer-base-action '(display-buffer-use-some-window))))


(provide 'ibuffer_init)
;;; ibuffer_init.el ends here
