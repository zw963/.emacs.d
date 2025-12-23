;; -*- lexical-binding: t; -*-

(require 'gcmh)

;; 有个哥们儿设置 GC 为最大整数值，即：关闭 GC, 直到重启。
;; ( setq
 ;; gc-cons-threshold most-positive-fixnum
 ;; gc-cons-percentage 0.1
 ;; )

 ;; gc-cons-threshold controlled by gcmh.el
 ;; 注意，不要随便设置很大的 gc-cons-threshold, 否则会突然很卡。
 (gcmh-mode 1)

 (provide 'gcmh_init)

;;; gcmh_init.el ends here
