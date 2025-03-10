(require 'gcmh)

;; (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

;; 有个哥们儿设置 GC 为最大整数值，即：关闭 GC, 直到重启。
;; ( setq
 ;; gc-cons-threshold most-positive-fixnum
 ;; gc-cons-percentage 0.1
 ;; )

 ;; gc-cons-threshold controlled by gcmh.el
 ;; 注意，不要随便设置很大的 gc-cons-threshold, 否则会突然很卡。
 (gcmh-mode 1)

 ;; 根据 lsp-dart 的建议，这个应该设定大一点，性能会好很多。
 (setq read-process-output-max (* 1024 1024)) ;; 1M

 (provide 'gcmh_init)

;;; gcmh_init.el ends here
