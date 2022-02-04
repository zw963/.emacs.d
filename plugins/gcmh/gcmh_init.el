(require 'gcmh)

;; (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

;; gc-cons-threshold controlled by gcmh.el
;; 注意，不要随便设置很大的 gc-cons-threshold, 否则会突然很卡。
(gcmh-mode 1)

;; 根据 lsp-dart 的建议，这个应该设定大一点，性能会好很多。
(setq read-process-output-max (* 1024 1024)) ;; 1M

(provide 'gcmh_init)

;;; gcmh_init.el ends here
