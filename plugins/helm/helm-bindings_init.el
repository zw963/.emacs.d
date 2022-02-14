(helm-mode 1)

;; 在其他界面, 可以直接切换到
;; (define-key helm-map [(control x) (n)] 'helm-resume-list-buffers-after-quit)

;; 下面的配置来自于 emacs-helm.sh
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key minibuffer-local-map [(control /)] 'helm-undo-yank-text-at-point)

;; 不懂干嘛的.
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(global-set-key [remap imenu] 'helm-imenu)

(add-hook 'prog-mode-hook
          #'(lambda ()
              (local-set-key [(control c) (j)] 'helm-imenu-in-all-buffers)
              ))

;;;  ------------------------ helm-swoop -------------------------

;; (setq helm-swoop-use-fuzzy-match t)
;; (setq helm-swoop-speed-or-color t)  ;;
;; (setq helm-swoop-split-direction 'split-window-horizontally)
;; ;; ;; nil 是默认值, 确保不要开启，因为多个文件时，无法切换到下一个文件。
;; (setq helm-move-to-line-cycle-in-source nil)
;; (require 'helm-swoop)
;; (global-set-key [(control r)] 'helm-swoop)
;; (define-key helm-swoop-map [(control r)] 'helm-multi-swoop-all-from-helm-swoop)
;; (define-key isearch-mode-map [(control r)] 'helm-swoop-from-isearch)
;; (define-key dired-mode-map [(control r)] 'helm-do-ag)

(provide 'helm-bindings_init)

;;; helm-bindings_init.el ends here
