(require 'helm-config) ;default use C-x c prefix.
(require 'helm-files)
(require 'helm-buffers)
(require 'helm-fd)

(add-list-to-list 'helm-boring-buffer-regexp-list boring-buffer-regexp-list)
(add-list-to-list 'helm-boring-file-regexp-list boring-file-regexp-list)

(setq
 helm-candidate-number-limit 25
 helm-buffer-max-length 50 ;; buffer 的名称长一点，否则看不到完整名字。
 ;; 似乎我的 (window-width) 值是 93, 这里设定小于 93, C-c o 将会在右侧 split.
 ;; (setq-default split-width-threshold 30)
 split-height-threshold nil
 helm-recentf-fuzzy-match t
 helm-ff-skip-boring-files t
 helm-ff-guess-ffap-filenames t
 helm-ff-file-name-history-use-recentf t
 helm-ff-auto-update-initial-value t
 )

;; 让 helm 弹出的窗口, 总在最下面, 当开启 follow-mode 时, 这个和 neotree 不工作.
(setq helm-always-two-windows nil) ;; this is default
(setq-default helm-display-buffer-default-height (* (frame-height) 0.618))
(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

;; (setq helm-move-to-line-cycle-in-source t)

;; change order to show recentf first.
;; (setq helm-mini-default-sources
;;       '(
;;        helm-source-recentf
;;        helm-source-buffers-list
;;        helm-source-buffer-not-found))

;; FIXME: 这个是干嘛的?
(add-to-list 'helm-buffers-favorite-modes 'ruby-mode)
(add-to-list 'helm-buffers-favorite-modes 'enh-ruby-mode)

;; FIXME: 这里没有用 remap, 因为要改写 ido-find-file 的 hack.
(define-key global-map [(control x) (control f)] 'helm-find-files)
;; 替换 helm-buffer-list 为 helm-resume

;; list-buffers C-x C-b switch-to-buffer C-x b
(global-set-key [(control x) (f)] 'helm-recentf)
(global-set-key [(control x) (b)] 'helm-buffers-list)
(global-set-key [(control x) (control b)] 'helm-mini)
(global-set-key [(control x) (control p)] 'helm-resume)

(add-hook 'helm-mode-hook '(lambda ()
                             (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
                             (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
                             (define-key helm-map (kbd "C-i") #'helm-select-action)
                             (define-key helm-map (kbd "C-z") #'helm-select-action)
                             ))

;; ;; because we map control j to super j.
;; (define-key helm-map [(super j)] 'helm-execute-persistent-action)
;; (define-key helm-map [(super l)] 'helm-recenter-top-bottom-other-window)

(require 'helm-imenu)
;; (custom-set-variables '(helm-imenu-fuzzy-match t))

(add-to-list 'helm-imenu-all-buffer-assoc  '(enh-ruby-mode . js2-mode))

;; 这里我们使用 counsel-imenu, 它有更多的反馈信息。
;; (add-hook 'prog-mode-hook
;;           #'(lambda ()
;;               (local-set-key [(control c) (j)] 'helm-imenu-in-all-buffers)
;;               ))

;; (add-hook 'org-mode-hook
;;           #'(lambda ()
;;               (global-set-key [(control c) (j)] 'helm-imenu)
;;               ))

(provide 'helm-common_init)
;;; helm-common_init.el ends here
