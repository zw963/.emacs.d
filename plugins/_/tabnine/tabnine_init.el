;; -*- lexical-binding: t; -*-

;; (require 'tabnine-capf)
;; (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)

;; (setq company-tabnine-binaries-folder (expand-file-name "plugins/company-mode/plugins/company-tabnine/"))

;; 注意：在编辑器内，输入 Tabnine::config 使用浏览器打开配置页面。
(defun add-tabnine-backend ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends #'company-tabnine)
  )

(with-eval-after-load 'company
  (require 'company-tabnine)

  ;; 你想在哪些 major-mode 里启用 TabNine，集中在这里
  (defcustom my-company-tabnine-modes
    '(sh-mode bash-ts-mode
              makefile-gmake-mode cmake-ts-mode
              crystal-mode
              ruby-mode enh-ruby-mode ruby-ts-mode)
    "Major modes where company-tabnine should be enabled."
    :type '(repeat symbol)
    :group 'company)

  (defun my-company--tabnine-should-enable-p ()
    (and (bound-and-true-p company-mode)
         (not (minibufferp))
         (memq major-mode my-company-tabnine-modes)))

  (defun my-company-add-backend-after-capf (backend)
    "Add BACKEND into the same backend group as `company-capf` (buffer-locally).
If no capf backend exists, append BACKEND at the end."
    (let ((found nil))
      ;; 避免和全局/别的 buffer 共享 cons 结构（尤其你后面还可能 append 到子 list）
      (setq-local company-backends (copy-tree company-backends))
      (setq-local company-backends
                  (mapcar (lambda (item)
                            (cond
                             ;; capf 已经在一个组里：把 backend 追加进去
                             ((and (listp item) (memq 'company-capf item))
                              (setq found t)
                              (if (memq backend item) item (append item (list backend))))
                             ;; capf 在顶层：把它变成一个组
                             ((eq item 'company-capf)
                              (setq found t)
                              (list 'company-capf backend))
                             (t item)))
                          company-backends))
      (unless found
        (add-to-list 'company-backends backend t))))

  (defun my-company-maybe-enable-tabnine ()
    "Enable company-tabnine for eligible buffers."
    (when (my-company--tabnine-should-enable-p)
      (my-company-add-backend-after-capf 'company-tabnine)))

  ;; 关键：只挂一个地方，不要在十几个 mode-hook 里散弹式 add-hook
  (add-hook 'company-mode-hook #'my-company-maybe-enable-tabnine))

;; 更稳一点的方式是：在 company-mode-hook 里做一次，然后用 mode 条件过滤；或者把 tabnine 放到你想要的后端组里（避免破坏原有分组）。你现在这种“每个 mode hook 都加一次”虽然 add-to-list 会去重，但结构上仍然很散。

(add-hook 'sh-mode-hook 'add-tabnine-backend)
(add-hook 'bash-ts-mode-hook 'add-tabnine-backend)
(add-hook 'makefile-gmake-mode-hook 'add-tabnine-backend)
(add-hook 'cmake-ts-mode-hook 'add-tabnine-backend)

;; (with-eval-after-load 'rust-mode (add-hook 'rust-mode-hook 'add-tabnine-backend))
;; (with-eval-after-load 'rustic-mode (add-hook 'rustic-mode-hook 'add-tabnine-backend))
(with-eval-after-load 'crystal-mode (add-hook 'crystal-mode-hook 'add-tabnine-backend))

(run-ruby-mode-hook '(add-tabnine-backend))

;; dart-mode 开启的话，很卡
;; (with-eval-after-load 'dart-mode (add-hook 'dart-mode-hook 'add-tabnine-backend))


(provide 'tabnine_init)

;;; tabnine_init.el ends here
