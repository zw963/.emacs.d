;; -*- lexical-binding: t; -*-

(require 'rustic)

;; 当开启 rustic-mode 时，会自动的 require 'lsp-rust,
;; 并作适当的调整，最后，会尝试开启 lsp server. (lsp)

;; $: cargo install cargo-outdated cargo-fix cargo-edit

;; (setq rustic-lsp-server 'rls)

;; (setq rustic-compile-display-method)
;; (setq rustic-compile-backtrace)
;; (setq rustic-compile-command)
(setq rustic-lsp-format t)
(setq rustic-format-trigger 'on-compile)
(setq compilation-read-command nil) ;; not prompt on minibuffer when do compile.
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

(defun my-rustic-hook ()
  (setq-local company-idle-delay 0.2)
  (setq-local company-minimum-prefix-length 1)
  ;; rustic-format-buffer
  (local-set-key [(control c) (return)] 'rustic-cargo-run)
  (local-set-key [(control c) (control c)] 'rustic-format-buffer)
  (local-set-key [(control c) (tab)] 'rustic-recompile)
  (local-set-key [(control c) (t)] 'rustic-cargo-test)
  (local-set-key [(control c) (b)] 'rustic-cargo-build)
  ;; (local-set-key [(control c) (r)] 'rustic-cargo-run)
  )

(add-hook 'rustic-mode-hook 'my-rustic-hook)

(defun rustic-compilation-mode-hack ()
  (local-set-key [(control \8)] 'quit-window)
  )

(add-hook 'rustic-cargo-run-mode-hook 'rustic-compilation-mode-hack)
(add-hook 'rustic-compilation-mode-hook 'rustic-compilation-mode-hack)

;; 这个和 shackle 配合使用
(setq rustic-compile-display-method 'pop-to-buffer)

;; (with-eval-after-load 'popwin
;;   (add-to-list 'popwin:special-display-config '(rustic-compilation-mode :noselect t)))

;; (with-eval-after-load 'shackle
;;   (add-to-list 'shackle-rules '(rustic-cargo-run-mode :select t :size 0.3 :autoclose t))
;;   )

;; 不使用 cargo, 直接用 rustc 编译。
;; (setq rustic-compile-command "rustc ./src/main.rs")

(provide 'rustic_init)

;;; rustic_init.el ends here
