(require 'rust-ts-mode)

;; (setq rust-format-on-save t)

(add-hook 'rust-ts-mode-hook
          #'(lambda ()
              (setq-local company-idle-delay 0.2)
              (setq-local company-minimum-prefix-length 3)
              (local-set-key [(control c) (control m)] 'rust-format-buffer)
              (local-set-key [(control c) (return)] 'rust-run)
              (local-set-key [(control c) (tab)] 'rust-compile)
              ))

;; 需要安装的依赖：
;; cargo install audit cargo-outdated
(require 'cargo)
(with-eval-after-load 'cargo
  (setq compilation-ask-about-save nil)   ; 运行 cargo 编译前，自动保存项目内所有文件。
  (define-key cargo-process-mode-map [(n)] 'compilation-next-error)
  (define-key cargo-process-mode-map [(p)] 'compilation-previous-error)
  (define-key cargo-process-mode-map [(control \8)] 'quit-window)

  (add-hook 'rust-ts-mode-hook
            #'(lambda ()
                ;; (define-key cargo-minor-mode-map (kbd "C-c C-e") 'cargo-process-bench)
                ;; (define-key cargo-minor-mode-map (kbd "C-c C-x") 'cargo-process-run-example)
                (local-set-key (kbd "C-c C-a") 'cargo-process-add)
                (local-set-key (kbd "C-c C-S-a") 'cargo-process-audit)
                (local-set-key (kbd "C-c C-s") 'cargo-process-search)
                (local-set-key (kbd "C-c C-o") 'cargo-process-doc-open)
                (local-set-key (kbd "C-c C-S-o") 'cargo-process-outdated)
                (local-set-key (kbd "C-c C-S-d") 'cargo-process-rm)
                (local-set-key (kbd "C-c C-u") 'cargo-process-update)
                (local-set-key (kbd "C-c C-S-u") 'cargo-process-upgrade)
                (local-set-key (kbd "C-c C-r") 'cargo-process-run)
                (local-set-key (kbd "C-c <return>") 'cargo-process-run)
                (local-set-key (kbd "C-c <tab>") 'cargo-process-build)
                (local-set-key (kbd "C-c t") 'cargo-process-test)
                (local-set-key (kbd "C-c C-c") 'cargo-process-repeat)
                ))

  (with-eval-after-load 'shackle
    (add-to-list 'shackle-rules '(cargo-process-mode :select t :size 0.3 :autoclose t))
    )
  )

(with-eval-after-load 'dap-mode
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  )

(provide 'rust-ts-mode_init)
;;; rust-ts-mode_init.el ends here
