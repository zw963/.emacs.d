(require 'mode-compile)

(setq
 mode-compile-reading-time 0.5
 mode-compile-expert-p t                    ;关闭提示, 立即返回结果.
 mode-compile-always-save-buffer-p t        ;调用时，首先自动保存 buffer.
 mode-compile-save-all-p nil                ;是否修改所有被保存的 buffer.(默认关闭)
 ;; mode-compile-never-edit-command-p t      ; 从来无需编辑，让 mode-compile 选择
 )


(add-list-to-list 'mode-compile-modes-alist
                  '(
                    ;; 将 enh-ruby-mode 加入, 默认仅仅支持 ruby-mode.
                    (enh-ruby-mode . (ruby-compile kill-compilation))
                    ;; (coffee-mode . (coffee-compile kill-compilation))
                    ))

(add-list-to-list 'mode-compile-filename-regexp-alist
                  '(
                    ("Rakefile" . ruby-mode)
                    ))

(add-list-to-list 'mode-compile-shell-alist
                  '(
                    ("bash" . sh-mode)
                    ("elixir" . elixir-mode)
                    ))

(add-hook 'mode-compile-before-compile-hook
          '(lambda ()
             ;; 动态的获取 command 及 dbg-flags.
             (let ((git-root (locate-dominating-file default-directory ".git")))
               (cond
                ((member major-mode '(ruby-mode enh-ruby-mode))
                 (setq ruby-dbg-flags (concat "-I.:" git-root "lib:" git-root "spec:" git-root "test"))
                 (cond
                  ((ruby-testfile-p) (setq ruby-command "rake"))
                  ((ruby-specfile-p) (setq ruby-command "rspec --format documentation "))
                  ((string-match
                    "\\(rakefile\\|Rakefile\\|.*\\.rake\\)$"
                    (buffer-name))
                   (setq ruby-command "rake" ruby-dbg-flags "--rakefile"))
                  (t
                   (setq ruby-command "ruby" ruby-dbg-flags "-w"))))
                ((equal major-mode 'elixir-mode)
                 (setq elixir-command
                       (if (equal (file-name-extension (buffer-file-name)) "ex")
                           "elixirc"
                         "elixir"
                         )))))))

(add-hook 'mode-compile-after-compile-hook
          '(lambda ()
             (switch-to-buffer-other-window "*compilation*")
             ))

;; (when (buffer-modified-p) (basic-save-buffer-1))

;; (setq coffee-command "coff")
;; (setq coffee-dbg-flags "")
;; (defun coffee-compile ()
;;   (mc--shell-compile coffee-command coffee-dbg-flags nil))

;; ;; elixir compile
;; (require 'erlang)
;; (add-to-list 'auto-mode-alist '("\\.\\(erl\\)$" . erlang-mode))
;; (require 'elixir-mode)
;; (require 'flymake-elixir)
;; (add-hook 'elixir-mode-hook 'flymake-elixir-load)
;; (add-to-list 'mode-compile-modes-alist '(elixir-mode . (elixir-compile nil)))
;; (setq elixir-command "elixirc")
;; (setq elixir-dbg-flags "")
;; (defun elixir-compile ()
;;   (interactive)
;;   (mc--shell-compile elixir-command elixir-dbg-flags nil))

(add-to-list 'mode-compile-modes-alist '(feature-mode . (feature-mode-compile nil)))
(setq feature-command "cucumber")
(setq feature-dbg-flags "--no-color")
(defun feature-mode-compile ()
  (interactive)
  (mc--shell-compile feature-command feature-dbg-flags nil))

(defun turn-on-mode-compile ()
  (local-set-key [(control c) (return)] 'mode-compile)
  (local-set-key [(control c) (?\r)] 'mode-compile))

(add-hook 'prog-mode-hook 'turn-on-mode-compile)
(add-hook 'feature-mode-hook 'turn-on-mode-compile)

(provide 'mode-compile_init)
;;; mode-compile_init.el ends here
