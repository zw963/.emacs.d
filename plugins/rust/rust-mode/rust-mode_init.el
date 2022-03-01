(require 'rust-mode)

(setq rust-format-on-save t)

(with-eval-after-load 'lsp-mode
  (require 'lsp-rust)
  (require 'lsp-modeline)

  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-full-docs t)

  ;; (setq lsp-rust-server 'rls)

  ;; 注意，如果用 rls, 必须把 lsp-enable-imenu 设为 nil, ruby-analyzer 无需。
  ;; (setq-local lsp-enable-imenu nil)
  (add-hook 'rust-mode-hook 'lsp-deferred)
  )

(defvar electric-pair-inhibit-predicate-mode-chars-alist
  '((t . nil))
  "A list of major-mode and inhibit chars.

Each element is in the form of (MODE . (CHAR/CHAR-STRING/CHAR-FUNCTION ...)).

MODE
    A mode, or t for all modes.

CHAR
    A character to match the input. for example:

        ?\{

CHAR-STRING
    A pair of character and string, the character to match the input,
    the string for ‘looking-back’. for example:

        (?\{ . \":{\")

CHAR-FUNCTION
    A pair of character and function, the character to match the input,
    the function accept the input character as parameter. for example:

        (?\{ . (lambda (_c)
                 (eq ?: (char-before (1- (point))))))")

(defun electric-pair-inhibit-predicate-function (c)
  (let ((alist
         (append
          (assoc-default major-mode electric-pair-inhibit-predicate-mode-chars-alist)
          (assoc-default t          electric-pair-inhibit-predicate-mode-chars-alist))))
    (or (cl-member c
                   alist
                   :test
                   (lambda (c it)
                     (cond
                      ((characterp it) (equal c it))
                      ((and (consp it) (equal c (car it)))
                       (cond ((stringp   (cdr it)) (looking-back (cdr it) 1))
                             ((functionp (cdr it)) (funcall (cdr it) c)))))))
        (electric-pair-default-inhibit c))))

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate
        #'electric-pair-inhibit-predicate-function))

(add-to-list 'electric-pair-inhibit-predicate-mode-chars-alist
             '(rust-mode . ((?' . "\&'"))))
(modify-syntax-entry ?' "\"" rust-mode-syntax-table)

(add-hook 'rust-mode-hook
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

  (add-hook 'rust-mode-hook
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

(provide 'rust-mode_init)
;;; rust-mode_init.el ends here
