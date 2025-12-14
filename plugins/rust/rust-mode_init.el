;; -*- lexical-binding: t; -*-

(require 'rust-mode)

;; (setq rust-format-on-save t)

(add-hook 'rust-mode-hook
          #'(lambda ()
              (setq-local company-idle-delay 0.2)
              (setq-local company-minimum-prefix-length 3)
              (local-set-key [(control c) (control c)] 'rust-format-buffer)
              (local-set-key [(control c) (return)] 'rust-run)
              (local-set-key [(control c) (control t)] 'rust-test)
              (local-set-key [(control c) (tab)] 'rust-compile)
              ))

(with-eval-after-load 'dap-mode
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil))
  )

;; (require 'cargo_init)

;; Rust 里 ' 既用于 字符字面量（'x'），也用于 生命周期（'a / &'a T）。
;; 你显然是想让它在写字符时帮你补齐，但别在写生命周期时乱插一个多余的 '。
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

(provide 'rust-mode_init)
;;; rust-mode_init.el ends here
