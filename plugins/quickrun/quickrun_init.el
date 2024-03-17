;; (setq quickrun-option-default-directory ".quickrun/")
;; (defun quickrun--create-option-default-directory ()
;;   (if (file-directory-p quickrun-option-default-directory)
;;       t
;;     (make-directory quickrun-option-default-directory t)))
;; (advice-add 'quickrun :before #'quickrun--create-option-default-directory)

(require 'quickrun)

(defun quickrun--temp-name (src)
  "Not documented."
  (let* ((extension (file-name-extension src))
         (suffix (or (and extension (concat "." extension)) ""))
         (dir (quickrun--default-directory)))
    (expand-file-name (concat dir (make-temp-name ".qr_") suffix))))

(setq quickrun-timeout-seconds nil)

(add-to-list 'quickrun--major-mode-alist '(enh-ruby-mode . "ruby"))

;; (setq quickrun-focus-p nil)

;; (setq quickrun-debug t)

(dolist (hook '(
                ruby-mode-hook
                ruby-ts-mode-hook
                enh-ruby-mode-hook
                dart-mode-hook
                go-mode-hook
                sh-mode-hook
                bash-ts-mode-hook
                elixir-ts-mode-hook
                ))
  (add-hook hook (lambda ()
                   (local-set-key [(control x) (control e)] 'quickrun-region)
                   (local-set-key [(control c) (return)] 'quickrun)
                   ;; (local-set-key (kbd "C-c RET") 'quickrun)
                   (local-set-key [(control c) (tab)] 'quickrun-compile-only)
                   )))


(add-hook 'quickrun-after-run-hook 'say)

(provide 'quickrun_init)

;;; quickrun_init.el ends here
