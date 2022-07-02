(require 'mint-mode)

(setq js-indent-level 2)

(defun mint--format-args ()
  (append
   '("format")
   '("--stdin")
   `(,(buffer-file-name))
   )
  )

(defun mint--format-exit-code (code)
  t
  ;; (when (or (eq code 0) (eq code 1))
  ;;   t
  ;;   )
  )

;; (reformatter-define mint-format
;;   :program "mint"
;;   :args (mint--format-args)
;;   :stdout nil
;;   :input (reformatter-temp-file-in-current-directory "mint")
;;   :exit-code-success-p mint--format-exit-code
;;   )

(reformatter-define mint-format
  :program "mint"
  :args '("format" "--stdin")
  ;; :stdout nil
  ;; :input (reformatter-temp-file-in-current-directory "mint")
  ;; :exit-code-success-p mint--format-exit-code
  )

;; 自动定义了以下方法:
;; mint-format-buffer mint-format-region
;; 以及一个 minor-mode
;; mint-format-on-save-mode
(add-hook 'mint-mode-hook (lambda ()
                            ;; 这个我改源码暂时关闭掉了
                            ;; (add-hook 'after-save-hook #'mint-format-file nil 'local)
                            (mint-format-on-save-mode 1)
                            (define-key mint-mode-map (kbd "C-c C-c") 'mint-format-buffer)
                            (setq-local js-indent-level 2)
                            ))

(provide 'mint-mode_init)

;;; mint-mode_init.el ends here
