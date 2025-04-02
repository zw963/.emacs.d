;; 运行 shell 启动 shell-mode

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)
 '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix nil)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only t)           ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () ""))      ; what to run when i press enter on a
                                        ; line above the current prompt
 )

(add-hook 'shell-mode-hook (lambda ()
                             (ansi-color-for-comint-mode-on)
                             (define-key shell-mode-map [(control n)] 'comint-next-input)
                             (define-key shell-mode-map [(control p)] 'comint-previous-input)
                             (define-key shell-mode-map [(control r)] 'comint-previous-matching-input-from-input)
                             ))

(require 'vterm_init)

(provide 'shell_init)

;;; shell_init.el ends here
