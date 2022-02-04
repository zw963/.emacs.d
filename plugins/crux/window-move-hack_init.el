;;;;;;;;;;;;;;;;;;;;;;;;;;
;; super n super p hack ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map [(meta n)] [(super n)])
(define-key key-translation-map [(meta p)] [(super p)])
(global-set-key [(super n)] 'window-move-up) ;光标位置不变，窗口向上移动四行
(global-set-key [(super p)] 'window-move-down) ;光标位置不变，窗口向下移动两行

(with-eval-after-load 'iedit
  (add-hook 'iedit-mode-hook (lambda ()
                               (define-key iedit-lib-keymap [(super p)] 'iedit-prev-occurrence)
                               (define-key iedit-lib-keymap [(super n)] 'iedit-next-occurrence)
                               )))

(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(super n)] 'isearch-repeat-forward)
            (define-key isearch-mode-map [(super p)] 'isearch-repeat-backward)
            ))

(add-hook 'compilation-mode-hook
          (lambda ()
            (define-key compilation-mode-map [(super n)] 'compilation-next-error)
            (define-key compilation-mode-map [(super p)] 'compilation-previous-error)
            ))

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (define-key minibuffer-local-map [(super n)] 'next-history-element)
            (define-key minibuffer-local-map [(super p)] 'previous-history-element)
            ))

(add-hook 'diff-mode-hook
          (lambda ()
            (define-key diff-mode-map [(super n)] 'diff-hunk-next)
            (define-key diff-mode-map [(super p)] 'diff-hunk-prev)
            ))

(provide 'window-move-hack_init)

;;; window-move_init.el ends here
