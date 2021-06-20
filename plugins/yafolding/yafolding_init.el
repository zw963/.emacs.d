;; (require 'yafolding)

;; (add-hook 'ruby-mode-hook 'yafolding-mode)
;; (add-hook 'enh-ruby-mode-hook 'yafolding-mode)
;; (add-hook 'nxml-mode-hook 'yafolding-mode)
;; (add-hook 'yaml-mode-hook 'yafolding-mode)

;; (with-eval-after-load 'all-the-icons
;;   (setq yafolding-ellipsis-content (all-the-icons-material "unfold_more"))
;;   )

(require 'hideshow)
;; 某些模式下, 可能需要定制 hs-hide-all-non-comment-function.
;; 参加下面的例子, the following code shows the next nested level in addition to
;; the top-level:
;;   (defun ttn-hs-hide-level-1 ()
;;     (when (hs-looking-at-block-start-p)
;;       (hs-hide-level 1))
;;     (forward-sexp 1))
;;   (setq hs-hide-all-non-comment-function 'ttn-hs-hide-level-1)

(add-hook 'prog-mode-hook 'hs-minor-mode)

(add-hook 'hs-minor-mode-hook '(lambda ()
                                 (local-set-key [(control c) (/)] 'hs-hide-all)
                                 (local-set-key [(control c) (\\)] 'hs-show-all)
                                 (local-set-key [(control tab)] 'hs-toggle-hiding)
                                 ))

(provide 'yafolding_init)
;;; yafolding_init.el ends here
