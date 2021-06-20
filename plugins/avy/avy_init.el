(require 'avy)
;; (global-set-key [(control c) (j)] 'avy-goto-char)
(global-set-key (kbd "M-o") 'avy-goto-word-1)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
;; (global-set-key [(control c) (r)] 'avy-resume)
(define-key isearch-mode-map [(control \')] 'avy-isearch)

(setq avy-background t)
(setq avy-style 'pre)

(setq avy-keys '
      (
       ?q ?w ?e ?r
       ?u ?i ?o ?p
       ?a ?s ?d ?f
       ?j ?k ?l
       ?c ?v
       ?n ?m
       ))

(with-eval-after-load 'super-save
  (defadvice ace-window (before super-save activate) (super-save-command) nil))

(require 'avy-zap)
(global-set-key [(control c) (d)] 'avy-zap-to-char-dwim)
;; (setq avy-zap-function 'delete-region)

(require 'goto-char-preview)
(global-set-key [remap goto-char] 'goto-char-preview)

(require 'ace-window)
(setq aw-scope 'visible)
;; (setq aw-minibuffer-flag t)
(setq aw-ignore-current t)
;; (setq aw-dispatch-always t)
(global-set-key [remap other-window] 'ace-window)

;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'ace-popup-menu)
(ace-popup-menu-mode 1)

(require 'ace-link)
;; 进入 help-mode, 测试快捷键 o.
(ace-link-setup-default)

(provide 'avy_init)
;;; avy_init.el ends here
