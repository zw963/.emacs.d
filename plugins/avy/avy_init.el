(require 'avy)
;; avy-goto-word-1 avy-goto-char
;; (global-set-key (kbd "C-:") 'avy-goto-char-2)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)

(add-hook 'org-mode-hook
          (lambda ()
            (global-set-key [(control j)] 'avy-goto-char-timer)
            ))

(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (define-key treemacs-mode-map [(control j)] 'avy-goto-char-timer)
              ))
  )
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
(global-set-key [remap zap-to-char] 'avy-zap-to-char-dwim)

;; (setq avy-zap-function 'delete-region)

(require 'goto-char-preview)

;; M-g c, 这个更多的是在调试时使用，打印错误，告诉那个位置出错，直接跳过去。
(global-set-key [remap goto-char] 'goto-char-preview)

(require 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'visible)
(setq aw-dispatch-always t)

;; ace-window 激活后，有一个有用的快捷键 m, 用来交换当前 window 和指定的 window.
;; 或者直接 C-u C-x o, 直接就是 swap window, C-u C-u C-x 0, 删除指定的 window.
;; (global-set-key [remap other-window] 'ace-window)
(global-set-key [remap other-window] (lambda ()
                                       (interactive)
                                       (progn
                                         (setq unread-command-events (listify-key-sequence (kbd "?"))) ;; Queue "?" key command to be sent
                                         (ace-window nil))
                                       ))

(set-face-attribute
 'aw-leading-char-face nil
 :foreground "deep sky blue"
 :weight 'bold
 :height 3.0)

(set-face-attribute
 'aw-mode-line-face nil
 :inherit 'mode-line-buffer-id
 :foreground "lawn green")

;; 这个和 treemacs rightclick menu 冲突.
;; 在某些情况下，这个无法方便的像 helm 一样过滤，不够方便
;; (require 'ace-popup-menu)
;; (ace-popup-menu-mode 1)
;; (with-eval-after-load 'yasnippet
;;   ;; 当设定为 yas-x-prompt 时, 会使用 ace-popup-menu
;;   (setq yas-prompt-functions '(yas-x-prompt))
;;   )

(require 'char-menu)
(setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
                  ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                  ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
                  ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
                  ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ"
                   "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")))

(require 'ace-link)
;; 进入 help-mode, 测试快捷键 o.
(ace-link-setup-default)

(require 'goto-line_init)

(provide 'avy_init)
;;; avy_init.el ends here
