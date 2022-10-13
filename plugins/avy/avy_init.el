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

(defun go-to-line-and-column-cond (lc-cond)
  "Allow a specification of LINE:COLUMN or LINE,COLUMN instead of just COLUMN.
Just :COLUMN or ,COLUMN moves to the specified column on the current line.
LINE alone still moves to the beginning of the specified line (like LINE:0 or LINE,0).
By Default I'm bind it to M-g M-l.
The default value of the COLUMN is decrement by -1
because all compilers consider the number of COLUMN from 1 (just for copy-past)"
  (interactive "sLine:Column:: ")
  (let (line delim column max-lines)
    (setq max-lines (count-lines (point-min) (point-max)))
    (save-match-data
      (string-match "^\\([0-9]*\\)\\([,:]?\\)\\([0-9]*\\)$" lc-cond)
      (setq line (string-to-number (match-string 1 lc-cond)))
      (setq delim (match-string 2 lc-cond))
      (setq column (string-to-number (match-string 3 lc-cond)))
      (if (= 0 line) (setq line (line-number-at-pos)))
      (if (> line max-lines) (setq line max-lines))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (message "Marker set to line %d column %s" (line-number-at-pos) (current-column))
      )))

(defun avy-goto-line-fixed (&optional arg)
  "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG."
  (interactive "p")
  (setq arg (or arg 1))
  (if (not (memq arg '(1 4)))
      (progn
        (goto-char (point-min))
        (forward-line (1- arg)))
    (avy-with avy-goto-line
      (let* ((avy-handler-old avy-handler-function)
             (avy-handler-function
              (lambda (char)
                (if (or (< char ?0)
                        (> char ?9))
                    (funcall avy-handler-old char)
                  (let ((line (read-from-minibuffer
                               "Goto line: " (string char))))
                    (when line
                      (avy-push-mark)
                      (save-restriction
                        (widen)
                        ;; (goto-char (point-min))
                        ;; (forward-line (1- (string-to-number line)))
                        (go-to-line-and-column-cond line)
                        )
                      (throw 'done 'exit))))))
             (r (avy--line (eq arg 4))))
        (when (and (not (memq r '(t nil))) (eq avy-action #'identity))
          (avy-action-goto r))))))

(global-set-key [remap goto-line] 'avy-goto-line-fixed)

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

;; (require 'goto-char-preview)
;; (global-set-key [remap goto-char] 'goto-char-preview)

(require 'ace-window)
;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-scope 'visible)
(setq aw-dispatch-always t)
;; ace-window 激活后，有一个有用的快捷键 m, 用来交换当前 window 和指定的 window.
;; 或者直接 C-u C-x o, 直接就是 swap window, C-u C-u C-x 0, 删除指定的 window.
(global-set-key [remap other-window] 'ace-window)

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
(require 'ace-popup-menu)
(ace-popup-menu-mode 1)

(require 'char-menu)

(global-set-key (kbd "<menu> SPC") #'char-menu)

(setq char-menu '("—" "‘’" "“”" "…" "«»" "–"
 ("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
 ("Math"       "≈" "≡" "≠" "∞" "×" "±" "∓" "÷" "√")
 ("Arrows"     "←" "→" "↑" "↓" "⇐" "⇒" "⇑" "⇓")
 ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ"
               "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")))

(require 'ace-link)
;; 进入 help-mode, 测试快捷键 o.
(ace-link-setup-default)

(require 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)

(provide 'avy_init)
;;; avy_init.el ends here
