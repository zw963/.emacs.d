;; ------------------------------ secondary  ------------------------------
;; browse-kill-ring+ 的目的是让你可以同时使用 king-ring,  secondary-ring.
;; browse-kill-ring+ 会自动尝试加载 second-sel, 并使用它作为 secondary-ring
(require 'browse-kill-ring+)

(setq browse-kill-ring-separator "\f")
(setq kill-do-not-save-duplicates t)
(setq browse-kill-ring-no-duplicates t)
(setq add-secondary-to-ring nil)

;; (defun yank-pop-secondary-ring (&optional arg)
;;   (interactive)
;;   (let ((browse-kill-ring-current-ring 'secondary-selection-ring))
;;     (call-interactively 'yank-pop)))

;; 默认, Emacs 提供了 yank-pop (Alt+y), 用来逐个的弹出 king-ring 中的内容.
;; (global-set-key [remap yank-pop] 'yank-pop-secondary-ring) ;;; Alt + y 默认弹出 secondary-ring

;; yank-pop-commands hack 了 yank-pop, 使它同时支持 king-ring, secondary-ring.
;; 实现的功能:
;; 1. C-y, 然后 M-y..M-y, 滚动 kill-ring
;; 2. C-M-y, 然后 M-y..M-y,  滚动 secondary-ring, 并滚动 secondary-ring
;; 3. 直接按下 M-y, 显示 kill-ring 列表

(global-set-key [remap yank-pop] 'yank-pop-commands)
(global-set-key [(control meta y)] 'yank-secondary) ; C-M-y 粘贴 secondary ring.
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))
(define-key isearch-mode-map (kbd "C-M-y")  'isearch-yank-secondary)
(add-hook 'browse-kill-ring-mode-hook
          '(lambda ()
             (define-key browse-kill-ring-mode-map [(control \8)] 'kill-buffer-and-window)
             (define-key browse-kill-ring-mode-map [(O)] 'browse-kill-ring-occur)
             (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert-move-and-quit)
             (define-key browse-kill-ring-mode-map [(control g)] 'browse-kill-ring-quit)
             ;; 为了让 M-y 和 global-hl-line-mode 一起工作,
             ;; 必须在 browse-king-ring-mode 中关闭 hl-line-mode
             ;; 否则 n, p 快捷键不工作.
             (make-local-variable 'global-hl-line-mode)
             (setq global-hl-line-mode nil)
             ))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'kill-ring-save)
        (kill-append (buffer-substring beg end) (< end beg))
      (copy-region-as-kill beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun convert-to-secondary-region ()
  (interactive)
  (primary-to-secondary (region-beginning) (region-end))
  (delete-overlay mouse-secondary-overlay)
  (deactivate-mark))

;; 这个功能会造成 meta + w 复制内容之后, 不会自动的 deactivate mark
;; 因此 deactivate-mark-hook 中的代码不会运行.

;; (defadvice kill-ring-save (around secondary-ring activate)
;;   "Let 'kill-ring-save support secondary ring."
;;   (when (use-region-p)
;;       (progn
;;         (convert-to-secondary-region)
;;         ad-do-it
;;         )
;;     ))

;; (defadvice kill-region (around secondary-ring activate)
;;   "Let 'kill-region support secondary ring."
;;   (if (use-region-p)
;;       (progn
;;         (convert-to-secondary-region)
;;         ad-do-it
;;         )
;;     (call-interactively 'kill-whole-line)))

;; (require 'popup-kill-ring)
;; (global-set-key [(meta y)] 'popup-kill-ring)

;;------------------------------ 显示分页符。 ------------------------------
(require 'page-break-lines)
(add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)
(global-page-break-lines-mode 1)

(provide 'browse-kill-ring_init)
;;; browse-kill-ring_init.el ends here
