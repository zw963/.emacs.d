;;;;;;;;;;;;;;;;;;;;;;;
;; builtin functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; 添加几个需要的前缀。
(define-prefix-command 'meta-c-map)
(global-set-key [(meta c)] 'meta-c-map)

(global-set-key [(f2)] 'kill-buffer-enhanced) ;关闭当前缓冲区 F2

(global-set-key [(control c) (j)] 'imenu)

(global-set-key [(meta D)] (lambda () (interactive) (dired "./"))) ; 打开 dired buffer.
(global-set-key [(control s)] 'isearch-forward-regexp) ;更改Ctrl-s为正则搜索

(define-key indent-rigidly-map [(control b)] 'indent-rigidly-left)
(define-key indent-rigidly-map [(control f)] 'indent-rigidly-right)
(define-key indent-rigidly-map [(meta b)]  'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map [(meta f)] 'indent-rigidly-right-to-tab-stop)

(add-hook 'isearch-mode-hook
          (lambda ()
            (define-key isearch-mode-map [(control f)] 'isearch-yank-char)
            (define-key isearch-mode-map [(control b)] 'isearch-delete-char)
            (define-key isearch-mode-map [(meta f)] 'isearch-yank-word-or-char)
            (define-key isearch-mode-map [(control e)] 'isearch-yank-line)
            (define-key isearch-mode-map [(control j)] 'isearch-yank-until-char)
            (define-key isearch-mode-map [(control y)] 'isearch-yank-kill)
            (define-key isearch-mode-map [(meta \')] 'expand-abbrev)
            (define-key isearch-mode-map [(meta n)] 'isearch-repeat-forward)
            (define-key isearch-mode-map [(meta p)] 'isearch-repeat-backward)
            (define-key isearch-mode-map [(meta \5)] 'isearch-query-replace-regexp)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; self-defined functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'crux)
(require 'move-dup)
;; (require 'mark-lines)

(global-set-key [(control x) (\4) (t)] 'crux-transpose-windows)
;; (global-set-key [(control k)] 'crux-smart-kill-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [(control return)] 'crux-smart-open-line)
;; (global-set-key [(control c) (r)] 'crux-rename-file-and-buffer)
;; (global-set-key [(shift f2)] 'rename-current-buffer-file)
(global-set-key [(control c) (r)] 'rename-current-buffer-file)

(global-set-key [(meta t)] 'translate-this-word-or-region)
(global-set-key [(control c) (D)] 'crux-delete-file-and-buffer)
(global-set-key [(control c) (c)] 'crux-copy-file-preserve-attributes)
(global-set-key [(control c) (b)] 'crux-switch-to-previous-buffer)
(global-set-key [(shift return)] 'crux-duplicate-current-line-or-region)
(global-set-key [(shift ?\r)] 'crux-duplicate-current-line-or-region)
(global-set-key [(shift meta return)] 'crux-duplicate-and-comment-current-line-or-region)
(global-set-key [(meta up)] 'move-dup-move-lines-up)
(global-set-key [(meta down)] 'move-dup-move-lines-down)
(global-set-key [(meta o)] 'crux-other-window-or-switch-buffer)
(global-set-key [(control c) (k)] 'crux-kill-other-buffers)
(global-set-key [(control c) (tab)] 'crux-indent-rigidly-and-copy-to-clipboard)
(global-set-key [(control ?^)] 'crux-top-join-line)
(global-set-key [(control backspace)] 'crux-kill-line-backwards)
(global-set-key [(control ?\d)] 'crux-kill-line-backwards)
(global-set-key [(control c) (control k)] 'save-buffer-and-kill-buffer-and-window)
(global-set-key [(control meta c)] 'copy-current-buffer-file-name)
(global-set-key '[(backtab)] 'backtab-space)
(global-set-key [(control x) (\2)] 'split-window-below-then-switch-to)
(global-set-key [(control x) (\3)] 'split-window-right-then-switch-to)
(global-set-key [(control right)] 'transpose-current-char) ;光标前所在字母右移
(global-set-key [(control left)] 'transpose-current-char-backward) ;光标前所在字母左移
(global-set-key [(meta k)] 'mark-next-line)
;; (global-set-key [(control c) (?\t)] 'crux-indent-rigidly-and-copy-to-clipboard)

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

(global-set-key [(meta P)] 'other-window-move-down) ;下一个窗口向下移动两行
(global-set-key [(meta N)] 'other-window-move-up) ;下一个窗口向上移动一行
(global-set-key [(meta n)] 'window-move-up) ;光标位置不变，窗口向上移动四行
(global-set-key [(meta p)] 'window-move-down) ;光标位置不变，窗口向下移动两行
;; (require 'window-move-hack_init)

(crux-reopen-as-root-mode 1)

(defun split-window-below-then-switch-to (&optional size)
  (interactive)
  (split-window-below size) (other-window 1))
(defun split-window-right-then-switch-to (&optional size)
  (interactive)
  (split-window-right size) (other-window 1))

(defun other-window-move-up (&optional arg)
  "Other window move-up 1 lines."
  (interactive "p")
  (scroll-other-window arg))

(defun other-window-move-down (&optional arg)
  "Other window move-down 2 lines."
  (interactive "P")
  (if arg
      (scroll-other-window-down arg)
    (scroll-other-window-down 2)))

(defun window-move-up (&optional arg)
  "Window move-up 2 lines."
  (interactive "P")
  (if arg
      (scroll-up arg)
    (scroll-up 2)))

(defun window-move-down (&optional arg)
  "Window move-down 3 lines."
  (interactive "P")
  (if arg
      (scroll-down arg)
    (scroll-down 3)))


(defun backtab-space (&optional indent-count)
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at (concat "^" (make-string (or indent-count 2) ? )))
        (replace-match "")))))

(defun crux-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%F %T %Z" (current-time))))

(defun save-buffer-and-kill-buffer-and-window ()
  "Simple convenience function.
  Saves the buffer of the current day's entry and kills the window
  Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun translate-this-word-or-region ()
  (interactive)
  (if (use-region-p)
      (run-process "trans" "-b" "en:zh-CN" (buffer-substring (region-beginning) (region-end)))
    (run-process "dict1" (current-word t t))
    ))

(defun copy-current-buffer-file-name ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-copy-filename-as-kill 0)
    (let ((root (if (locate-dominating-file default-directory ".git")
                    (expand-file-name (locate-dominating-file default-directory ".git"))
                  "")))
      (kill-new (replace-regexp-in-string (regexp-quote root) "" (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
      )))

;; (defun textmate-next-line ()
;;   "Inserts an indented newline after the current line and moves the point to it."
;;   (interactive)
;;   (end-of-line)
;;   (cond
;;    ((member major-mode '(rust-mode rustic-mode)) (insert ";")
;;     ))
;;   ;; (electric-newline-and-maybe-indent)
;;   ;; (newline-and-indent)

;;   (if (fifth (syntax-ppss))
;;       ;; 记住: 总可以通过Ctrl+Alt+j 执行下面的功能.(注释中新行自动注释)
;;       (default-indent-new-line)
;;     (reindent-then-newline-and-indent)))

(defun transpose-current-char (&optional arg)
  "Move current char right."
  (interactive "p")
  (forward-char 1)
  (transpose-chars 1)
  (forward-char -1))

(defun transpose-current-char-backward (&optional arg)
  "Move current word left."
  (interactive "p")
  (forward-char 1)
  (transpose-chars (- 1))
  (forward-char -1))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

(defun mark-next-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun kill-buffer-enhanced ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (progn
        (call-interactively 'save-buffer)
        (call-interactively 'bury-buffer))
    (progn
      (setq menu-updating-frame nil)
      (kill-buffer (current-buffer)))))

;; (defun mark-next-line ()
;;   "Mark next line continuously."
;;   (interactive)
;;   (if (use-region-p)
;;       (next-line nil)
;;     (if (eql (point-max) (line-end-position))
;;         (mark-lines-next-line nil)
;;       (mark-lines-previous-line nil)
;;       )))

;; (defun open-line-and-indent (n)
;;   (interactive "*p")
;;   (call-interactively 'open-line)
;;   (indent-according-to-mode)
;;   )

;; (global-set-key [(control o)] 'open-line-and-indent)

;; (defun move-dup-duplicate-up-then-comment ()
;;   (interactive)
;;   (move-dup-duplicate-up 1)
;;   (comment-region (line-beginning-position) (line-end-position))
;;   (forward-line 1))

(provide 'crux_init)

;;; crux_init.el ends here
