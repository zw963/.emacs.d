(require 'dired)
;; dired
(setq dired-recursive-copies 'always)            ;dired递归拷贝
(setq dired-recursiveq-deletes 'always)           ;dired递归删除
(setq dired-auto-revert-buffer t) ; 重新进入已经打开的 dired buffer 时, 刷新.
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; 尝试打开这个试试。
(setq dired-dwim-target t)

(setq dired-guess-shell-alist-user `((,(rx "."
                                           (or
                                            ;; Videos
                                            "mp4" "avi" "mkv" "flv" "ogv" "mov"
                                            ;; Music
                                            "wav" "mp3" "flac"
                                            ;; Images
                                            "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                            ;; Docs
                                            "pdf" "md" "djvu" "ps" "eps")
                                           string-end)
                                      ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                                             ((eq system-type 'darwin) "open")
                                             ((eq system-type 'windows-nt) "start")
                                             (
                                              t "")))))

(require 'dired-x) ;; 这个是内置
;; "\\`[.]?#\\|\\`[.][.]?\\'\\|~$\\|\\`.qr_[a-zA-Z0-9]{6}..*\\'"
(setq dired-omit-files (concat dired-omit-files "\\|~$\\|\\`\\.qr_[a-zA-Z0-9]\\{6\\}\\..*\\'"))

(require 'dired-aux)  ;; 这个是内置
(unless (version-list-<= (version-to-list emacs-version) '(24 3 1))
  (add-hook 'isearch-mode-hook 'dired-isearch-filenames-setup) ; isearch 仅仅搜索文件名。
  )
;; (define-key dired-mode-map [(f)] 'dired-isearch-filenames-regexp)
(define-key dired-mode-map [(control s)] 'dired-isearch-filenames)
(define-key dired-mode-map [(?\d)] 'dired-up-directory)
(define-key dired-mode-map [(backspace)] 'dired-up-directory)
(define-key dired-mode-map [(control d)] 'dired-do-delete)
(define-key dired-mode-map [(insert)] 'dired-cpa-current-file)

(setq global-auto-revert-non-file-buffers t) ;自动还原 ibuffer, dired.

(define-key dired-mode-map  "/" 'dired-filter-by-name)
(defun dired-filter-by-name(filter-regexp)
  (interactive "s(only show matched):")
  (let ((dired-marker-char 16)
        (files (directory-files default-directory t)))
    ;;(dired-unmark-all-files dired-marker-char)
    (save-excursion
      (dolist (file files)
        (when (and (dired-goto-file (expand-file-name file))
                   (not (string= "" filter-regexp))
                   (string-match filter-regexp (file-name-nondirectory file)))
          (dired-mark 1)
          )))
    (dired-toggle-marks)
    (dired-do-kill-lines nil (concat "Filter:'" filter-regexp "' omitted %d line%s"))
    (dired-move-to-filename)))

;; 这个是外部 package
(require 'dired-efap)                   ; rename
(define-key dired-mode-map [(shift f2)] 'dired-efap)
;;; Only if you want to control rename with the mouse...
(define-key dired-mode-map [down-mouse-1] 'dired-efap-click)

;; (require 'ripgrep-dired)
;; (setq ripgrep-dired-rg-basic-args "-nH --no-heading --smart-case -g '!*~' -g '!#*#' -g '!.#*'")

;; 所有 diredp- 作为前缀的名字，都来自于 dired+
(require 'dired+)
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode)
            (diredp-toggle-find-file-reuse-dir t)
            (dired-hide-details-mode -1)
            ;; 这行代码在 hook 里面是必须的, 因为 dired-efap 改写了 meta b 参数.
            (define-key dired-mode-map  [(meta b)] nil)
            (define-key dired-mode-map  [(control r)] 'rg-grep)
            (define-key dired-mode-map  [(control c) (+)] 'dired-create-empty-file)
            ;; (define-key dired-mode-map ")" 'dired-git-info-mode)
            ))

;; (require 'dired-k)
;; (setq dired-k-style 'git)
;; (define-key dired-mode-map (kbd "K") 'dired-k)
;; (add-hook 'dired-after-readin-hook #'dired-k-no-revert)

;; Enable additional font locking in ‘dired-mode’.
(require 'diredfl)
(diredfl-global-mode t)

;; (require 'fd-dired)

(require 'dired-quick-sort)
(dired-quick-sort-setup)

(require 'dired-narrow)
(setq dired-narrow-exit-when-1-left t)
(define-key dired-mode-map  "/" 'dired-narrow-fuzzy)
(define-key dired-mode-map  (kbd "<down-mouse-1>") 'dired-find-file)

(require 'diredc)
;; (global-set-key [(meta D)] (lambda () (interactive) (dired "./"))) ; 打开 dired buffer.
(global-set-key [(meta D)] 'diredc) ; 打开 dired buffer.
(global-set-key [remap dired-other-frame] 'diredc)

(require 'find-dupes-dired)

(require 'dired-preview)
(setq dired-preview-delay 0.7)
(setq dired-preview-max-size (expt 2 20))
(setq dired-preview-ignored-extensions-regexp
      (concat "\\."
              "\\(mkv\\|webm\\|mp4\\|mp3\\|ogg\\|m4a"
              "\\|gz\\|zst\\|tar\\|xz\\|rar\\|zip"
              "\\|iso\\|epub\\|pdf\\)"))
(dired-preview-global-mode 1)

;; 一些常用的命令:
;; t, dired-toggle-marks, 可以用来 mark 所有文件.
;; /, dired-narrow-fuzzy, 用来动态的 narrow 匹配的文件, g 会恢复.
;; 在标记的文件中搜索,  diredp-do-grep

(provide 'dired_init)
;;;  dired_init.el ends here
