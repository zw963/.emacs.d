(require 's)

(defun run-ruby-mode-hook (func)
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook `(lambda ()
                      ,func
                      ))))

(defun get-local-window-for-buffer-name (buffer-name-string)
  "Return the window displaying the specified buffer in the current frame.
Returns nil otherwise."
  (declare (side-effect-free error-free))
  (->> (window-list (selected-frame))
       (--first (->> it
                     (window-buffer)
                     (buffer-name)
                     (s-starts-with? buffer-name-string)))))

(defun get-local-buffer-for-buffer-name (buffer-name-string)
  "Return the specified buffer if exists.
Returns nil otherwise."
  (declare (side-effect-free error-free))
  (->> (buffer-list)
       (--first (->> it
                     (buffer-name)
                     (s-starts-with? buffer-name-string)))))

(defun count-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (let (
        (visible-buffer-count (length (mapcar #'window-buffer (window-list frame))))
        )
    (if (and (featurep 'treemacs) (eq (treemacs-current-visibility) 'visible))
        (- visible-buffer-count 1) visible-buffer-count)))

;; stolen from rinari
(defun self-defined-highlight-keywords (keywords &optional face)
  "Highlight the passed KEYWORDS in current buffer.
Use `font-lock-add-keywords' in case of `ruby-mode' or
`ruby-extra-keywords' in case of Enhanced Ruby Mode."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  (eval-when-compile (if (string-match "\\_>" "ruby")
                                         "\\_>"
                                       "\\>"))
                  )
          (list 2 (or face 'font-lock-builtin-face))))))

(require 'ansi-color)
(defun minibuffer-echo-filter (proc string)
  "Echo process's output to minibuffer."
  (message (ansi-color-apply (replace-regexp-in-string "\r?\n\r?\\'" "" string))))

(defun run-process (proc-name &rest arg)
  "run process and print output to minibuffer."
  (let ((proc (apply 'start-process "" nil proc-name arg)))
    (set-process-filter proc 'minibuffer-echo-filter))
  (print arg)
  )

(defun use-proxy (port)
  (interactive)
  (setenv "http_proxy" (concat "http://127.0.0.1:" port))
  (setenv "https_proxy" (concat "http://127.0.0.1:" port))
  (message (concat "set http_proxy https_proxy to http://127.0.0.1:" port))
  )

(defun add-list-to-list(target list)
  "Add LIST to TARGET."
  (set target (append list (eval target))))

;; 自动安装 package, e.g: (install 'imenu-anywhere)
(defun install (package)
  "Install a PACKAGE."
  (interactive)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(defun noop (&optional noop)
  "Do nothing, NOOP."
  (interactive))

(defun say (&optional noop)
  "Say"
  (interactive)
  (call-process "say" nil t))


;; (require 'sgml-mode)
;; (defun format-xml ()
;;   (interactive)
;;   (save-excursion
;;     (sgml-pretty-print (point-min) (point-max))
;;     (indent-region (point-min) (point-max))))

;; ============================== 快捷键相关 ==============================

;; (defun set-webpack-watch-dog ()
;;   (let ((watch-dog-file (getenv "WEBPACK_WATCH_DOG")))
;;     (print watch-dog-file)
;;     (when (and
;;            watch-dog-file
;;            (not (string= watch-dog-file (buffer-file-name)))
;;            (string-match "/app/views/" (buffer-file-name))
;;            )
;;       (shell-command-to-string (concat "add_webpack_watch_dog " watch-dog-file)))))
;; (add-hook 'before-save-hook 'set-webpack-watch-dog nil t)

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size (about 1M), make the buffer read only."
  (when (> (buffer-size) (* 1024 1024 10))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    ;; (ad-deactivate 'kill-ring-save)
    (fundamental-mode)
    ;; (when (boundp 'auto-indent-mode) (auto-indent-mode -1))
    ;; (when (boundp 'undo-tree-mode) (undo-tree-mode -1))
    ;; (when (boundp 'show-paren-mode) (show-paren-mode -1))
    ;; (when (boundp 'hl-line-mode) (hl-line-mode -1))
    ;; (when (boundp 'delete-selection-mode) (delete-selection-mode -1))
    ;; (when (boundp 'electric-indent-mode) (electric-indent-mode -1))
    ;; (when (boundp 'electric-pair-mode) (electric-pair-mode -1))
    ;; (when (boundp 'font-lock-mode) (font-lock-mode -1))
    ;; (when (boundp 'hi-lock-mode) (hi-lock-mode -1))
    ;; (when (boundp 'auto-complete-mode) (auto-complete-mode -1))
    ;; (when (boundp 'git-gutter-mode) (git-gutter-mode -1))
    ;; (when (boundp 'flycheck-mode) (flycheck-mode -1))
    ;; (when (boundp 'save-place-mode) (save-place-mode -1))
    ;; (when (boundp 'super-save-mode) (super-save-mode -1))
    ;; (when (boundp 'yafolding-mode) (yafolding-mode -1))
    ;; (when (boundp 'yas-minor-mode) (yas-minor-mode -1))
    ;; (when (boundp 'line-number-mode) (line-number-mode -1))
    ;; (when (boundp 'column-number-mode) (column-number-mode -1))
    ;; (when (boundp 'auto-revert-mode) (auto-revert-mode -1))
    ;; (when (boundp 'page-break-lines-mode) (page-break-lines-mode -1))
    ))
(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)
