;; -*- lexical-binding: t; -*-

;; ============================== Beyond Comapre 集成 ==============================

(global-set-key [(control f11)] 'bc1-current-region)
(global-set-key [(meta f11)] 'bc2-current-region)

(global-set-key [(control f12)] 'bc1-current-file)
(global-set-key [(meta f12)] 'bc2-current-file)

(require 'dired)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map [(control f12)] 'dired-bc1-current-file)
            (define-key dired-mode-map [(meta f12)] 'dired-bc2-current-file)
            ))

(require 'ibuffer)
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (define-key ibuffer-mode-map [(control f12)] 'ibuffer-bc1-current-file)
            (define-key ibuffer-mode-map [(meta f12)] 'ibuffer-bc2-current-file)
            ))

(require 'vc-annotate)
(add-hook 'vc-annotate-mode-hook
          (lambda ()
            (define-key vc-annotate-mode-map [(n)] 'next-line)
            (define-key vc-annotate-mode-map [(p)] 'previous-line)
            (define-key vc-annotate-mode-map [(meta g) (meta g)] 'vc-annotate-goto-line)
            (define-key vc-annotate-mode-map [(d)] 'bc3-gd1-file-at-point)
            ))

(require 'vc-git)
(add-hook 'vc-git-log-view-mode-hook
          (lambda ()
            (define-key vc-git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
            (define-key vc-git-log-view-mode-map [(d)] 'bc3-gd1-file-at-point) ;; 单个 commit 做了那些修改, 只看当前文件
            (define-key vc-git-log-view-mode-map [(q)] 'kill-buffer-and-window)
            ))

(with-eval-after-load 'git-emacs
  (global-set-key [(control x) (v) (=)] 'bc3-gitdiff) ;; 当前 diff 修改.
  (global-set-key [(control x) (v) (h)] 'bc3-gitdiff-head) ;; diff + index 修改
  )

;; (add-hook 'git-log-view-mode-hook
;;           (lambda ()
;;             (define-key git-log-view-mode-map [(=)] 'bc3-gitdiff-file-at-point) ;; 当前文件与 commit 之间的修改.
;;             (define-key git-log-view-mode-map [(d)] 'bc3-gd1-file-at-point) ;; 单个 commit 做了那些修改, 只看当前文件
;;             (define-key git-log-view-mode-map [(q)] 'kill-buffer-and-window)
;;             ))

(provide 'beyond-compare_init)

;;; beyond-compare_init.el ends here

(defun bc3-gd1-file-at-point ()
  "git gd1 current file with pointing tag, Use Beyond Compare 3."
  (interactive)
  (cond ((eq major-mode 'vc-git-log-view-mode)
         (run-process "gd1"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'vc-annotate-mode)
         (run-process "gd1"
                      (car (vc-annotate-extract-revision-at-line))
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ))

(defun bc3-gitdiff-file-at-point ()
  "Git diff current file with pointing tag, Use Beyond Compare 3."
  (interactive)
  (cond ((eq major-mode 'my/git-branches-mode)
         (run-process "git-bcompare"
                      (my/git-branches-branch-name)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'vc-git-log-view-mode)
         (run-process "git-bcompare"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-status-mode)
         (let ((fn (git--status-view-select-filename))
               (stat (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view)))))
           (if (equal stat 'staged)
               ;; INDEX 内将要提交的改变.
               ;; (run-process "git-bcompare" "--cached" "--" fn)
               (print (concat "git-bcompare" "--cached" "--" fn))
             ;; 提交的改变.
             ;;             (run-process "git-bcompare" "--" fn)
             (print (concat "git-bcompare" "--" fn))
             )))
        ))

(defun bc3-gitdiff ()
  "Git diff current file content with INDEX, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "--" (file-relative-name buffer-file-name)))

(defun bc3-gitdiff-head ()
  "Git diff current buffer with HEAD, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "HEAD" "--" buffer-file-name))

(defun bc1-current-file ()
  "Setting Beyond Compare first compared file."
  (interactive)
  (run-process "bc1" buffer-file-name))

(defun bc2-current-file ()
  "Setting Beyond Compare second compared file."
  (interactive)
  (run-process "bc2" buffer-file-name))

(defun bc3-autosave-file ()
  "Use Beyond Compare 3 compare with auto-save files, right is newer"
  (interactive)
  (run-process "bcompare" (make-auto-save-file-name) buffer-file-name))

(defun bc3-buffer-with-file ()
  "Use Beyond Compare 3 compare buffer with file, right is newer"
  (interactive)
  (let ((tempfile (make-temp-file "buffer-content-")))
    (write-region nil nil tempfile nil 'nomessage)
    (run-process "bcompare"
                 buffer-file-name
                 tempfile
                 "-lefttitle=File content（Old）"
                 "-righttitle=Buffer content（New）"
                 )))

(defun dired-bc1-current-file ()
  "Setting Beyond Compare first compared file in dired."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "bc1" fn)))

(defun dired-bc2-current-file ()
  "Setting Beyond Compare second compared file in dired."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "bc2" fn)))

(defun dired-cpa-current-file ()
  "run cpa at current file in dired buffer."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "cpa" fn)))

(defun ibuffer-bc1-current-file ()
  "Setting Beyond Compare first compared file in ibuffer."
  (interactive)
  (let ((fn (buffer-file-name (ibuffer-current-buffer t))))
    (run-process "bc1" fn)))

(defun ibuffer-bc2-current-file ()
  "Setting Beyond Compare second compared file in ibuffer."
  (interactive)
  (let ((fn (buffer-file-name (ibuffer-current-buffer t))))
    (run-process "bc2" fn)))

(defun region-to-file-force (file)
  "Prints string into file, matters not if file exists."
  (write-region (region-beginning) (region-end) file nil nil nil nil))

(defun bc1-current-region ()
  (interactive)
  (region-to-file-force "/tmp/bc3_one")
  (run-process "bc1" "/tmp/bc3_one"))

(defun bc2-current-region ()
  (interactive)
  (region-to-file-force "/tmp/bc3_two")
  (run-process "bc2" "/tmp/bc3_two"))

(defun treemacs-bc1-file (&optional arg)
  (interactive "P")
  (run-process "bc1" (treemacs--select-file-from-btn (treemacs-current-button) "bc1: ")))

(defun treemacs-bc2-file (&optional arg)
  (interactive "P")
  (run-process "bc2" (treemacs--select-file-from-btn (treemacs-current-button) "bc2: ")))
