;; -*- lexical-binding: t; -*-

(defun bc3-gd1-file-at-point ()
  "git gd1 current file with pointing tag, Use Beyond Compare 3."
  (interactive)
  (cond ((eq major-mode 'mo-git-blame-mode)
         (run-process "gd1"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))
                      ))
        ((eq major-mode 'git-log-view-mode)
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

  (cond ((eq major-mode 'git-branch-mode)
         (run-process "git-bcompare"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-log-view-mode)
         (run-process "git-bcompare"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-state-mode)
         (let ((fn (git--status-view-select-filename))
               (stat (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view)))))
           (if (equal stat 'staged)
               ;; INDEX 内将要提交的改变.
               (run-process "git-bcompare" "--cached" "--" fn)
             ;; 提交的改变.
             (run-process "git-bcompare" "--" fn))))
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

(provide 'beyond-compare-functions)

;;; beyond-compare-functions.el ends here
