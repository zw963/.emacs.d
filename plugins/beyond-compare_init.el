;; ============================== Beyond Comapre 集成 ==============================

(require 'ansi-color)
;; beyond compare integration
(defun run-process (proc &rest arg)
  "run process and print output to minibuffer."
  (let ((proc (apply 'start-process "" nil proc arg)))
    (set-process-filter proc 'minibuffer-echo-filter))
  )
(defun minibuffer-echo-filter (proc string)
  "Echo process's output to minibuffer."
  (message (ansi-color-apply (replace-regexp-in-string "\r?\n\r?\\'" "" string))))

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

(provide 'beyond-compare_init)

;;; beyond-compare_init.el ends here
