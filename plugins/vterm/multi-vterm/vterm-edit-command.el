;; Stolen from https://gitee.com/blindingdark/BEmacs/raw/master/vendor/vterm-edit-command.el

(require 'subr-x)

(defun vterm-edit-command-action ()
  (interactive)
  (let* ((vterm-buffer (current-buffer))
         (begin-point (vterm--get-prompt-point))
         (end-point (point)))
    (setq vterm-edit-command--vterm-buffer vterm-buffer)
    (setq vterm-edit-command--begin-point begin-point)
    (setq vterm-edit-command--end-point end-point)
    (kill-ring-save begin-point end-point)
    (vterm-edit-command-buffer)))

(defun vterm-edit-command-commit ()
  (interactive)
  (let ((content (buffer-string)))
    (with-current-buffer vterm-edit-command--vterm-buffer
      (vterm-delete-region vterm-edit-command--begin-point vterm-edit-command--end-point)
      (vterm-send-string (string-trim content))))
  (vterm-edit-command-abort))

(defun vterm-edit-command-abort ()
  (interactive)
  (kill-buffer-and-window))

(defvar vterm-edit-command-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c C-c") #'vterm-edit-command-commit)
    (define-key keymap (kbd "C-c C-k") #'vterm-edit-command-abort)
    keymap))

(define-minor-mode vterm-edit-command-mode
  "Vterm Edit Command Mode")

(defun vterm-edit-command-buffer ()
  (let ((buffer (get-buffer-create "vterm-edit-command")))
    (with-current-buffer buffer
      (insert-buffer-substring
       vterm-edit-command--vterm-buffer
       vterm-edit-command--begin-point
       vterm-edit-command--end-point)
      (vterm--remove-fake-newlines)
      (set-text-properties (point-min) (point-max) nil)
      (goto-char (point-max))
      (insert "\n")
      (sh-mode)
      (vterm-edit-command-mode)
      (setq-local header-line-format
                  (substitute-command-keys
                   (concat "Edit, then "
                           (mapconcat
                            'identity
                            (list "\\[vterm-edit-command-commit]: Finish"
                                  "\\[vterm-edit-command-abort]: Abort"
                                  )
                            ", "))))
      (split-window-sensibly)
      (switch-to-buffer-other-window buffer))))

(provide 'vterm-edit-command)
;;; vterm-edit-command.el ends here
