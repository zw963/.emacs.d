;; -*- lexical-binding: t; -*-

(require 'mint-mode)

(defun format-all--locate-file (filename)
  "Internal helper to locate dominating copy of FILENAME for current buffer."
  (let* ((dir (and (buffer-file-name)
                   (locate-dominating-file (buffer-file-name) filename))))
    (when dir (expand-file-name (concat dir filename)))))

(defun mint-format-file ()
  "Formats current file using `mint format`."

  (let* ((file buffer-file-name)
         (error-file (make-temp-file "mint-format-errors-file"))
         (command (concat "mint format " file " > " error-file))
         (default-directory (file-name-directory (format-all--locate-file "mint.json")))

         ;; Error container
         (error-buffer (get-buffer-create "*prettier errors*"))

         ;; Revert options
         (ignore-auto t)
         (noconfirm t)
         (preserve-modes t)

         ;; Run command in process
         (result (call-process-shell-command command nil nil nil)) )

    ;; Check command result
    (if (or (zerop result) (eq 1 result))

        ;; Update formatted file and destroy error-buffer
        (progn
          (kill-buffer error-buffer)
          (revert-buffer ignore-auto noconfirm preserve-modes))

      ;; Show errors
      (progn
        (with-current-buffer error-buffer
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert-file-contents error-file t nil nil)
          (ansi-color-apply-on-region (point-min) (point-max))
          (compilation-mode))

        (display-buffer error-buffer)) )

    ;; Remove temporary error file
    (delete-file error-file) ))


(add-hook 'mint-mode-hook (lambda ()
                            (remove-hook 'after-save-hook #'mint-format-file t)
                            (setq-local js-indent-level 2)
                            (setq-local comment-start "/*")
                            (setq-local comment-end "*/")
                            ))

(require 'lsp-mint_init)

(provide 'mint-mode_init)

;;; mint-mode_init.el ends here
