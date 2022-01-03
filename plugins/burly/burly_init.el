(require 'bookmark)
(require 'burly)

(defun burly-bookmark-names ()
  "Return list of all Burly bookmark names."
  (list-bookmarks)
  (cl-loop for bookmark in bookmark-alist
           for (_name . params) = bookmark
           when (equal #'burly-bookmark-handler (alist-get 'handler params))
           collect (car bookmark)))

;; (defun set-burly-bookmark (frame)
;;   (call-interactively 'burly-bookmark-windows)
;;   t
;;   )
;; (add-hook 'delete-frame-functions 'set-burly-bookmark)

(add-hook 'kill-emacs-hook '
          (lambda ()
            (when (> (length (frame-list)) 1)
              (burly-bookmark-windows "Burly: default")
              )
            ))

(global-set-key [(f6)] 'burly-open-bookmark)

(defun burly-open-last-auto-saved-bookmark (&optional frame)
  (interactive)
  (burly-bookmark-names)
  (bookmark-jump "Burly: default")
  )

(defun burly-open-last-auto-saved-bookmark-delay (&optional frame)
  (unless (> (length (frame-list)) 1)
              (run-with-idle-timer 0.5 nil 'burly-open-last-auto-saved-bookmark)))

(add-hook 'window-setup-hook 'burly-open-last-auto-saved-bookmark)
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions 'burly-open-last-auto-saved-bookmark-delay t)
  )

(provide 'burly_init)

;;; burly_init.el ends here
