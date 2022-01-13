(require 'shackle)

(setq shackle-default-alignment 'below)

;; 让 helm 的弹出窗口有类似于 popwin 的效果。
(setq helm-display-function 'pop-to-buffer) ; make helm play nice
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.618)))

(shackle-mode t)

;; (setq shackle-default-rule '(:select t))

;; (setq shackle-rules
;;       '(
;;         (("*Help*") :select t :size 0.3 :autoclose t)
;;         (helpful-mode :select t :size 0.3 :autoclose t)
;;         (hover-mode :noselect t :popup t :size 0.3 :autoclose t)
;;         (( "*Apropos*" "*Warnings*" "*Messages*") :noselect t :size 0.3 :autoclose t)
;;         ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :autoclose t)
;;         (compilation-mode :select t :size 0.3 :autoclose t)
;;         (emacs-lisp-compilation-mode :size 0.3 :autoclose t)
;;         ("*Completions*" :size 0.3 :autoclose t)
;;         ("*Backtrace*" :select t :size 15)
;;         (" *undo-tree*" :select t)
;;         ((" *Org todo*" "*Org Dashboard*" "*Org Select*") :select t :size 0.4 :autoclose t)
;;         (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :autoclose t)
;;         (" *Flycheck checkers*" :select t :size 0.3 :autoclose t)
;;         ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
;;          :select t :size 0.25 :autoclose t)
;;         (("*lsp-help*" "*lsp session*") :size 0.3 :autoclose t)
;;         ("*DAP Templates*" :select t :size 0.4 :autoclose t)
;;         (dap-server-log-mode :size 15 :autoclose t)
;;         ("*rustfmt*" :select t :size 0.3 :autoclose t)
;;         ("*quickrun*" :select t :size 15)
;;         ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3)
;;         ("^\\*vc-.*\\*$" :regexp t :size 0.3 :autoclose t)
;;         (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3)
;;         ))

;; (defvar shackle--popup-window-list nil) ; all popup windows
;; (defvar-local shackle--current-popup-window nil) ; current popup window
;; (put 'shackle--current-popup-window 'permanent-local t)

;; ;; Add keyword: `autoclose'
;; (defun shackle-display-buffer-hack (fn buffer alist plist)
;;   (let ((window (funcall fn buffer alist plist)))
;;     (setq shackle--current-popup-window window)

;;     (when (plist-get plist :autoclose)
;;       (push (cons window buffer) shackle--popup-window-list))
;;     window))

;; (defun shackle-close-popup-window-hack (&rest _)
;;   "Close current popup window via `C-g'."
;;   (setq shackle--popup-window-list
;;         (cl-loop for (window . buffer) in shackle--popup-window-list
;;                  if (and (window-live-p window)
;;                          (equal (window-buffer window) buffer))
;;                  collect (cons window buffer)))
;;   ;; `C-g' can deactivate region
;;   (when (and (called-interactively-p 'interactive)
;;              (not (region-active-p)))
;;     (let (window buffer)
;;       (if (one-window-p)
;;           (progn
;;             (setq window (selected-window))
;;             (when (equal (buffer-local-value 'shackle--current-popup-window
;;                                              (window-buffer window))
;;                          window)
;;               (winner-undo)))
;;         (setq window (caar shackle--popup-window-list))
;;         (setq buffer (cdar shackle--popup-window-list))
;;         (when (and (window-live-p window)
;;                    (equal (window-buffer window) buffer))
;;           (delete-window window)
;;           (pop shackle--popup-window-list))))))

;; (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
;; (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

(provide 'shackle_init)

;;; shackle_init.el ends here
