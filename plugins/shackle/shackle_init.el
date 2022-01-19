(require 'shackle)

;; ;; 让 helm 的弹出窗口有类似于 popwin 的效果。
;; (setq helm-display-function 'pop-to-buffer) ; make helm play nice
;; (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.618)))

(setq shackle-default-size 0.4
      shackle-default-alignment 'below
      shackle-default-rule nil
      shackle-select-reused-windows t
      shackle-rules
      '(
        (("*Help*" "*Apropos*") :select t :size 0.3 :autoclose t)
        (helpful-mode :select t :size 0.5 :popup t :autoclose t)
        (compilation-mode :select t :size 0.3 :autoclose t)
        (comint-mode :select t :size 0.4 :autoclose t)
        ("*Completions*" :size 0.3 :autoclose t)
        ("*Pp Eval Output*" :size 15 :autoclose t)
        ("*Backtrace*" :select t :size 15 )
        (("*Warnings*" "*Messages*") :size 0.3 :autoclose t)
        ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :autoclose t)
        ("\\*[Wo]*Man.*\\*" :regexp t :select t :autoclose t)
        ("*Calendar*" :select t :size 0.3 )
        (("*shell*" "*eshell*" "*ielm*") :popup t :size 0.3 )
        ("^\\*vc-.*\\*$" :regexp t :size 0.3 :autoclose t)
        ("*gud-debug*" :select t :size 0.4 :autoclose t)
        ("\\*ivy-occur .*\\*" :regexp t :select t :size 0.3 )
        (" *undo-tree*" :select t)
        ("*quickrun*" :select t :size 15 )
        ("*tldr*" :size 0.4 :autoclose t)
        ("*Finder*" :select t :size 0.3 :autoclose t)
        ("^\\*macro expansion\\**" :regexp t :size 0.4 )
        ("^\\*elfeed-entry" :regexp t :size 0.7 :autoclose t)
        (" *Install vterm* " :size 0.35 :same t )
        (("*Paradox Report*" "*package update results*") :size 0.2 :autoclose t)
        ("*Package-Lint*" :size 0.4 :autoclose t)
        ("*How Do You*" :select t :size 0.5 :autoclose t)

        ((youdao-dictionary-mode osx-dictionary-mode fanyi-mode) :select t :size 0.5 :autoclose t)

        (("*Org Agenda*" " *Agenda Commands*" " *Org todo*" "*Org Dashboard*" "*Org Select*")
         :select t :size 0.1 :autoclose t)
        (("\\*Capture\\*" "^CAPTURE-.*\\.org*") :regexp t :select t :size 0.3 :autoclose t)

        ("*ert*" :size 15 :autoclose t)
        (overseer-buffer-mode :size 15 :autoclose t)

        (" *Flycheck checkers*" :select t :size 0.3 :autoclose t)
        ((flycheck-error-list-mode flymake-diagnostics-buffer-mode)
         :select t :size 0.25 :autoclose t)

        (("*lsp-help*" "*lsp session*" "*LSP Error List*") :size 0.3 :autoclose t)
        ("*DAP Templates*" :select t :size 0.4 :autoclose t)
        (dap-server-log-mode :size 15 :autoclose t)
        ("*rustfmt*" :select t :size 0.3 :autoclose t)
        ((rustic-compilation-mode rustic-cargo-clippy-mode rustic-cargo-outdated-mode rustic-cargo-test-mode)
         :select t :size 0.3 :autoclose t)

        (profiler-report-mode :select t :size 0.5 )
        ("*ELP Profiling Restuls*" :select t :size 0.5 )

        ((inferior-python-mode inf-ruby-mode swift-repl-mode) :size 0.4 )
        ("*prolog*" :size 0.4 )

        (("*Gofmt Errors*" "*Go Test*") :select t :size 0.3 :autoclose t)
        (godoc-mode :select t :size 0.4 :autoclose t)

        ((grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode) :select t :size 0.4 )
        (Buffer-menu-mode :select t :size 0.5 :autoclose t)
        (gnus-article-mode :select t :size 0.7 :autoclose t)
        (devdocs-mode :select t :size 0.4 :autoclose t)
        ((process-menu-mode list-environment-mode cargo-process-mode) :select t :size 0.3 )
        (("*docker-containers*" "*docker-images*" "*docker-networks*" "*docker-volumes*")
         :size 0.4 :autoclose t)
        (bookmark-bmenu-mode :select t :size 0.4 )
        (tabulated-list-mode :size 0.4 :autclose t)
        (hover-mode :select t :size 0.2 :autoclose t)
        (vterm-mode :select t :size 0.7 :align 'top)
        (vc-annotate-mode :select t :size 0.8 :autoclose t)
        ))

;; Stolen from https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-window.el
(with-no-warnings
  (defvar shackle--popup-window-list nil
    "All popup windows.")
  (defvar-local shackle--current-popup-window nil
    "Current popup window.")
  (put 'shackle--current-popup-window 'permanent-local t)

  (defun shackle-last-popup-buffer ()
    "View last popup buffer."
    (interactive)
    (ignore-errors
      (display-buffer shackle-last-buffer)))

  ;; Add keyword: `autoclose'
  (defun shackle-display-buffer-hack (fn buffer alist plist)
    (let ((window (funcall fn buffer alist plist)))
      (setq shackle--current-popup-window window)

      (when (plist-get plist :autoclose)
        (push (cons window buffer) shackle--popup-window-list))
      window))
  (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack)

  (defun shackle-close-popup-window-hack (&rest _)
    "Close current popup window via `C-g'."
    (setq shackle--popup-window-list
          (cl-loop for (window . buffer) in shackle--popup-window-list
                   if (and (window-live-p window)
                           (equal (window-buffer window) buffer))
                   collect (cons window buffer)))
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p)))
      (if (one-window-p)
          (let ((window (selected-window)))
            (when (equal (buffer-local-value 'shackle--current-popup-window
                                             (window-buffer window))
                         window)
              (winner-undo)))
        (let* ((window (caar shackle--popup-window-list))
               (buffer (cdar shackle--popup-window-list))
               (process (get-buffer-process buffer)))
          (when (and (window-live-p window)
                     (equal (window-buffer window) buffer))
            ;; (when (process-live-p process)
            ;;   (kill-process process))
            (delete-window window)
            (pop shackle--popup-window-list))))))
  (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack))

(shackle-mode t)

(provide 'shackle_init)

;;; shackle_init.el ends here
