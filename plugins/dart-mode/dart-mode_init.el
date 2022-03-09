;; (require 'project)

;; (defun project-try-dart (dir)
;;   (let ((project (or (locate-dominating-file dir "pubspec.yaml")
;;                      (locate-dominating-file dir "BUILD"))))
;;     (if project
;;         (cons 'dart project)
;;       (cons 'transient dir))))
;; (add-hook 'project-find-functions #'project-try-dart)
;; (cl-defmethod project-root ((project (head dart)))
;;   (list (cdr project)))

(require 'dart-mode)
(require 's)

(add-to-list 'auto-mode-alist '("\\.dart\\'" . dart-mode))

(defun dart-mode-common-hooks ()
  (when (featurep 'treemacs) (save-selected-window (treemacs-select-window)))
  )
(add-hook 'dart-mode-hook 'dart-mode-common-hooks)

;; 匹配dart 中可能出现的符号
(setq dart-symbols "[\s\t\na-zA-Z0-9():,{}'$;.=></_]*")
;; Flutter 之中，widget 调用匹配结束括号以及前面的逗号，需要被删除
;;     ,
;;   )
(setq flutter-widget-end ",\n\\s-+)")
(setq flutter-function-end ";\n\\s-+}")

(defun flutter-unwrap-layout-builder (&optional arg)
  (interactive)
  (let ((start-point (s-lex-format "LayoutBuilder(${dart-symbols}return "))
        (end-point (s-lex-format ";\n\\s-+}${flutter-widget-end}")))
    (let ((begin-pos (save-excursion (search-backward-regexp start-point nil t)))
          (end-pos (save-excursion (search-forward-regexp end-point nil t 1))))
      (cond ((and begin-pos end-pos)
             (save-excursion
               (search-backward-regexp start-point nil t)
               (forward-sexp 2)
               (when (re-search-backward end-point nil t 1)
                 (replace-match "")))
             (save-excursion (replace-regexp start-point "" nil begin-pos end-pos)))
            (t (message "Not in Layoutbuilder"))))))

(defun flutter-unwrap-container (&optional arg)
  (interactive)
  (let ((start-point (s-lex-format "Container(${dart-symbols}child: "))
        (end-point flutter-widget-end))
    (let ((begin-pos (save-excursion (search-backward-regexp start-point nil t)))
          ;; 这个结果不准，很有可能是某个中间的其他 widget 的结尾。
          ;; (end-pos (save-excursion (search-forward-regexp end-point nil t 1)))
          )
      (cond (begin-pos
             (save-excursion
               (search-backward-regexp start-point nil t)
               (forward-sexp 2)
               (when (re-search-backward end-point nil t 1)
                 (replace-match "")))
             (save-excursion (replace-regexp start-point "" nil begin-pos (point))))
            (t (message "Not in Container"))))))

;; (setq flutter-widgets (regexp-opt '
;;  (
;;   "Align"
;;   "Center" "CircleAvatar" "ConstrainedBox" "Container"
;;   "ElevatedButton" "Expanded"
;;   "FittedBox" "Flexible" "Fractionalsizedbox"
;;   "GestureDetector"
;;   "ListView" "ListView.builder" "Listview.separated"
;;   "Padding" "Positioned"
;;   "RefreshIndicator" "RaisedButton"
;;   "SafeArea" "Scaffold" "SizedBox" "Stack" "SliverToBoxAdapter"
;;   "TextButton" "TextSpan"
;;   "Visibility"
;;   "Wrap"
;;   )))

(setq flutter-widget-pattern " [A-Z][A-Za-z]+")
(setq flutter-args-pattern "[A-Za-z0-9_,? ]*")

(defun flutter-unwrap-function-body (&optional arg)
  (interactive)
  (let ((handle (prepare-change-group)))
    (unwind-protect
        (progn
          (activate-change-group handle)
          (let ((start-point (s-lex-format "\\((${flutter-args-pattern})\\(:? async\\)?\\) {\\(:?\n\\s-+return\\)?")
                             )
                (end-point flutter-function-end)
                (current-point (point))
                )
            (let ((begin-pos (save-excursion (search-backward-regexp start-point nil t 1))))
              (let ((args (match-string 1)))
                (cond (begin-pos
                       (save-excursion
                         (goto-char begin-pos)
                         (if (string-match "async" args)
                             (forward-sexp 3)
                           (forward-sexp 2))
                         (when (re-search-backward end-point begin-pos t 1)
                           (replace-match "")))
                       (save-excursion (replace-regexp start-point (s-lex-format "${args} =>") nil begin-pos current-point t)))
                      (t (message "Not in function"))))))
          (undo-amalgamate-change-group handle)))))

(defun flutter-unwrap-widget (&optional arg)
  (interactive)
  (let ((handle (prepare-change-group)))
    (unwind-protect
        (progn
          (activate-change-group handle)
          (let ((start-point (s-lex-format "${flutter-widget-pattern}(${dart-symbols}\\(child\\|body\\):"))
                (end-point flutter-widget-end)
                (current-point (point))
                )
            (let ((begin-pos (save-excursion (search-backward-regexp start-point nil t 1)))
                  )
              (cond (begin-pos
                     (save-excursion
                       (goto-char begin-pos)
                       (forward-sexp 2)
                       (when (re-search-backward end-point begin-pos t 1)
                         (replace-match "")))
                     (save-excursion (replace-regexp start-point "" nil begin-pos current-point t)))
                    (t (message "Not in Widget")))))
          (undo-amalgamate-change-group handle)))))

(defun flutter-toggle-container/sizedbox (&optional arg)
  (interactive)
  (let (
        (start-point (s-lex-format "\\(?1:Container\\|SizedBox\\)(${dart-symbols}child: "))
        (end-point flutter-widget-end)
        )
    (let ((end-pos (save-excursion (search-forward-regexp end-point nil t 1)))
          (begin-pos (save-excursion (search-backward-regexp start-point nil t))))
      (cond ((and begin-pos end-pos)
             (if (string= (match-string 1) "SizedBox")
                 (save-excursion (replace-regexp "SizedBox(" "Container(color: Colors.grey," nil begin-pos end-pos))
               (save-excursion (replace-regexp "Container(\n\\s-+\\(color: Colors\.[^,]+,\\)?" "SizedBox(" nil begin-pos end-pos))))
            (t (message "Not in Container or SizedBox"))))))

(define-key dart-mode-map [(meta c) (c)] 'flutter-unwrap-widget)
(define-key dart-mode-map [(meta c) (s)] 'flutter-toggle-container/sizedbox)
(define-key dart-mode-map [(meta c) (l)] 'flutter-unwrap-layout-builder)

(require 'lsp-dart_init)
;; (require 'dart-mode-eglot_init)
(require 'dart-mode-context-menu_init)

;; (require 'flutter)
;; (setq flutter-l10n-arb-dir "lib/i10n")
;; (setq flutter-l10n-template-arb-file "intl_zh_Hans.arb")
;; (setq flutter-l10n-output-localization-file "l10n.dart")

(defun use-charles-proxy ()
  (interactive)
  (use-proxy "8888")
  )

;; (add-hook 'dart-mode-hook 'flutter-test-mode)
;; (define-key dart-mode-map (kbd "C-M-x") 'flutter-run-or-hot-reload)

;; (require 'hover)

(with-eval-after-load 'hover
  (define-key hover-minor-mode-map (kbd "C-M-x") 'hover-run-or-hot-reload)
  (define-key hover-minor-mode-map (kbd "C-M-z") 'hover-run-or-hot-restart)
  (setq
   hover-screenshot-path (concat (getenv "HOME") "/Pictures")
   hover-screenshot-prefix "magpie-"
   hover-observatory-uri "http://127.0.0.1:50300"
   hover-clear-buffer-on-hot-restart t
   hover-hot-reload-on-save t
   )
  (hover-global-minor-mode t)
  )

(provide 'dart-mode_init)

;;; dart-mode_init.el ends here
