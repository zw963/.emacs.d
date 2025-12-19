;; -*- lexical-binding: t; -*-

;; ============================================================================
;;
;; Yasnippets
;;
;; ============================================================================
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
(add-to-list 'warning-suppress-log-types '(yasnippet backquote-change))

(require 'yasnippet)
;; (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" yas--loaddir))
;; (setq yas-snippet-dirs (expand-file-name "snippets"))
(setq yas-snippet-dirs `(,(expand-file-name "snippets")))

(setq  yas-also-indent-empty-lines t
       yas-also-auto-indent-first-line t
       ;; yas-triggers-in-field nil         ; 这是默认值, 不允许递归扩展.
       )

;; (setq yas-buffer-local-condition '(not (yas-in-string-or-comment-p)))

;; (setq yas-buffer-local-condition
;;       '(if (yas-in-string-or-comment-p)
;;            '(require-snippet-condition . force-in-comment)
;;          '(not (require-snippet-condition . force-in-comment))))

(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

(defun test-yas/snippet ()
  "test yas/snippet pattern."
  (interactive)
  (yas-load-snippet-buffer (yas--read-table))
  (yas-tryout-snippet))

(add-hook 'snippet-mode-hook
          (lambda ()
            (whitespace-mode t)
            (local-set-key [(control meta g)] 'test-yas/snippet)
            ))

(defun yas-longest-nonwordkey (start-point)
  "As `yas-key-syntaxes' element, look for longest key made of nonword characters."
  (if (= (point) start-point)
      (skip-syntax-backward "^w")
    (forward-char))
  (unless (<= start-point (1+ (point)))
    'again))
;; 查看函数 modify-syntax-entry 函数获取语法表帮助.
(setq yas-key-syntaxes '(yas-longest-nonwordkey yas-try-key-from-whitespace "w_.()" "w_." "w_"))

(defun current-buffer-directory-name ()
  (file-name-nondirectory (string-remove-prefix "/$" default-directory)))

(defun rails-refactoring:camelize (name)
  "Translate file name into corresponding Ruby class name."
  (string-remove-prefix
   "_"
   (replace-regexp-in-string
    "/" "::"
    (replace-regexp-in-string
     "_\\([a-z]\\)" (lambda (match)
                      (upcase (substring match 1)))
     (capitalize name)))))

(defun elixir-refactoring:camelize (name)
  "Translate file name into corresponding Ruby class name."
  (string-remove-prefix
   "_"
   (replace-regexp-in-string
    "/" "."
    (replace-regexp-in-string
     "_\\([a-z]\\)" (lambda (match)
                      (upcase (substring match 1)))
     (capitalize name)))))

(defun current-git-file-path ()
  (interactive)
  (let ((git-folder (locate-dominating-file
                     default-directory ".git")))
    (when git-folder
      (string-remove-prefix
       (concat "^" (regexp-quote
                    (expand-file-name
                     git-folder)))
       (buffer-file-name)))))

(defun insert-comment (arg)
  "Insert a comment line, use current mode comment tag."
  (interactive "*P")
  (comment-normalize-vars)
  (if comment-insert-comment-function
      (funcall comment-insert-comment-function)
    (let ((add (comment-add arg)))
      (indent-according-to-mode)
      (insert (comment-padright comment-start add)))))

(defun expand-from-key-p ()
  "当使用 key 再按下 TAB 来激活的 snippet 时，this_command 是 yas-expand"
  (member (this-command-keys-vector) '([tab] [?\t])))

(defun expand-with-binding ()
  "当使用 binding: 激活一个 snippet 时，this command 是 yas-expand-from-keymap"
  (eq this-command 'yas-expand-from-keymap))

(defun yas-camelize (&optional string)
  "merge underscore-split word into a capitalize form."
  (string-remove-prefix "_\\|@\\|\\$" (capitalize (or string yas-text))))

(defun class-from-file ()
  "Return corresponding class/module name for given FILE."
  (let ((file (current-git-file-path)))
    (if file
        (let ((path (cl-find-if (lambda (path) (string-match (concat "^" (regexp-quote path)) file))
                                '("app/models/" "app/controllers/" "app/helpers/" "lib/"
                                  "test/controllers/" "test/models/" "test/helpers/" "test/jobs" "test/integration"
                                  "spec/models/" "spec/controllers/" "spec/helpers/ spec/lib/" "test/graphql"))))
          (if path
              (string-remove-prefix
               "^::"
               (cond
                ((member major-mode '(ruby-mode enh-ruby-mode ruby-ts-mode crystal-mode))
                 (rails-refactoring:camelize
                  (string-remove-prefix path (string-remove-prefix "\\(_spec\\)?\\.rb$" file))))

                ((member major-mode '(elixir-mode elixir-ts-mode))
                 (elixir-refactoring:camelize
                  (string-remove-prefix path (string-remove-prefix "\\(\\.ex\\|\\.exs\\)$" file))))
                )
               )
            (string-remove-prefix
             "^::"
             (rails-refactoring:camelize
              (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
             )))
      (string-remove-prefix
       "^::"
       (rails-refactoring:camelize
        (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
       ))))

;; 下面的代码因为 company 中的那个 hack, 已经不再需要了。
;; (with-eval-after-load 'company
;;   (defun ac/start () (interactive) (company-mode 1))
;;   (defun ac/close () (interactive) (company-mode -1))
;;   (add-hook 'yas-before-expand-snippet-hook 'ac/close)
;;   (add-hook 'yas-after-exit-snippet-hook 'ac/start)
;;   )

;; (define-key yas-keymap [(tab)] (yas-filtered-definition 'yas-next-field))
;; (define-key yas-keymap (kbd "TAB") (yas-filtered-definition 'yas-next-field))

;; TODO: 下面的代码, 是为了解决, 当使用 yas 展开之后, 在需要输入 $1 的部分,
;; 如果输入很快, ac 没有来得及退出 overlay, 会造成需要反复两次 TAB 才会跳到 $2
;; 可以再试一阵子, 因为感觉实际使用中, 不会输入那么快, 这不是大问题.

(with-eval-after-load 'auto-complete
  (defun ac/start () (interactive) (auto-complete-mode 1))
  (defun ac/close () (interactive) (auto-complete-mode -1))
  (add-hook 'yas-before-expand-snippet-hook 'ac/close)
  (add-hook 'yas-after-exit-snippet-hook 'ac/start)

  (defun ac-add-yas-source ()
    ;; 将 ac-source-yasnippets 加到最后面.
    ;; 不记得为什么要把 ac-source-yasnippets 放在最后,
    ;; 但是带来的新问题是: 本来应该 expand, 但是却匹配到其他 word.
    ;; 例如: 当我键入 times 时, ac 首选的提示是: timestamp,
    ;; times snippets 在最后, 无法直接被 expand.


    ;; 现在将 yas 和 ac 快捷键分开了. yas 使用 TAB, ac 使用 RET.
    ;; 将 yas 放在最后, 作为 ac-menu 弹出后的一个选择, 是不错的.

    (add-to-list 'ac-sources 'ac-source-yasnippet t)

    ;; (setq ac-sources (append ac-sources '(ac-source-yasnippet))
    )

  (run-ruby-mode-hook '(ac-add-yas-source))
  )

(defun shk-yas/helm-prompt (prompt choices &optional display-fn)
  "Use helm to select a snippet. Put this into `yas-prompt-functions.'"
  (interactive)
  (if (require 'helm nil t)
      (let ((result (helm-other-buffer
                     (list `((name . ,prompt)
                             (candidates . ,(if display-fn (mapcar display-fn choices)
                                              choices))
                             (action . (("Expand" . identity)))))
                     "*helm-select-yasnippet")))
        (cond ((null result)
               (signal 'quit "user quit!"))
              (display-fn
               (catch 'result
                 (dolist (choice choices)
                   (when (equal (funcall display-fn choice) result)
                     (throw 'result choice)))))
              (t result)))
    nil))

(defun yas/popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(shk-yas/helm-prompt yas/popup-isearch-prompt yas/no-prompt))

;; (require 'dropdown-list)
;; (setq yas-prompt-functions '(yas-x-prompt yas-dropdown-prompt))


;; (require 'yasnippet-multiple-key)
;; (yasnippet-multiple-key-compile-directory (expand-file-name "snippets"))

(require 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)

(yas-recompile-all)
(yas-global-mode t)

(require 'auto-yasnippet)

(provide 'yasnippet_init)
;;; yasnippet_init.el ends here
