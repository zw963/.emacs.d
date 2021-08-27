;; -*-Emacs-Lisp-*-

;; 目前已知的稳定版函数有：
;; (yas-field "1" "arg" " \"" "\"")
;; (yas-key-field "1" "arg" "|" "|")zw
;; (yas-if-any "1" "hello" "world")  "${1:$(if-any "hello" "world")}"
;; ------------------------------ expansion functions ------------------------------

(defun strip-string (string)
  "strip backslash and double-quote, used by snippets Embedded format."
  (if string
      (replace-regexp-in-string
       "\"" "\\\\\\\\\\\\\""
       (replace-regexp-in-string
        "(" "\\\\("
        (replace-regexp-in-string
         ")" "\\\\)"
         (replace-regexp-in-string
          "\\\\" "\\\\\\\\\\\\\\\\"
          string t nil))))))

(defun stripped-field ()
  (if yas-selected-text
      (strip-string yas-selected-text)))

(defun yas-stripped-selected-text (&optional before after expression-delimiter)
  "strip leading whitespace, first and last blank lines,
reinsert one blank line if region is multi-line."
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          (let ((text (replace-regexp-in-string
                       ".*\\(\n\\)\\'" ""
                       (replace-regexp-in-string
                        "\\`\\(\n\\).*" ""
                        yas-selected-text nil nil 1) nil nil 1)))
            (concat
             (or before (yas-newline))
             (replace-regexp-in-string "\\`\\([ \t]+\\).*" "" text nil nil 1)
             (or after (yas-newline))))
        (concat before yas-selected-text after (or expression-delimiter (expression-delimiter))))
    ;; (concat before yas-selected-text after)
    ))

(defun insert-comment (arg)
  "Insert a comment line, use current mode comment tag."
  (interactive "*P")
  (comment-normalize-vars)
  (if comment-insert-comment-function
      (funcall comment-insert-comment-function)
    (let ((add (comment-add arg)))
      (indent-according-to-mode)
      (insert (comment-padright comment-start add)))))

(defun def (&optional def-begin def-end single-line-end-seperator)
  (insert
   (concat
    (def-begin def-begin)
    "`(yas-stripped-selected-text)`$0"
    (def-end def-end single-line-end-seperator)
    )))

(defun end (&optional end)
  "Insert end."
  (let ((end (or end (yas-def-end))))
    (if yas-selected-text
        (if (string-match "\n\\'" yas-selected-text)
            (concat end "\n")
          end)
      end)))

(defun yas-def-begin ()
  "定义一个函数时，起始分隔符。
例如：
单行 Ruby def 以 ; 开头，def meth; puts 100 end
js 以 { 开头, function(done) { ... }
"
  (cond
   ((member major-mode '(ruby-mode enh-ruby-mode))
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text) "" ";")
      "")
    )
   ((member major-mode '(elixir-mode)) " do")
   ((member major-mode '(sh-mode)) "")
   ((member major-mode '(js2-mode cc-mode rust-mode)) " {"))
  )

(defun yas-def-end ()
  "定义一个函数时，结尾分隔符."
  (cond
   ((member major-mode '(ruby-mode elixir-mode enh-ruby-mode)) "end")
   ((eql major-mode 'sh-mode) "done")
   ((member major-mode '(js2-mode cc-mode rust-mode)) "}")
   ((member major-mode '(rhtml-mode web-mode)) "<% end %>")
   ))

;; 为了做到关注点分离, 有时候, 存在公共的重复逻辑是不可避免的.
;; 例如: yas-def-begin 与 def-begin 中有关 yas-selected-text 的判断.
(defun def-begin (&optional def-begin)
  (let ((begin (or def-begin (yas-def-begin))))
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text)
            begin
          (concat begin " "))
      (concat begin "\n"))))

(defun def-end (&optional def-end single-line-end-seperator)
  (let ((end (or def-end (yas-def-end))))
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text)
            (if (string-match "\n\\'" yas-selected-text)
                (concat end "\n")
              end)
          (concat single-line-end-seperator " " end))
      (concat "\n" end))))

(defun expression-delimiter ()
  ( unless (or
            (fourth (syntax-ppss))
            (fifth (syntax-ppss))
            (member major-mode '(emacs-lisp-mode snippet-mode rhtml-mode web-mode))
            )
    ""))

(defun expand-from-key-p ()
  "当使用 eky 再按下 TAB 来激活的 snippet 时，this_command 是 yas-expand"
  (equal (this-command-keys-vector) '[tab]))

(defun expand-with-binding ()
  "当使用 binding: 激活一个 snippet 时，this command 是 yas-expand-from-keymap"
  (eq this-command 'yas-expand-from-keymap))

(defun yas-if-any (field-index &optional arg1 arg2)
  "用来拼装字符串 ${1:$(if-any \"hello\" \"world\")}"
  (insert (concat
           "${"
           field-index
           ":$("
           "if-any"
           (when arg1 (concat " \"" (strip-string arg1) "\""))
           (when arg2 (concat " \"" (strip-string arg2) "\""))
           ")}"
           )))

(defun yas-field-func (field-index &optional arg1 arg2 func)
  "用来拼装字符串 ${1:$(if-any \"hello\" \"world\")}, 函数名也可以改。"
  (concat
   "${"
   field-index
   ":$("
   (or func "if-any")
   (when arg1 (concat " \"" (strip-string arg1) "\""))
   (when arg2 (concat " \"" (strip-string arg2) "\""))
   ")}"
   ))

(defun yas-expand-func (&optional arg1 arg2 func)
  "用来拼装 `(if-any \"hello\" \"world\")`"
  (concat
   "`("
   (or func "if-any")
   (when arg1 (concat " \"" (replace-regexp-in-string "\"" "\\\\\"" arg1) "\""))
   (when arg2 (concat " \"" (replace-regexp-in-string "\"" "\\\\\"" arg2) "\""))
   ")`"
   ))

(defun if-any (&optional string1 string2)
  "Add STRING1 if `yas-text' content is not empty, otherwise STRING2."
  (if (string-match "^$" (or yas-text yas-selected-text))
      string2
    (or string1 "")
    )
  ;; (if yas-modified-p
  ;;     ;; 不可以为空, 或空格.
  ;;     ;; 当按下 C-d 时，yas-text 是一个 "".
  ;;     (if (string-match "^$" (or yas-text yas-selected-text))
  ;;         string2
  ;;         (or string1 "")
  ;;       )
  ;;   ;; 仅不可以为空格.
  ;;   (if (string-match "^\s" (or yas-text yas-selected-text))
  ;;       string2
  ;;     (or string1 ""))
  ;;   )
  )

(defun if-key (string1 &optional string2)
  "Add STRING1 if expand with a key, only be use in `...`."
  (if (expand-from-key-p)
      (insert string1)
    (when string2 (insert string2))))

(defun if-binding (string1 &optional string2)
  "Add STRING1 if  expand with a binding, only be use in `...`."
  (if (expand-with-binding)
      (insert string1)
    (when string2 (insert string2))))

(defun block-begin (&optional field-index field-content no-space)
  "Inset ruby block begin."
  (insert
   (if field-index
       (concat
        (block-do)
        (yas-field-func field-index (block-left-border) " ")
        (yas-field-string-content field-index field-content)
        (yas-field-func field-index "| "))
     (concat (block-do) (or no-space " ")))
   ))

(defun block-do (&optional do)
  (or do
      (if yas-selected-text
          (if (string-match "\n" yas-selected-text)
              "do"
            "{")
        "{")))

(defun block-end (&optional end-newline)
  "Insert ruby block end."
  (insert
   (if yas-selected-text
       (if (string-match "\n" yas-selected-text)
           (if (string-match "\n\\'" yas-selected-text)
               (concat "end" (or end-newline "\n"))
             "end")
         " }")
     " }")
   ))

(defun block-left-border ()
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          " |"
        "|")
    "|"))

(defun ruby-block (&optional field-index field-content no-space)
  "Inset ruby do/end block."
  ;; 如果是两个参数.
  (block-begin field-index field-content no-space)
  (insert "`(yas-stripped-selected-text)``(if-key \"$0\")`")
  (block-end)
  )

(defun ruby-module-name-field (field-index)
  "Insert ruby module name field."
  (field
   field-index
   (class-from-file)
   ""))

(defun yas-newline ()
  "Insert a `newline'."
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          "\n"
        (yas-newline-seperator))
    ""))

(defun yas-field-seperator ()
  "Insert field delimiter."
  (if (member major-mode '(
                           emacs-lisp-mode
                           snippet-mode
                           sh-mode
                           ;; rhtml-mode
                           ;; html-mode
                           ))
      " "
    ""))

(defun yas-field-string-content (field-index &optional content)
  (if content
      (concat
       "${"
       field-index
       ":"
       (strip-string content)
       "}"
       )
    (concat "$" field-index)))

(defun key-field (field-index
                  field-content
                  &optional
                  before-field-delimiter
                  after-field-delimiter
                  field-seperator
                  field-seperator-function
                  before-field-delimiter-function
                  after-field-delimiter-function
                  not-exist-after-field-delimiter)
  "Insert a key field."
  (if (and (expand-with-binding) (region-active-p))
      (insert (concat
               (or field-seperator (yas-field-seperator))
               (yas-expand-func before-field-delimiter nil before-field-delimiter-function)
               (yas-stripped-selected-text "" "" "")
               (unless not-exist-after-field-delimiter
                 (yas-expand-func after-field-delimiter nil after-field-delimiter-function))
               ))
    (field field-index field-content before-field-delimiter after-field-delimiter field-seperator
           field-seperator-function after-field-delimiter-function before-field-delimiter-function)))

(defun yas-field (field-index
                  field-content
                  &optional
                  before-field-delimiter
                  after-field-delimiter
                  before-field-delimiter-function
                  after-field-delimiter-function
                  not-exist-after-field-delimiter)
  "field-index: 字段的索引
field-content: 字段的内容
before-field-delimiter: 字段之前的标识符，例如： 开始双引号
after-field-delimiter: 字段之后的标识符。例如： 结束双引号
"
  (insert
   (concat
    ;; 字段之前的分隔符，以及字段之前分隔符使用的函数， 默认 if-any
    (yas-field-func field-index before-field-delimiter nil before-field-delimiter-function)
    (yas-field-string-content field-index field-content)
    (yas-field-func field-index after-field-delimiter nil after-field-delimiter-function)
    )))

(defun yas-key-field (field-index
                      field-content
                      &optional
                      before-field-delimiter
                      after-field-delimiter
                      before-field-delimiter-function
                      after-field-delimiter-function
                      not-exist-after-field-delimiter)
  "Insert a key field."
  (if (and (expand-with-binding) (region-active-p))
      (insert
       (concat
        (yas-expand-func before-field-delimiter nil before-field-delimiter-function)
        (yas-stripped-selected-text "" "" "")
        (unless not-exist-after-field-delimiter
          (yas-expand-func after-field-delimiter nil after-field-delimiter-function))
        ))
    (yas-field field-index
               field-content
               before-field-delimiter
               after-field-delimiter
               before-field-delimiter-function
               after-field-delimiter-function
               not-exist-after-field-delimiter
               )
    ))

(defun field (field-index
              field-content
              &optional
              before-field-delimiter
              after-field-delimiter
              field-seperator
              field-seperator-function
              before-field-delimiter-function
              after-field-delimiter-function
              not-exist-after-field-delimiter)
"field-index: 字段的索引
field-content: 字段的内容
before-field-delimiter: 字段之前的标识符，例如： 开始双引号
after-field-delimiter: 字段之后的标识符。例如： 结束双引号
"
  (insert
   (concat
    (yas-field-func field-index (or field-seperator (yas-field-seperator)) nil field-seperator-function)

    ;; 字段之前的分隔符，以及字段之前分隔符使用的函数， 默认 if-any
    (yas-field-func field-index  before-field-delimiter nil before-field-delimiter-function)

    (yas-field-string-content field-index field-content)


    (unless not-exist-after-field-delimiter
      (yas-field-func field-index  after-field-delimiter nil after-field-delimiter-function))
    )))

(defun string-field (field-index
                     field-content
                     &optional
                     before-field-delimiter
                     after-field-delimiter
                     field-seperator
                     )
  "Insert string field."
  (field
   field-index
   field-content
   before-field-delimiter
   after-field-delimiter
   field-seperator
   nil
   "quo-before"
   "quo-after"
   ))

(defun yas-camelize (&optional string)
  "merge underscore-split word into a capitalize form."
  (replace-regexp-in-string "_\\|@\\|\\$" "" (capitalize (or string yas-text))))

;; 下面的这一堆函数, 只是为了 quo, 稍后删除它.

;; (defun quo-before (&optional string)
;;   (when (if-any)
;;     (concat
;;      string
;;      (if (and (not (fourth (syntax-ppss))) (string-match "," (or yas-text yas-selected-text)))
;;          "%w["
;;        (quo))
;;      )))

;; (defun quo-after (&optional string)
;;   (when (if-any)
;;     (concat
;;      (if (and (not (fourth (syntax-ppss))) (string-match "," (or yas-text yas-selected-text)))
;;          "]"
;;        (quo))
;;      string)))

;; (defvar ruby-string-array-literal nil)
;; (defun ruby-string-array-literal ()
;;   "use ruby string array literal."
;; (set (make-local-variable 'ruby-string-array-literal) ""))

;; (defun instance-variable ()
;;   "transform to a instance variable."
;;   (replace-regexp-in-string "&\\|\*" "" (replace-regexp-in-string "[a-zA-Z_]+" "@\\\&" yas-text t nil)))

;; (defun local-variable ()
;;   "transform to a normally variable."
;;   (replace-regexp-in-string "&\\|\*" "" yas-text t nil))

(yas-define-condition-cache yas-use-region-p (or (expand-from-key-p) (use-region-p)))
(yas-define-condition-cache yas-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\` ?/)))
(yas-define-condition-cache yas-in-string-p (fourth (syntax-ppss)))
(yas-define-condition-cache yas-in-comment-p (fifth (syntax-ppss)))
(yas-define-condition-cache yas-in-string-or-comment-p (or (fourth (syntax-ppss)) (fifth (syntax-ppss))))
(yas-define-condition-cache yas-not-escaped-p (not (looking-back "\\\\")))
(yas-define-condition-cache yas-ruby-test-p (ruby-testfile-p))
(yas-define-condition-cache yas-ruby-not-test-p (not (ruby-testfile-p)))

;; (yas-define-condition-cache
;;  yas-rails-routes-p
;;  "Non-nil if the current buffer is a rails route."
;;  (and (rinari-root)
;;       (string-match "config/" default-directory)
;;       (string-match "routes.rb" (file-name-nondirectory buffer-file-name))))

;; (yas-define-condition-cache
;;  yas-minitest-describe-p
;;  "Non-nil if the current buffer is a ruby minitest file."
;;  (looking-back "describe.*" (line-beginning-position)))

;;; .yas-setup.el ends here.
