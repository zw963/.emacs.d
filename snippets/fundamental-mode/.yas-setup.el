;; -*-Emacs-Lisp-*-

;; 目前已知的稳定版函数有：
;; (yas-field "1" "arg" " \"" "\"")
;; (yas-key-field "1" "arg" "|" "|")
;; (yas-if-any "1" "hello" "world")  "${1:$(if-any "hello" "world")}"
;; ------------------------------ expansion functions ------------------------------

(relative-load "functions.el")

(defun yas-if-any (field-index &optional arg1 arg2)
  "用来拼装字符串 ${1:$(if-any \"hello\" \"world\")}, 快捷键是 a"
  (insert (_yas-field-func field-index arg1 arg2)))

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
before-field-delimiter: 字段之前的标识符，例如：开始的那个双引号
after-field-delimiter: 字段之后的标识符。例如： 结束的那个双引号
before-field-delimiter-function: 默认是 if-any
after-field-delimiter-function: 默认也是 if-any
not-exist-after-field-delimiter：delimiter 有时候可能是只有前面有，后面没有，例如，符号
              field-seperator
              field-seperator-function
这个允许在起始的 delimiter 之前，可选的增加一个字符串, 增加的判断函数也是 if-any
这个 seperator 定义了不同的语言中，参数之间的分隔符，例如，lisp 是空格，大多数语言则是逗号 ,
"
  (insert
   (concat
    (_yas-field-func field-index (or field-seperator (_yas-field-seperator)) nil field-seperator-function)

    ;; 字段之前的分隔符，以及字段之前分隔符使用的函数， 默认 if-any
    (_yas-field-func field-index before-field-delimiter nil before-field-delimiter-function)
    (_yas-field-string-content field-index field-content)
    (unless not-exist-after-field-delimiter
      (_yas-field-func field-index after-field-delimiter nil after-field-delimiter-function))
    )))

(defun string-field (field-index
                     field-content
                     &optional
                     before-field-delimiter
                     after-field-delimiter
                     field-seperator
                     )
  "两边会自动增加删除双引号的 field"
  (field
   field-index
   field-content
   before-field-delimiter
   after-field-delimiter
   field-seperator
   nil
   "_quo-before"
   "_quo-after"
   ))

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
      (insert
       (concat
        (_yas-field-func field-index (or field-seperator (_yas-field-seperator)) nil field-seperator-function)
        (_yas-expand-func before-field-delimiter nil before-field-delimiter-function)
        (yas-stripped-selected-text "" "" "")
        (unless not-exist-after-field-delimiter
          (_yas-expand-func after-field-delimiter nil after-field-delimiter-function))

        ))
    (field
     field-index
     field-content
     before-field-delimiter
     after-field-delimiter
     field-seperator
     field-seperator-function
     after-field-delimiter-function
     before-field-delimiter-function
     )))

(defun string-key-field (field-index
                         field-content
                         &optional
                         before-field-delimiter
                         after-field-delimiter
                         field-seperator)
  "Insert string key-field."
  (key-field
   field-index
   field-content
   (or before-field-delimiter nil)
   (or after-field-delimiter nil)
   field-seperator
   nil
   "_quo-before"
   "_quo-after"
   ))

(defun yas-stripped-selected-text (&optional before after expression-delimiter)
  "删除选区开始部分的白空格, 选区开始结束部分的空白行, 如果是多行字符串，最后再加一个空行"
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          (let ((text (replace-regexp-in-string
                       ".*\\(\n\\)\\'" ""
                       (replace-regexp-in-string
                        "\\`\\(\n\\).*" ""
                        yas-selected-text nil nil 1) nil nil 1)))
            (concat
             (or before (_yas-newline))
             (replace-regexp-in-string "\\`\\([ \t]+\\).*" "" text nil nil 1)
             (or after (_yas-newline))))
        (concat before yas-selected-text after (or expression-delimiter (_expression-delimiter))))
    ;; (concat before yas-selected-text after)
    ))

(defun if-key (string1 &optional string2)
  "Add STRING1 if expand with a key, only be use in `...`."
  (if (expand-from-key-p)
      (insert string1)
    (when string2 (insert string2))))

(defun if-binding (string1 &optional string2)
  "Add STRING1 if expand with a binding, only be use in `...`."
  (if (expand-with-binding)
      (insert string1)
    (when string2 (insert string2))))

(defun ruby-do-block (&optional field-index field-content)
  "这个不同于 ruby-block, 这个总是输入 do end 形式的 block."
  ;; 如果是两个参数.
  (insert
   (if field-content
       (concat
        "do"
        (_yas-field-func field-index " |")
        (_yas-field-string-content field-index field-content)
        ;; 记住, 插入的字符串不可以 \n 结尾, 否则当 expand 的时候, 更改这个字段会发生缩进错误.
        ;; 它会给你再加一个 \n, 但是不会自动缩进了.
        (_yas-field-func field-index "|")
        ;; 原来在 | 后面的 \n, 挪到了这里.
        (if (expand-from-key-p) "\n" "")
        "`(yas-stripped-selected-text)`$0"
        "\nend"
        )
     ;; 如果不是两个参数, field-index 如果存在, 表示不插入 $0, 否则插入.
     (concat
      "do\n"
      (unless field-index "`(yas-stripped-selected-text \"\" \"\")``(if-key \"$0\")`")
      "\nend")
     )))

(defun ruby-block (&optional field-index field-content no-space)
  "这个用来输入一个 do ... end block.
支持输入 block 参数，并且，当使用 binding 的时候，支持根据当前选区是否多行，变换为 { ... } 形式，
同时还会处理 {|arg| } 这里，arg 前面的空格的问题。
如果总是希望输入 do ... end 形式，使用 ruby-do-block
"
  (_ruby-block-begin field-index field-content no-space)
  (insert "`(yas-stripped-selected-text)``(if-key \"$0\")`")
  (_ruby-block-end)
  )

(defun ruby-module-name-field (field-index)
  "Insert ruby module name field."
  (field
   field-index
   (class-from-file)
   ""))

;; (defun instance-variable ()
;;   "transform to a instance variable."
;;   (replace-regexp-in-string "&\\|\*" "" (replace-regexp-in-string "[a-zA-Z_]+" "@\\\&" yas-text t nil)))

;; (defun local-variable ()
;;   "transform to a normally variable."
;;   (replace-regexp-in-string "&\\|\*" "" yas-text t nil))

(yas-define-condition-cache yas-use-region-p (or (expand-from-key-p) (use-region-p)))
(yas-define-condition-cache yas-in-interpolated-string-p (member (fourth (syntax-ppss)) (list ?\" ?\` ?/)))
(yas-define-condition-cache yas-in-string-p (fourth (syntax-ppss)))
(yas-define-condition-cache yas-not-in-string-p (not (fourth (syntax-ppss))))
(yas-define-condition-cache yas-in-comment-p (fifth (syntax-ppss)))
(yas-define-condition-cache yas-in-string-or-comment-p (or (fourth (syntax-ppss)) (fifth (syntax-ppss))))
(yas-define-condition-cache yas-not-escaped-p (not (looking-back "\\\\")))
(yas-define-condition-cache yas-ruby-testfile-p (ruby-testfile-p))
(yas-define-condition-cache yas-ruby-specfile-p (ruby-specfile-p))
;; (yas-define-condition-cache
;;  yas-rails-test-p
;;  "Non-nil if the current buffer is a rails test."
;;  (and (rinari-root)
;;       (string-match "test/\\|spec/" default-directory)))

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

;; =========================== 目前没用到的函数 ====================================

;; (defun stripped-field ()
;;   (if yas-selected-text
;;       (_strip-string yas-selected-text)))

;; =========================== 不打算用的函数 ====================================



;;; .yas-setup.el ends here.
