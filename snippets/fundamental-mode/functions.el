(defun _strip-string (string)
  "转义了四个字符：双引号，左右圆括号，还有转义字符自己"
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

(defun _yas-field-func (field-index &optional arg1 arg2 func)
  "用来拼装字符串 ${1:$(if-any \"hello\" \"world\")}, if-any 这个函数名也可以改。
(_yas-field-func \"1\" \"| \")
"
  (concat
   "${"
   field-index
   ":$("
   (or func "if-any")
   (when arg1 (concat " \"" (prin1-to-string arg1) "\""))
   (when arg2 (concat " \"" (prin1-to-string arg2) "\""))
   ")}"
   ))

(defun _yas-field-string-content (field-index &optional content)
  "用来拼接：${1:hello} 这样的内容。
(_yas-field-string-content \"1\" \"hello\")
yas-field 的依赖函数"
  (if content
      (concat
       "${"
       field-index
       ":"
       (prin1-to-string content)
       "}"
       )
    (concat "$" field-index)))


(defun _ruby-block-begin (&optional field-index field-content no-space)
  "生成 Ruby block 参数以及两边的部分, 如：do |x| 或 {|x|"
  (insert
   (if field-index
       (concat
        (_block-do)
        (_yas-field-func field-index (_block-left-border) " ")
        (_yas-field-string-content field-index field-content)
        (_yas-field-func field-index "| "))
     (concat (_block-do) (or no-space (or field-index " "))))
   ))

(defun _ruby-block-end (&optional end-newline)
  "生成 Ruby block 参数后面的部分"
  (insert
   (if yas-selected-text
       (if (string-match "\n" yas-selected-text)
           (if (string-match "\n\\'" yas-selected-text)
               (concat "end" (or end-newline "\n"))
             "end")
         " }")
     " }")
   ))

(defun _block-do (&optional do)
  "生成 Ruby block 参数前面的部分, 如：do 或 {"
  (or do
      (if yas-selected-text
          (if (string-match "\n" yas-selected-text)
              "do"
            "{")
        "{")))

(defun _block-left-border ()
  "处理 block 参数第一个 | 前面是否要空格"
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          " |"
        "|")
    "|"))

(defun _yas-expand-func (&optional arg1 arg2 func)
  "用来拼装 `(if-any \"hello\" \"world\")`"
  (concat
   "`("
   (or func "if-any")
   (when arg1 (concat " \"" (replace-regexp-in-string "\"" "\\\\\"" arg1) "\""))
   (when arg2 (concat " \"" (replace-regexp-in-string "\"" "\\\\\"" arg2) "\""))
   ")`"
   ))

(defun _yas-field-seperator ()
  "记不得上下文了，似乎有些语当做 expanding 时，前面要插入一个空格？"
  (if (member major-mode '(
                           emacs-lisp-mode
                           snippet-mode
                           sh-mode
                           ;; rhtml-mode
                           ;; html-mode
                           ))
      " "
    ""))

(defun _yas-newline ()
  "多行的时候输入 \n, 单行的时候，输入特定的字符作为表达式结尾字符。"
  (if yas-selected-text
      (if (string-match "\n" yas-selected-text)
          "\n"
        (_yas-newline-seperator))
    ""))

(defun _yas-newline-seperator ()
  "表达式结尾字符"
  (if (member major-mode '(sh-mode)) ";" ""))

(defun _expression-delimiter ()
  "忘记这个干嘛用的了，针对大多数模式，就是一个空字符串。"
  (unless (or
           (cl-fourth (syntax-ppss))
           (cl-fifth (syntax-ppss))
           (member major-mode '(emacs-lisp-mode snippet-mode rhtml-mode web-mode))
           )
    ""))

(defun _quo-before (&optional string)
  (when (if-any)
    (concat
     string
     (if (and (not (cl-fourth (syntax-ppss))) (string-match "," (or yas-text yas-selected-text)))
         "%w["
       (_quo))
     )))

(defun _quo-after (&optional string)
  (when (if-any)
    (concat
     (if (and (not (cl-fourth (syntax-ppss))) (string-match "," (or yas-text yas-selected-text)))
         "]"
       (_quo))
     string)))

(defun _quo ()
  "add a quote-mark, when press `:' or `,', erased automatic."
  (or
   ;; (when ruby-string-array-literal "")
   (when (string-match "^[{\s:'\"$]" (or yas-text yas-selected-text)) "")
   (_yas-quotation-mark)))

(defun _yas-quotation-mark ()
  "Insert quotation mark."
  (cond
   ((member major-mode '(
                         emacs-lisp-mode
                         snippet-mode
                         html-mode
                         rhtml-mode
                         web-mode))
    "\"")
   (t "\"")))

;; ---------------------------
(defun def (&optional def-begin def-end single-line-end-seperator def-space)
  "这个 def 用来定义一个 tag 的结束部分是否添加换行，根据选区的不同，
同时支持单行/多行定义的问题。这个模式尤其对于 web 模式有效，
例如，你希望 wrap 一个 div 像这样

<div>some text</div>

但是对于多行内容, 你希望以换行的方式 wrap.

<div>
  <p>
    text line 1
    text line 2
  <p>
</div>
 "
  (insert
   (concat
    (_def-begin def-begin (or def-space " "))
    "`(yas-stripped-selected-text)`$0"
    (_def-end def-end single-line-end-seperator (or def-space " "))
    )))

;; 为了做到关注点分离, 有时候, 存在公共的重复逻辑是不可避免的.
;; 例如: yas-def-begin 与 def-begin 中有关 yas-selected-text 的判断.
(defun _def-begin (&optional def-begin def-space)
  "解决一个语言在定义方法时，如果使用单行定义，加空格，多行定义加换行的问题。
这个函数使用 _yas-def-begin 来确定方法定义的分隔符。
"
  (let ((begin (or def-begin (_yas-def-begin)))
        (space (or def-space " "))
        )
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text)
            begin
          (concat begin space))
      (concat begin "\n"))))

(defun _yas-def-begin ()
  "在某一个语言中定义一个方法时，使用的起始分隔符。
例如：
单行 Ruby def 以 ; 开头，def meth; puts 100 end
js 以 { 开头, function(done) { ... }
"
  (cond
   ((member major-mode '(ruby-mode ruby-ts-mode enh-ruby-mode crystal-mode))
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text) "" ";")
      "")
    )
   ((member major-mode '(elixir-mode elixir-ts-mode)) " do")
   ((member major-mode '(sh-mode)) "")
   ((member major-mode '(js2-mode cc-mode rust-mode)) " {"))
  )

(defun _def-end (&optional def-end single-line-end-seperator def-space)
  "当定义一个方法时，结尾的字符"
  (let ((end (or def-end (_yas-def-end)))
        (space (or def-space " "))
        )
    (if yas-selected-text
        (if (string-match "\n" yas-selected-text)
            (if (string-match "\n\\'" yas-selected-text)
                (concat end "\n")
              end)
          (concat single-line-end-seperator space end))
      (concat "\n" end))))

(defun _yas-def-end ()
  "定义一个方法时，结尾分隔符."
  (cond
   ((member major-mode '(ruby-mode ruby-ts-mode elixir-mode elixir-ts-mode enh-ruby-mode crystal-mode)) "end")
   ((member major-mode '(sh-mode bash-ts-mode)) "done")
   ((member major-mode '(js2-mode cc-mode rust-mode)) "}")
   ((member major-mode '(rhtml-mode web-mode)) "<% end %>")
   ))


(defun end (&optional end)
  "抽象各种语言不同的 end, 并且，在选区的时候，可能添加尾部空格"
  (let ((end (or end (_yas-def-end))))
    (if yas-selected-text
        (if (string-match "\n\\'" yas-selected-text)
            (concat end "\n")
          end)
      end)))
