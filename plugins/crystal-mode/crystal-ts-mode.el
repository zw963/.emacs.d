;;; crystal-ts-mode.el --- Major mode for editing Crystal files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;;
;; Author: You + ChatGPT (ported from ruby-ts-mode style)
;; Keywords: crystal languages tree-sitter
;; Version: 0.1
;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Tree-sitter language versions
;;
;; crystal-ts-mode has been tested conceptually against:
;; - tree-sitter-crystal: commit 50ca9e6fcfb16a2cbcad59203cfd8ad650e25c49
;;
;; We try our best to make this mode work with recent grammar versions.
;; If the grammar updates and breaks the mode, please update the queries
;; and/or submit a bug report.

;;; Commentary:
;;
;; This file defines `crystal-ts-mode`, a major mode for editing Crystal
;; source files that uses Tree-sitter to parse and fontify the language.
;;
;; Requirements:
;; 1) Emacs must be compiled with tree-sitter support (Emacs 29+; you use Emacs 31 master).
;; 2) You must have the Crystal tree-sitter grammar installed (libtree-sitter-crystal).
;;
;; Installing the grammar (recommended):
;; - Ensure this file is loaded (so it registers `treesit-language-source-alist` entry)
;; - Then run:
;;     M-x treesit-install-language-grammar RET crystal RET
;;
;; Enabling the mode by default:
;; - Remap crystal-mode to crystal-ts-mode:
;;     (add-to-list 'major-mode-remap-alist '(crystal-mode . crystal-ts-mode))
;;
;; Note:
;; - This mode intentionally *depends on* crystal-mode:
;;   it inherits crystal-mode keybindings, syntax-propertize logic, and indentation.
;;   Tree-sitter is used primarily for parsing, navigation, imenu/outline, and font-lock.
;;
;; Feature overview (similar organization to ruby-ts-mode):
;; * Font Lock (tree-sitter driven, feature/level based)
;; * Navigation (beginning/end of defun)
;; * IMenu
;; * Outline (outline-minor-mode uses `treesit-outline-predicate`)
;;
;; Indentation:
;; - For phase 1, indentation is kept from crystal-mode (SMIE / classic).
;;   Tree-sitter indentation can be added later once you decide the exact rules you want.

;;; Code:

(require 'treesit)
(require 'crystal-mode)
(require 'cl-lib)

(treesit-declare-unavailable-functions)

;; Register the grammar source so `treesit-install-language-grammar` can install it.
;; Pinned to a known good commit from upstream.
(add-to-list
 'treesit-language-source-alist
 '(crystal "https://github.com/crystal-lang-tools/tree-sitter-crystal"
           :commit "50ca9e6fcfb16a2cbcad59203cfd8ad650e25c49")
 t)

(defgroup crystal-ts nil
  "Major mode for editing Crystal code."
  :prefix "crystal-ts-"
  :group 'languages
  :link '(url-link :tag "tree-sitter-crystal"
                   "https://github.com/crystal-lang-tools/tree-sitter-crystal"))

;;;; -------------------------------------------------------------------
;;;; Constants / regexps (mirror ruby-ts-mode structure)
;;;; -------------------------------------------------------------------

(defvar crystal-ts--operators
  '("+" "-" "*" "/" "//" "%" "&" "|" "^" "**"
    ">>" "<<" "==" "!=" "<" "<=" ">" ">=" "<=>" "==="
    "!" "~" "!~" "=~"
    "[]" "[]?" "[]="

    ;; wrapping operators (Crystal supports &+ etc)
    "&+" "&-" "&*" "&**"

    ;; range operators are punctuation-ish, but often treated as operators
    ".." "...")
  "Crystal operators for tree-sitter font-locking.")

(defvar crystal-ts--delimiter-tokens
  '("." "::" ";" "," ":" "=>" "->" "&." "&" "@" "@@" "$" "?")
  "Crystal delimiter/punctuation tokens for tree-sitter font-locking.")

;; Keep the keyword list conservative: these are syntactic keywords in Crystal.
;; (Methods like `puts` are not keywords and are parsed as calls; we do not list them here.)
(defvar crystal-ts--keywords
  '("abstract" "alias" "annotation" "as" "as?" "asm"
    "begin" "break"
    "case" "class"
    "def" "do"
    "else" "elsif" "end" "ensure" "enum" "extend"
    "for" "fun"
    "if" "in" "include"
    "lib"
    "macro" "module"
    "next"
    "of" "out"
    "private" "protected"
    "require" "rescue" "return"
    "select" "struct" "super"
    "then"
    "type" "typeof"
    "union" "uninitialized" "unless" "until"
    "verbatim"
    "when" "while" "with"
    "yield")
  "Crystal keywords for tree-sitter font-locking.")

(defconst crystal-ts--type-def-regexp
  (rx string-start
      (or "module_def" "class_def" "struct_def" "enum_def"
          "lib_def" "union_def" "annotation_def")
      string-end)
  "Regular expression matching Crystal type/namespace definitions.")

(defconst crystal-ts--defun-regexp
  (rx string-start
      (or
       ;; “Defun-ish” constructs.
       "method_def" "abstract_method_def" "macro_def" "fun_def"
       ;; Also treat type definitions as defuns for navigation/outline.
       "module_def" "class_def" "struct_def" "enum_def"
       "lib_def" "union_def" "annotation_def")
      string-end)
  "Regular expression matching Crystal definitions for navigation.")

(defun crystal-ts--lineno (node)
  "Return line number of NODE's start."
  (line-number-at-pos (treesit-node-start node)))

;;;; -------------------------------------------------------------------
;;;; Font-lock helpers (mirror ruby-ts-mode pattern)
;;;; -------------------------------------------------------------------

(defun crystal-ts--comment-font-lock (node override start end &rest _)
  "Apply font lock to comment NODE within START and END.
Applies `font-lock-comment-delimiter-face' and
`font-lock-comment-face'.  See `treesit-fontify-with-override' for
values of OVERRIDE."
  ;; In tree-sitter-crystal, `comment` is /#.*/ and includes the leading '#'.
  (let* ((node-start (treesit-node-start node))
         (plus-1 (1+ node-start))
         (node-end (treesit-node-end node))
         (text (treesit-node-text node t)))
    (when (and (>= node-start start)
               (<= plus-1 end)
               (string-match-p "\\`#" text))
      (treesit-fontify-with-override
       node-start plus-1 'font-lock-comment-delimiter-face override))
    (treesit-fontify-with-override
     (max plus-1 start) (min node-end end)
     'font-lock-comment-face override)))

(defun crystal-ts--font-lock-settings (language)
  "Tree-sitter font-lock settings for Crystal."
  (treesit-font-lock-rules
   :default-language language

   ;; -----------------------
   ;; Comments
   ;; -----------------------
   :feature 'comment
   '((comment) @crystal-ts--comment-font-lock)

   ;; -----------------------
   ;; Keywords
   ;; -----------------------
   :feature 'keyword
   `([,@crystal-ts--keywords] @font-lock-keyword-face)

   ;; self/nil/true/false are nodes (named terminals); highlight as builtin/constant.
   :feature 'builtin-variable
   '((self) @font-lock-builtin-face
     (underscore) @font-lock-builtin-face
     (special_variable) @font-lock-builtin-face)

   :feature 'constant
   '((true) @font-lock-constant-face
     (false) @font-lock-constant-face
     (nil) @font-lock-constant-face
     (pseudo_constant) @font-lock-builtin-face
     ;; Symbols are “constant-ish”.
     (symbol) @font-lock-constant-face)

   ;; -----------------------
   ;; Literals (numbers, chars, regex)
   ;; -----------------------
   :feature 'literal
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face
     (char) @font-lock-string-face)

   :feature 'regexp
   '((regex) @font-lock-regexp-face
     (regex_modifier) @font-lock-preprocessor-face)

   ;; -----------------------
   ;; Strings / heredocs / commands
   ;; -----------------------
   :feature 'string
   '((string) @font-lock-string-face
     (chained_string) @font-lock-string-face
     (command) @font-lock-string-face
     (heredoc_body) @font-lock-string-face
     (heredoc_start) @font-lock-string-face
     (heredoc_end) @font-lock-string-face
     ;; literal_content is used inside many string-ish constructs
     (literal_content) @font-lock-string-face)

   :feature 'interpolation
   '((interpolation "#{" @font-lock-misc-punctuation-face)
     (interpolation "}" @font-lock-misc-punctuation-face))

   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   ;; -----------------------
   ;; Types and type-ish nodes
   ;; -----------------------
   :feature 'type
   '((constant) @font-lock-type-face
     (generic_type) @font-lock-type-face
     (generic_instance_type) @font-lock-type-face
     (nilable_constant) @font-lock-type-face
     (nilable_type) @font-lock-type-face
     (union_type) @font-lock-type-face
     (named_type) @font-lock-type-face)

   ;; -----------------------
   ;; Variables
   ;; -----------------------
   :feature 'variable
   '((instance_var) @font-lock-variable-use-face
     (class_var) @font-lock-variable-use-face
     (macro_var) @font-lock-variable-use-face
     (global_var) @font-lock-variable-use-face)

   :feature 'parameter
   '((param (identifier) @font-lock-variable-name-face)
     (splat_param (identifier) @font-lock-variable-name-face)
     (double_splat_param (identifier) @font-lock-variable-name-face)
     (block_param (identifier) @font-lock-variable-name-face)
     (fun_param (identifier) @font-lock-variable-name-face))

   ;; -----------------------
   ;; Definitions
   ;; -----------------------
   :feature 'definition
   '((module_def name: [(constant) (generic_type)] @font-lock-type-face)
     (class_def  name: [(constant) (generic_type)] @font-lock-type-face)
     (struct_def name: [(constant) (generic_type)] @font-lock-type-face)
     (enum_def   name: (constant) @font-lock-type-face)
     (lib_def    name: [(constant) (generic_type)] @font-lock-type-face)
     (union_def  name: (constant) @font-lock-type-face)
     (annotation_def name: (constant) @font-lock-type-face)

     (method_def
      name: [(identifier) (operator) (setter)] @font-lock-function-name-face)
     (abstract_method_def
      name: [(identifier) (operator) (setter)] @font-lock-function-name-face)
     (macro_def
      name: [(identifier) (operator)] @font-lock-function-name-face)
     (fun_def
      name: [(identifier) (constant) (operator)] @font-lock-function-name-face))

   ;; Attributes (annotations) usage
   :feature 'attribute
   '((annotation) @font-lock-preprocessor-face)

   ;; -----------------------
   ;; Function calls
   ;; -----------------------
   :feature 'function
   '((call
      name: [(identifier) (operator) (setter)] @font-lock-function-call-face))

   ;; -----------------------
   ;; Operators / punctuation / brackets
   ;; -----------------------
   :feature 'operator
   `((operator) @font-lock-operator-face
     ;; Index operator “[]” is represented in index_call; highlight the token.
     (index_call "[]") @font-lock-operator-face
     ;; Blocks use pipes.
     (block "|") @font-lock-operator-face
     ;; Some operator tokens may appear as plain strings in the tree.
     [,@crystal-ts--operators] @font-lock-operator-face)

   :feature 'punctuation
   `([,@crystal-ts--delimiter-tokens] @font-lock-delimiter-face)

   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   ;; -----------------------
   ;; Errors
   ;; -----------------------
   :feature 'error
   '((ERROR) @font-lock-warning-face)))

;;;; -------------------------------------------------------------------
;;;; Naming helpers (imenu + add-log) — mirror ruby-ts-mode style
;;;; -------------------------------------------------------------------

(defun crystal-ts--type-node-p (node)
  "Return non-nil if NODE is a Crystal type/namespace definition."
  (and node (string-match-p crystal-ts--type-def-regexp (treesit-node-type node))))

(defun crystal-ts--defun-node-p (node)
  "Return non-nil if NODE is a Crystal definition node."
  (and node (string-match-p crystal-ts--defun-regexp (treesit-node-type node))))

(defun crystal-ts--get-name (node)
  "Return the text of NODE's `name' field (if present)."
  (when-let ((name (treesit-node-child-by-field-name node "name")))
    (treesit-node-text name t)))

(defun crystal-ts--method-receiver (method-node)
  "Return receiver text for METHOD-NODE if it is a class method, else nil.
In tree-sitter-crystal, method_def may have field `class` like `Foo.` or `self.`."
  (when-let ((klass (treesit-node-child-by-field-name method-node "class")))
    (let ((txt (string-trim (treesit-node-text klass t))))
      (when (and txt (not (string-empty-p txt)))
        (if (string-suffix-p "." txt) (substring txt 0 -1) txt)))))

(defun crystal-ts--enclosing-types (node)
  "Return list of enclosing type names from outermost to innermost for NODE.
Includes NODE itself if it is a type definition node."
  (let ((types nil)
        (n node))
    (while (setq n (treesit-parent-until n #'crystal-ts--type-node-p t))
      (push (crystal-ts--get-name n) types)
      (setq n (treesit-node-parent n)))
    (nreverse (delq nil types))))

(defun crystal-ts--full-name (node)
  "Return fully qualified name of NODE for imenu/add-log."
  (let* ((type-stack (crystal-ts--enclosing-types node))
         (context (and type-stack (string-join type-stack "::")))
         (type (treesit-node-type node)))
    (pcase type
      ;; Type/namespace-like
      ((or "module_def" "class_def" "struct_def" "enum_def" "lib_def" "union_def" "annotation_def")
       context)

      ;; Methods
      ((or "method_def" "abstract_method_def")
       (let* ((name (or (crystal-ts--get-name node) ""))
              (receiver (crystal-ts--method-receiver node))
              (current-type (car (last type-stack)))
              (delim (if receiver "." "#"))
              (prefix
               (cond
                ;; class method on self: use enclosing type context
                ((and receiver (string= receiver "self"))
                 context)
                ;; explicit receiver matches innermost type name: treat as class method on context
                ((and receiver current-type (string= receiver current-type))
                 context)
                ;; explicit receiver given, but doesn't match context: use it as prefix
                (receiver receiver)
                ;; no receiver: instance method under context
                (t context))))
         (cond
          ((and prefix (not (string-empty-p prefix)))
           (concat prefix delim name))
          (t name))))

      ;; Macros: treat like instance-ish in naming (#)
      ("macro_def"
       (let* ((name (or (crystal-ts--get-name node) ""))
              (prefix context))
         (if (and prefix (not (string-empty-p prefix)))
             (concat prefix "#" name)
           name)))

      ;; C fun: treat as namespace member (Lib::fun)
      ("fun_def"
       (let* ((name (or (crystal-ts--get-name node) ""))
              (prefix context))
         (if (and prefix (not (string-empty-p prefix)))
             (concat prefix "::" name)
           name)))

      (_
       ;; Fallback: use `name` if present, else nil
       (or (crystal-ts--get-name node) nil)))))

(defun crystal-ts--imenu-helper (tree)
  "Convert a treesit sparse tree TREE into a flat imenu list."
  (if (cdr tree)
      (cl-mapcan #'crystal-ts--imenu-helper (cdr tree))
    (let* ((node (car tree))
           (name (crystal-ts--full-name node)))
      (when (and name (not (string-empty-p name)))
        (list (cons name (treesit-node-start node)))))))

(defun crystal-ts--imenu ()
  "Return Imenu alist for the current buffer."
  (let* ((root (treesit-buffer-root-node))
         ;; Include type defs + methods + macros + fun defs.
         (tree (treesit-induce-sparse-tree
                root
                (rx bol
                    (or "module_def" "class_def" "struct_def" "enum_def"
                        "lib_def" "union_def" "annotation_def"
                        "method_def" "abstract_method_def" "macro_def" "fun_def")
                    eol))))
    (delq nil (crystal-ts--imenu-helper tree))))

(defun crystal-ts-add-log-current-function ()
  "Return the current Crystal definition name as a string.

This is used by `add-log-current-defun-function`."
  (let* ((node (treesit-node-at (point)))
         (pred (lambda (n)
                 (and (<= (treesit-node-start n) (point))
                      (>= (treesit-node-end n) (point))
                      (crystal-ts--defun-node-p n))))
         (defun (and node (treesit-parent-until node pred t))))
    (when defun
      (crystal-ts--full-name defun))))

;;;; -------------------------------------------------------------------
;;;; treesit-thing-settings (optional but keeps parity with ruby-ts-mode)
;;;; -------------------------------------------------------------------

(defun crystal-ts--list-p (node)
  "Predicate used by `treesit-thing-settings` for list nodes."
  (treesit-node-check node 'named))

(defun crystal-ts--sexp-p (node)
  "Predicate for sexp nodes.

For phase 1 we keep it permissive: any named node is considered a sexp,
except a few container nodes."
  (and (treesit-node-check node 'named)
       (not (member (treesit-node-type node)
                    '("expressions" "_statements" "comment")))))

;;;; -------------------------------------------------------------------
;;;; Keymap (inherits crystal-mode-map)
;;;; -------------------------------------------------------------------

(defvar-keymap crystal-ts-mode-map
  :doc "Keymap used in crystal-ts-mode."
  :parent crystal-mode-map)

;;;###autoload
(define-derived-mode crystal-ts-mode crystal-mode "Crystal"
  "Major mode for editing Crystal, powered by tree-sitter."
  :group 'crystal
  :syntax-table crystal-mode-syntax-table

  (unless (treesit-ensure-installed 'crystal)
    (error "Tree-sitter for Crystal isn't available"))

  ;; Keep crystal-mode’s indentation/syntax propertize:
  ;; crystal-mode already set these up in its body (we are derived from it).
  (let ((orig-indent-line-function indent-line-function)
        (orig-syntax-propertize-function syntax-propertize-function))

    (setq treesit-primary-parser (treesit-parser-create 'crystal))

    ;; add-log
    (setq-local add-log-current-defun-function #'crystal-ts-add-log-current-function)

    ;; Navigation / defun movement.
    (setq-local treesit-defun-type-regexp crystal-ts--defun-regexp)

    ;; “Things” (sexp/list/etc) used by treesit navigation commands.
    (setq-local treesit-thing-settings
                `((crystal
                   (sexp ,(cons (rx bos (or "expressions" "comment") eos)
                                #'crystal-ts--sexp-p))
                   (list ,(cons (rx bos
                                    (or "array" "hash" "tuple" "named_tuple"
                                        "argument_list" "param_list" "block"
                                        "parenthesized_expressions"
                                        "string" "regex" "heredoc_body")
                                    eos)
                                #'crystal-ts--list-p))
                   (sentence ,(rx bos (or "return" "assign" "call") eos))
                   (text ,(lambda (n)
                            (member (treesit-node-type n)
                                    '("comment" "literal_content" "heredoc_content")))))))

    ;; Imenu.
    (setq-local imenu-create-index-function #'crystal-ts--imenu)

    ;; Outline minor mode: headings are definitions.
    (setq-local treesit-outline-predicate
                `(and ,(rx bos
                           (or "module_def" "class_def" "struct_def" "enum_def"
                               "lib_def" "union_def" "annotation_def"
                               "method_def" "abstract_method_def" "macro_def" "fun_def")
                           eos)
                      named))
    ;; Restore default outline variables to use `treesit-outline-predicate`.
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)

    ;; Font-lock (tree-sitter).
    (setq-local treesit-font-lock-settings (crystal-ts--font-lock-settings 'crystal))

    ;; Feature levels (mirrors ruby-ts-mode style).
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string type)
                  (builtin-variable constant literal regexp
                                    variable parameter attribute
                                    interpolation escape-sequence
                                    definition function operator punctuation)
                  (bracket error)))

    ;; Apply treesit setup.
    (treesit-major-mode-setup)

    ;; Restore indentation + syntax propertize from crystal-mode.
    ;; (treesit-major-mode-setup may set indent-line-function if indent rules exist;
    ;; we keep crystal-mode indentation for phase 1.)
    (setq-local indent-line-function orig-indent-line-function)
    (setq-local syntax-propertize-function orig-syntax-propertize-function)))

;;;###autoload
(when (boundp 'treesit-major-mode-remap-alist)
  (add-to-list 'treesit-major-mode-remap-alist
               '(crystal-mode . crystal-ts-mode)))

;;;###autoload
(when (boundp 'major-mode-remap-alist)
  (add-to-list 'major-mode-remap-alist
               '(crystal-mode . crystal-ts-mode)))

(provide 'crystal-ts-mode)

;;; crystal-ts-mode.el ends here
