;;; crystal-ts-mode.el --- Crystal major mode using built-in tree-sitter  -*- lexical-binding: t; -*-

;; Version: 0.4
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'treesit)
(require 'prog-mode)
(require 'cl-lib)

(treesit-declare-unavailable-functions)

;;;; Customization

(defun crystal-ts-mode--face (preferred fallback)
  "Return PREFERRED if it's a defined face, otherwise FALLBACK."
  (if (facep preferred) preferred fallback))

(defgroup crystal-ts nil
  "Crystal major mode using Emacs built-in tree-sitter."
  :group 'languages
  :prefix "crystal-ts-")

(defcustom crystal-ts-mode-query-dir nil
  "Directory containing Crystal tree-sitter query files.

If non-nil, this directory may contain:
  - highlights.scm (recommended)
  - folds.scm (optional)

If nil, `crystal-ts-mode' tries standard locations under
`user-emacs-directory'."
  :type '(choice (const :tag "Auto" nil)
                 (directory :tag "Query directory"))
  :group 'crystal-ts)

(defcustom crystal-ts-indent-offset 2
  "Indentation offset (number of spaces) for `crystal-ts-mode'."
  :type 'integer
  :group 'crystal-ts)

(defcustom crystal-ts-use-ruby-indent t
  "If non-nil, reuse `ruby-indent-line' for indentation."
  :type 'boolean
  :group 'crystal-ts)

(defcustom crystal-ts-enable-fold t
  "If non-nil and `treesit-fold' is available, enable `treesit-fold-mode'."
  :type 'boolean
  :group 'crystal-ts)

(defcustom crystal-ts-prefer-user-grammar t
  "If non-nil, prefer grammar in `~/.emacs.d/tree-sitter/'."
  :type 'boolean
  :group 'crystal-ts)

;;;; Query loading / sanitizing

(defvar crystal-ts-mode--font-lock-settings-cache nil)
(defvar crystal-ts-mode--query-cache (make-hash-table :test 'equal))

(defun crystal-ts-mode--default-query-dirs ()
  "Standard query directories to try."
  (let ((base (expand-file-name "tree-sitter/" user-emacs-directory)))
    (list (expand-file-name "queries/crystal/" base)
          (expand-file-name "crystal/" base)
          (expand-file-name "tree-sitter/queries/crystal/" user-emacs-directory))))

(defun crystal-ts-mode--query-dir ()
  "Return the query directory to use, or nil."
  (cond
   ((and crystal-ts-mode-query-dir
         (file-directory-p crystal-ts-mode-query-dir))
    (file-name-as-directory (expand-file-name crystal-ts-mode-query-dir)))
   (t
    (cl-loop for d in (crystal-ts-mode--default-query-dirs)
             when (file-directory-p d)
             return (file-name-as-directory d)))))

(defun crystal-ts-mode--read-file (path)
  "Read PATH and return its content, or nil."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun crystal-ts-mode--load-query (filename &optional fallback)
  "Load query file FILENAME from query dir, else FALLBACK."
  (let* ((dir (crystal-ts-mode--query-dir))
         (path (and dir (expand-file-name filename dir)))
         (cache-key (or path (concat "embedded:" filename)))
         (cached (gethash cache-key crystal-ts-mode--query-cache)))
    (or cached
        (let ((txt (or (crystal-ts-mode--read-file path) fallback)))
          (puthash cache-key txt crystal-ts-mode--query-cache)
          txt))))

(defun crystal-ts-mode--sanitize-query (query)
  "Sanitize QUERY for Emacs treesit.

Remove unsupported (#set! ...) directives."
  (setq query (replace-regexp-in-string "(#set![^)]*)" "" query))
  (setq query (replace-regexp-in-string "^[ \t]+$" "" query))
  query)

;;;; Capture -> face mapping (keep your working logic)

(defun crystal-ts-mode--capture-face-alist ()
  "Return mapping from nvim capture names (without @) to Emacs faces."
  (let* ((call-face (crystal-ts-mode--face 'font-lock-function-call-face
                                          'font-lock-function-name-face))
         (var-use   (crystal-ts-mode--face 'font-lock-variable-use-face
                                          'font-lock-variable-name-face))
         (prop-face (crystal-ts-mode--face 'font-lock-property-name-face
                                          'font-lock-variable-name-face))
         (num-face  (crystal-ts-mode--face 'font-lock-number-face
                                          'font-lock-constant-face))
         (op-face   (crystal-ts-mode--face 'font-lock-operator-face
                                          'font-lock-keyword-face))
         (br-face   (crystal-ts-mode--face 'font-lock-bracket-face
                                          'font-lock-punctuation-face))
         (delim-face (crystal-ts-mode--face 'font-lock-delimiter-face
                                           (crystal-ts-mode--face 'font-lock-punctuation-face
                                                                 'font-lock-keyword-face)))
         (misc-punct (crystal-ts-mode--face 'font-lock-misc-punctuation-face
                                           (crystal-ts-mode--face 'font-lock-punctuation-face
                                                                 'font-lock-keyword-face)))
         (escape-face (crystal-ts-mode--face 'font-lock-escape-face
                                            'font-lock-string-face))
         (builtin-face (crystal-ts-mode--face 'font-lock-builtin-face
                                             'font-lock-constant-face)))
    `(
      ("keyword" . font-lock-keyword-face)
      ("keyword.function" . font-lock-keyword-face)
      ("keyword.type" . font-lock-keyword-face)
      ("keyword.import" . font-lock-preprocessor-face)
      ("keyword.return" . font-lock-keyword-face)
      ("keyword.conditional" . font-lock-keyword-face)
      ("keyword.conditional.ternary" . font-lock-keyword-face)
      ("keyword.repeat" . font-lock-keyword-face)
      ("keyword.exception" . font-lock-keyword-face)
      ("keyword.modifier" . font-lock-keyword-face)

      ("comment" . font-lock-comment-face)
      ("string" . font-lock-string-face)
      ("string.escape" . ,escape-face)
      ("string.special" . font-lock-string-face)
      ("string.special.symbol" . font-lock-constant-face)
      ("string.regexp" . font-lock-regexp-face)
      ("character" . font-lock-constant-face)
      ("character.special" . font-lock-constant-face)

      ("number" . ,num-face)
      ("number.float" . ,num-face)
      ("boolean" . font-lock-constant-face)
      ("constant.builtin" . ,builtin-face)
      ("label" . font-lock-constant-face)

      ("operator" . ,op-face)
      ("punctuation.delimiter" . ,delim-face)
      ("punctuation.bracket" . ,br-face)
      ("punctuation.special" . ,misc-punct)
      ("tag.delimiter" . font-lock-preprocessor-face)

      ("type" . font-lock-type-face)
      ("type.builtin" . ,builtin-face)
      ("attribute" . font-lock-preprocessor-face)

      ("function" . font-lock-function-name-face)
      ("function.method" . font-lock-function-name-face)
      ("function.call" . ,call-face)

      ("variable.parameter" . font-lock-variable-name-face)
      ("variable.parameter.builtin" . ,builtin-face)
      ("variable.member" . ,var-use)
      ("variable.builtin" . ,builtin-face)
      ("variable" . ,var-use)

      ("property" . ,prop-face))))

(defun crystal-ts-mode--capture->face (cap)
  "Map capture name string CAP (without @) to an Emacs face symbol."
  (let* ((alist (crystal-ts-mode--capture-face-alist))
         (probe cap)
         face)
    (while (and probe (not face))
      (setq face (cdr (assoc probe alist)))
      (unless face
        (if (string-match "\\`\\(.*\\)\\.[^.]+\\'" probe)
            (setq probe (match-string 1 probe))
          (setq probe nil))))
    face))

(defun crystal-ts-mode--rewrite-captures (query)
  "Rewrite nvim-style captures in QUERY to Emacs face captures."
  (let ((q (crystal-ts-mode--sanitize-query query)))
    (replace-regexp-in-string
     "@\\([A-Za-z0-9_.-]+\\)"
     (lambda (m)
       (let* ((cap (match-string 1 m))
              (face (crystal-ts-mode--capture->face cap)))
         (if face (concat "@" (symbol-name face)) m)))
     q t t)))

;;;; Font-lock

(defconst crystal-ts-mode--mini-highlights-query
  "[(comment)] @font-lock-comment-face
   [(string)] @font-lock-string-face
   [(regex)] @font-lock-regexp-face
   [\"if\" \"else\" \"elsif\" \"unless\" \"case\" \"when\" \"begin\" \"rescue\" \"ensure\" \"end\"
    \"def\" \"class\" \"module\" \"struct\" \"enum\" \"lib\" \"fun\" \"macro\"
    \"return\" \"yield\" \"while\" \"until\" \"for\" \"select\"]
   @font-lock-keyword-face"
  "Tiny emergency fallback query (kept intentionally small).")

(defun crystal-ts-mode--font-lock-settings ()
  "Return treesit font-lock settings for Crystal (cached)."
  (or crystal-ts-mode--font-lock-settings-cache
      (let* ((raw (crystal-ts-mode--load-query "highlights.scm" nil))
             (query (if raw (crystal-ts-mode--rewrite-captures raw)
                      crystal-ts-mode--mini-highlights-query)))
        (condition-case err
            (progn
              (treesit-query-compile 'crystal query)
              (setq crystal-ts-mode--font-lock-settings-cache
                    (treesit-font-lock-rules
                     :language 'crystal
                     :feature 'crystal
                     query)))
          (treesit-query-error
           (message "crystal-ts-mode: highlights query failed: %s"
                    (error-message-string err))
           (setq crystal-ts-mode--font-lock-settings-cache
                 (treesit-font-lock-rules
                  :language 'crystal
                  :feature 'crystal
                  crystal-ts-mode--mini-highlights-query)))))))

;;;###autoload
(defun crystal-ts-mode-reload-queries ()
  "Reload Crystal tree-sitter query files and refontify current buffer."
  (interactive)
  (clrhash crystal-ts-mode--query-cache)
  (setq crystal-ts-mode--font-lock-settings-cache nil)
  (when (derived-mode-p 'crystal-ts-mode)
    (setq-local treesit-font-lock-settings (crystal-ts-mode--font-lock-settings))
    (treesit-font-lock-recompute-features)
    (font-lock-flush)))

;;;; Folding (treesit-fold) — keep your working behavior

(defconst crystal-ts-mode--mini-folds-query
  "[(annotation_def) (begin) (block) (c_struct_def) (case) (class_def) (else)
    (enum_def) (fun_def) (if) (lib_def) (method_def) (module_def) (rescue)
    (select) (struct_def) (union_def) (when) (while)
    (array) (array_like) (hash) (hash_like) (named_tuple) (tuple)] @fold"
  "Tiny fallback folds query.")

(defun crystal-ts-mode--fold-node-types ()
  "Return list of node type strings from folds.scm (or fallback)."
  (let* ((raw (crystal-ts-mode--load-query "folds.scm" crystal-ts-mode--mini-folds-query))
         (txt (crystal-ts-mode--sanitize-query raw))
         (pos 0)
         (out nil))
    (while (string-match "(\\([A-Za-z0-9_]+\\))" txt pos)
      (push (match-string 1 txt) out)
      (setq pos (match-end 0)))
    (delete-dups (nreverse out))))

(defun crystal-ts-mode--fold-range-bodyish (node &rest _)
  "Fold a body-like region inside NODE."
  (let ((body (or (treesit-node-child-by-field-name node "body")
                  (treesit-node-child-by-field-name node "then")
                  (treesit-node-child-by-field-name node "else")
                  (treesit-node-child-by-field-name node "rescue")
                  (treesit-node-child-by-field-name node "ensure"))))
    (cond
     (body
      (cons (treesit-node-start body) (treesit-node-end body)))
     (t
      (let* ((n (treesit-node-child-count node nil))
             (first (and (> n 0) (treesit-node-child node 0 nil)))
             (last  (and (> n 1) (treesit-node-child node (1- n) nil))))
        (when (and first last)
          (let ((s (treesit-node-end first))
                (e (treesit-node-start last)))
            (when (< s e) (cons s e)))))))))

(defun crystal-ts-mode--fold-range-bracketed (node &rest _)
  "Fold inside a bracketed NODE (array/hash/tuple/etc.)."
  (let* ((n (treesit-node-child-count node nil))
         (first (and (> n 0) (treesit-node-child node 0 nil)))
         (last  (and (> n 1) (treesit-node-child node (1- n) nil))))
    (when (and first last)
      (let ((s (treesit-node-end first))
            (e (treesit-node-start last)))
        (when (< s e) (cons s e))))))

(defun crystal-ts-mode--register-fold-definitions ()
  "Register fold definitions for `crystal-ts-mode'."
  (when (and crystal-ts-enable-fold
             (require 'treesit-fold nil t)
             (boundp 'treesit-fold-range-alist))
    (let* ((types (crystal-ts-mode--fold-node-types))
           (bracketed (mapcar #'intern '("array" "array_like" "hash" "hash_like" "named_tuple" "tuple")))
           (ranges (mapcar (lambda (ty)
                             (let ((sym (intern ty)))
                               (cons sym (if (memq sym bracketed)
                                             #'crystal-ts-mode--fold-range-bracketed
                                           #'crystal-ts-mode--fold-range-bodyish))))
                           types)))
      (setf (alist-get 'crystal-ts-mode treesit-fold-range-alist) ranges)
      t)))

;;;; Movement / forward-sexp (treesit-thing-settings)

(defconst crystal-ts-mode--list-node-regexp
  (rx bos
      (or "class_def" "module_def" "struct_def" "enum_def" "lib_def" "union_def"
          "method_def" "macro_def" "fun_def" "abstract_method_def"
          "if" "unless" "case" "select" "begin" "rescue" "ensure" "while" "until" "for"
          "block"
          "parenthesized_statements" "argument_list"
          "array" "hash" "tuple" "named_tuple"
          "interpolation" "string" "regex" "heredoc_body")
      eos)
  "Node types treated as lists for `forward-sexp' in Crystal.")

(defun crystal-ts-mode--list-p (node)
  "Return non-nil if NODE should be treated as a list."
  (and (treesit-node-check node 'named)
       (string-match-p crystal-ts-mode--list-node-regexp (treesit-node-type node))))

;;;; Mode definition

(defvar crystal-ts-mode--defun-type-regexp
  (rx string-start
      (or "annotation_def" "c_struct_def" "class_def" "enum_def" "fun_def"
          "lib_def" "macro_def" "method_def" "module_def" "struct_def" "union_def")
      string-end)
  "Regexp matching Crystal defun-like node types.")

(defvar-keymap crystal-ts-mode-map
  :doc "Keymap for `crystal-ts-mode'."
  :parent prog-mode-map)

;;;###autoload
(define-derived-mode crystal-ts-mode prog-mode "Crystal[ts]"
  "Major mode for editing Crystal, powered by tree-sitter."
  :group 'crystal-ts

  (when crystal-ts-prefer-user-grammar
    (let ((dir (expand-file-name "tree-sitter/" user-emacs-directory)))
      (when (file-directory-p dir)
        (add-to-list 'treesit-extra-load-path dir))))

  (unless (treesit-available-p)
    (error "Emacs built without tree-sitter support"))
  (unless (treesit-language-available-p 'crystal)
    (error "Tree-sitter grammar for Crystal not found (language `crystal`)"))

  ;; Parser (important for treesit-fold and others).
  (setq treesit-primary-parser (treesit-parser-create 'crystal))

  ;; Comments.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local comment-end "")

  ;; Navigation.
  (setq-local treesit-defun-type-regexp crystal-ts-mode--defun-type-regexp)

  ;; Sexp movement.
  (setq-local treesit-thing-settings
              `((crystal
                 (list ,(cons crystal-ts-mode--list-node-regexp
                             #'crystal-ts-mode--list-p)))))

  ;; Indent: keep your working behavior.
  (when crystal-ts-use-ruby-indent
    (require 'ruby-mode)
    (setq-local ruby-indent-level crystal-ts-indent-offset)
    (setq-local indent-line-function #'ruby-indent-line))

  ;; Font-lock: keep your working behavior.
  (setq-local treesit-font-lock-settings (crystal-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list '((crystal)))
  (treesit-major-mode-setup)

  ;; Folding: keep your working behavior.
  (when (crystal-ts-mode--register-fold-definitions)
    (treesit-fold-mode 1)))

(provide 'crystal-ts-mode)

;;; crystal-ts-mode.el ends here