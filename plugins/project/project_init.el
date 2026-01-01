;; -*- lexical-binding: t; -*-

(require 'project)

;; (project-current) 返回信息。
;; (project-root (project-current)) 返回找到的项目根目录
;; M-: (car (project-current)) RET, 如果是 vc，说明是 Git/VC backend 命中；如果是 transient，说明是你这个函数命中。

;; .git 不需要，因为首先会尝试 project-try-vc
(defcustom project-root-markers '("Cargo.toml" "shard.yml" "Gemfile")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (file-name-concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (let ((path (expand-file-name path)))
    (catch 'found
      (while (not (equal "/" path))
        (if (not (project-root-p path))
            (setq path (file-name-directory (directory-file-name path)))
          (throw 'found (cons 'transient (file-name-as-directory path)))
          )))))

(defun zw/project-root ()
  "Return project root dir or nil."
  (when-let ((proj (project-current nil)))
    (project-root proj)))

;; 加入后默认应该有两个： (project-try-vc project-find-root)
(add-to-list 'project-find-functions #'project-find-root t)

(with-eval-after-load 'lsp-mode
  (setq lsp-auto-guess-root t))

(provide 'project_init)

;;; project_init.el ends here
