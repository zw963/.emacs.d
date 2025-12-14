;; -*- lexical-binding: t; -*-

(require 'subr-x)   ;; string-trim
(require 'cl-lib)

(defconst my/git-branches-buffer "*git-branches*")

(defvar-local my/git-branches--root nil)
(defvar-local my/git-branches--branches nil)

(defun my/git--call-lines (dir &rest args)
  "Run: git ARGS in DIR, return stdout lines."
  (unless (and dir (stringp dir))
    (user-error "my/git--call-lines: DIR is nil or not a string"))
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (with-temp-buffer
      (let ((rc (apply #'process-file "git" nil t nil args)))
        (unless (eq rc 0)
          (user-error "git %s failed in %s: %s"
                      (string-join args " ")
                      default-directory
                      (string-trim (buffer-string))))
        (split-string (buffer-string) "\n" t)))))

(defun my/git--repo-root (&optional dir)
  "Return git toplevel for DIR (or `default-directory')."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (root (condition-case nil
                   (car (my/git--call-lines dir "rev-parse" "--show-toplevel"))
                 (error nil))))
    (unless (and root (file-directory-p root))
      (user-error "Not inside a Git repository: %s" dir))
    (file-name-as-directory (expand-file-name root))))

(defun my/git--local-branches (root)
  "List local branches (refs/heads)."
  (my/git--call-lines root "branch" "--list"
                      "--format=%(refname:short)" "--sort=refname"))

(defun my/git--current-branch (root)
  "Return current branch short name, or nil if detached."
  (condition-case nil
      (car (my/git--call-lines root "symbolic-ref" "--short" "HEAD"))
    (error nil)))

(defun my/git--branch-hash (root branch)
  "Return full commit hash of BRANCH."
  (car (my/git--call-lines root "rev-parse" "--verify" branch)))

(defun my/git-branches--ensure-root ()
  "Ensure `my/git-branches--root' is non-nil and consistent."
  (let ((root (or my/git-branches--root
                  ;; 如果 buffer-local 没了，就用当前 default-directory 重算
                  (ignore-errors (my/git--repo-root default-directory)))))
    (unless root
      (user-error "Cannot determine git root (are you in a repo?)"))
    (setq my/git-branches--root root)
    ;; 关键：让该 buffer 自己的 default-directory 也跟着 root 走，避免复用时粘住旧项目
    (setq default-directory root)
    root))

(define-derived-mode my/git-branches-mode special-mode "Git-Branches"
  "Minimal local branch list."
  (setq buffer-read-only t)
  (setq truncate-lines t))

(defun my/git-branches-refresh (&optional root)
  "Refresh branch list. ROOT optional."
  (interactive)
  (unless (derived-mode-p 'my/git-branches-mode)
    (user-error "Not in my/git-branches buffer"))
  (let* ((inhibit-read-only t)
         (root (or root (my/git-branches--ensure-root)))
         (branches (my/git--local-branches root))
         (cur (my/git--current-branch root))
         (keep (my/git-branches--branch-at-point t)))
    (setq my/git-branches--branches branches)
    (erase-buffer)
    (insert (format "Repo: %s\n\n" (abbreviate-file-name root)))
    (dolist (b branches)
      (insert (if (and cur (string= b cur)) "* " "  "))
      (insert b)
      (insert "\n"))
    ;; 尽量把光标放回原来的分支
    (goto-char (point-min))
    (forward-line 2)
    (when keep
      (let ((pos (cl-position keep branches :test #'string=)))
        (when pos
          (goto-char (point-min))
          (forward-line (+ 2 pos)))))))

(defun my/git-branches--branch-at-point (&optional noerror)
  "Return branch name at point (current line)."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^[* ] \\(.*\\)$") (match-string 1))
     (noerror nil)
     (t (user-error "No branch on this line")))))

;;; 1) next/prev functions (bind to n/p)
(defun my/git-branches-next ()
  (interactive)
  (unless (derived-mode-p 'my/git-branches-mode)
    (user-error "Not in my/git-branches buffer"))
  (forward-line 1)
  (when (eobp) (forward-line -1))
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2)))

(defun my/git-branches-prev ()
  (interactive)
  (unless (derived-mode-p 'my/git-branches-mode)
    (user-error "Not in my/git-branches buffer"))
  (forward-line -1)
  (when (< (line-number-at-pos) 3)
    (goto-char (point-min))
    (forward-line 2)))

;;; 2) get hash of branch at point
(defun my/git-branches-branch-name (&optional branch)
  "Return commit hash of BRANCH (or branch at point)."
  (interactive)
  (let* ((root (my/git-branches--ensure-root))
         (branch (or branch (my/git-branches--branch-at-point)))
         (hash (my/git--branch-hash root branch)))
    (when (called-interactively-p 'interactive)
      (kill-new hash)
      (message "%s  %s" hash branch))
    branch))

;;; entry
(defun my/git-branches ()
  "Show local branches for the repo of the current buffer."
  (interactive)
  (let* ((root (my/git--repo-root default-directory))
         (buf (get-buffer-create my/git-branches-buffer)))
    (with-current-buffer buf
      (my/git-branches-mode)
      ;; 每次进入都强制更新 root，避免复用粘住旧项目
      (setq my/git-branches--root root)
      (setq default-directory root)
      (let ((map my/git-branches-mode-map))
        (define-key map (kbd "n") #'my/git-branches-next)
        (define-key map (kbd "p") #'my/git-branches-prev)
        (define-key map (kbd "g") #'my/git-branches-refresh)
        (define-key map (kbd "q") #'quit-window))
      (my/git-branches-refresh root))
    (pop-to-buffer buf)))

(define-key my/git-branches-mode-map [(=)] 'bc3-gitdiff-file-at-point)
(global-set-key [(control x) (v) (b)] 'my/git-branches) ;; 在 Emacs 下面显示分支

(provide 'my-git-branch-view-mode)
;;; my-git-branch-view-mode.el ends here
