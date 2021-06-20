# My .emacs.d

The entry is `.emacs`, but where all the magic begins should be `~/.emacs`.

So, you need add following content to ~/.emacs to let all packages start to work.

```el
;;; .emacs --- Where all the magic begins

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defun relative-load (file-relative-path)
  (let ((absolute-path (expand-file-name
                        file-relative-path
                        (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-exists-p absolute-path) (load absolute-path))))

;; Emacs 默认只是在 shell, vterm, shell, term 下面设定这个环境变量.
;; 这里全局增加这个环境变量.
(setenv "INSIDE_EMACS" "true")

(relative-load "Dropbox/common/.emacs.d/set_load_path.el")
(relative-load "Dropbox/common/.emacs.d/.emacs")
```
