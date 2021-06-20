# My .emacs.d

The entry is `.emacs` in this project root folder, but where all the magic begins should be `~/.emacs`.

You need add following content to ~/.emacs to let all packages start to work.

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

# philosophy


1. all `*.el`s in project root folder consider as users common(non-package specified) self-defined config.

2. all required packages live in `plugins/` folder, which was predownloaded manually(not use ELPA).
   almost all package live with a individual config file, e.g. assume one package named `some-package`, 
   it should exists a `some-package_init.el` in same package folder, you can enable/disable those packages
   one by one with in `init.el`. current, there exists 120+ packages in plugins folder.

3. `autoload_plugins/` is some packages which not be used daily, so, those packages is set to autoloadable in autoloads.el. 

4. Run `./update_elc` was used for update all packages elc files, it only need to be run when add a new package, should only works on linux.

Filename still not well organized yet, need improve, but anyway, it works!
