(require 'cfrs)
(require 'treemacs)
(require 'treemacs-file-management)
(require 'treemacs-extensions)

(treemacs-git-mode 'deferred)
(treemacs-filewatch-mode t)
;; (setq treemacs-file-event-delay 1000)

;; we don't need treemacs command, use q to close and exit treemacs.
(global-set-key [(f9)] 'treemacs)
(setq treemacs-show-hidden-files nil) ; 快捷键 th
;; (setq treemacs-width-is-initially-locked nil) ; 快捷键 tw

(treemacs-fringe-indicator-mode 'always)
(treemacs-indent-guide-mode t)
(treemacs-follow-mode t)
(require 'treemacs-project-follow-mode)
(treemacs-project-follow-mode t)
(setq treemacs-is-never-other-window t)
(setq treemacs-silent-refresh    t)
(setq treemacs-silent-filewatch    t)
;; (setq treemacs-position 'right)

(defun treemacs-ignore-example (filename absolute-path)
  (or
   ;; (string-suffix-p ".elc" filename)
   (string-suffix-p "/_build" absolute-path)
   (string-suffix-p "/deps" absolute-path)
   (string-prefix-p filename ".qr_")
   ))

(add-to-list 'treemacs-ignored-file-predicates 'treemacs-ignore-example)

(global-set-key [(control x) (\1)] 'treemacs-delete-other-windows)

(setq treemacs-width 42)

;; (require 'treemacs-icons-dired)
;; ;; 让 dired 使用 treemacs 图标。
;; (treemacs-icons-dired-mode t)

;; 改变图标大小，默认 22.
;; (treemacs-resize-icons 18)

;; (setq treemacs-indentation-string (propertize " ⫶ " 'face 'font-lock-comment-face)
;;       treemacs-indentation 1)

(defun treemacs-bc1-file (&optional arg)
  (interactive "P")
  (run-process "bc1" (treemacs--select-file-from-btn (treemacs-current-button) "bc1: ")))

(defun treemacs-bc2-file (&optional arg)
  (interactive "P")
  (run-process "bc2" (treemacs--select-file-from-btn (treemacs-current-button) "bc2: ")))

(defun treemacs-rightclick-menu-hacked (event)
  "Show a contextual right click menu based on click EVENT."
  (interactive "e")
  (treemacs-without-following
   (unless (eq major-mode 'treemacs-mode)
     ;; no when-let - the window must exist or this function would not be called
     (select-window (treemacs-get-local-window)))
   (goto-char (posn-point (cadr event)))
   (hl-line-highlight)
   ;; need to redisplay manually so hl-line and point move correctly
   ;; and visibly
   (redisplay)
   (cl-labels ((check (value) (not (null value))))
     (let* ((node    (treemacs-node-at-point))
            (state   (-some-> node (treemacs-button-get :state)))
            (project (treemacs-project-at-point))
            (menu
             (easy-menu-create-menu
              nil
              `(["Paste here"
                 treemacs-paste-dir-at-point-to-minibuffer
                 :visible ,(string-match-p "\\(\\(Move\\)\\|\\(Copy\\)\\) to: " (or (minibuffer-prompt) ""))]
                ("New"
                 ["New File"      treemacs-create-file]
                 ["New Directory" treemacs-create-dir])
                ["Open"   treemacs-visit-node-no-split :visible ,(check node)]
                ("Open With" :visible ,(not (null node))
                 ["Open Directly"                    treemacs-visit-node-no-split]
                 ["Open In External Application"     treemacs-visit-node-in-external-application]
                 ["Open With Vertical Split"         treemacs-visit-node-vertical-split]
                 ["Open With Horizontal Split"       treemacs-visit-node-horizontal-split]
                 ["Open With Ace"                    treemacs-visit-node-ace]
                 ["Open With Ace & Vertical Split"   treemacs-visit-node-ace-vertical-split]
                 ["Open With Ace & Horizontal Split" treemacs-visit-node-ace-horizontal-split])
                ["Open Tags"  treemacs-toggle-node :visible ,(check (memq state '(file-node-closed tag-node-closed)))]
                ["Close Tags" treemacs-toggle-node :visible ,(check (memq state '(file-node-open tag-node-open)))]

                ["--" #'ignore                           :visible ,(check node)]
                ["Rename"           treemacs-rename-file :visible ,(check node)]
                ["Delete"           treemacs-delete-file :visible ,(check node)]
                ["Move"             treemacs-move-file   :visible ,(check node)]
                ["bc1"             treemacs-bc1-file   :visible ,(check node)]
                ["bc2"             treemacs-bc2-file   :visible ,(check node)]
                ("Copy"
                 ["Copy File"          treemacs-copy-file                   :visible ,(check node)]
                 ["Copy Absolute Path" treemacs-copy-absolute-path-at-point :visible ,(check node)]
                 ["Copy Relative Path" treemacs-copy-relative-path-at-point :visible ,(check node)]
                 ["Copy Project Path"  treemacs-copy-project-path-at-point  :visible ,(check node)]
                 ["Copy Filename"      treemacs-copy-filename-at-point      :visible ,(check node)])

                ["--" #'ignore t]
                ("Projects"
                 ["Add Project" treemacs-add-project]
                 ,@(--map `(,(car it) ,@(funcall (cdr it)))
                          treemacs--mouse-project-list-functions)
                 ["Remove Project" treemacs-remove-project-from-workspace :visible ,(check project)]
                 ["Rename Project" treemacs-rename-project                :visible ,(check project)])
                ("Workspaces"
                 ["Edit Workspaces"        treemacs-edit-workspaces]
                 ["Create Workspace"       treemacs-create-workspace]
                 ["Remove Workspace"       treemacs-remove-workspace]
                 ["Rename Workspace"       treemacs-rename-workspace]
                 ["Switch Workspace"       treemacs-switch-workspace]
                 ["Set Fallback Workspace" treemacs-set-fallback-workspace])
                ("Toggles"
                 [,(format "Dotfile Visibility (Currently %s)"
                           (if treemacs-show-hidden-files "Enabled" "Disabled"))
                  treemacs-toggle-show-dotfiles]
                 [,(format "Follow-Mode (Currently %s)"
                           (if treemacs-follow-mode "Enabled" "Disabled"))
                  treemacs-follow-mode]
                 [,(format "Filewatch-Mode (Currently %s)"
                           (if treemacs-filewatch-mode "Enabled" "Disabled"))
                  treemacs-filewatch-mode]
                 [,(format "Fringe-Indicator-Mode (Currently %s)"
                           (if treemacs-fringe-indicator-mode "Enabled" "Disabled"))
                  treemacs-fringe-indicator-mode])
                ("Help"
                 ["Show Helpful Hydra"     treemacs-helpful-hydra]
                 ["Show Active Extensions" treemacs-show-extensions]
                 ["Show Changelog"         treemacs-show-changelog]))))
            (choice (x-popup-menu event menu))
            (cmd (lookup-key menu (apply 'vector choice))))
       ;; In the terminal clicking on a nested menu item does not expand it, but actually
       ;; selects it as the chosen use option.  So as a workaround we need to manually go
       ;; through the menus until we land on an executable command.
       (while (and (not (commandp cmd))
                   (not (eq cmd menu)))
         (setf menu choice
               choice (x-popup-menu event cmd)
               cmd (lookup-key cmd (apply 'vector choice))))
       (when (and cmd (commandp cmd))
         (call-interactively cmd))
       (hl-line-highlight)))))

(add-hook 'treemacs-mode-hook
          (lambda ()
            (define-key treemacs-mode-map [(control d)] 'treemacs-remove-project-from-workspace)
            (define-key treemacs-mode-map [mouse-3]         'treemacs-rightclick-menu-hacked)
            ))

(with-eval-after-load 'lsp-mode
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-treemacs-symbols)
  ;; (add-hook 'treemacs-select-hook 'lsp-ui-imenu)
  ;; (add-hook 'treemacs-switch-workspace-hook 'lsp-ui-imenu)
  )


(provide 'treemacs_init)
;;; treemacs_init.el ends here
