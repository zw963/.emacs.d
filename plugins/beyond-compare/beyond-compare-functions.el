(defun bc3-gd1-file-at-point ()
  "git gd1 current file with pointing tag, Use Beyond Compare 3."
  (interactive)
  (cond ((eq major-mode 'mo-git-blame-mode)
         (run-process "gd1"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))
                      ))
        ((eq major-mode 'git-log-view-mode)
         (run-process "gd1"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'vc-annotate-mode)
         (run-process "gd1"
                      (car (vc-annotate-extract-revision-at-line))
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ))

(defun bc3-gitdiff-file-at-point ()
  "Git diff current file with pointing tag, Use Beyond Compare 3."
  (interactive)

  (cond ((eq major-mode 'git-branch-mode)
         (run-process "git-bcompare"
                      (thing-at-point 'symbol)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-log-view-mode)
         (run-process "git-bcompare"
                      (log-view-current-tag)
                      "--"
                      (file-relative-name (buffer-file-name (other-buffer (current-buffer) t)))))
        ((eq major-mode 'git-state-mode)
         (let ((fn (git--status-view-select-filename))
               (stat (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view)))))
           (if (equal stat 'staged)
               ;; INDEX 内将要提交的改变.
               (run-process "git-bcompare" "--cached" "--" fn)
             ;; 提交的改变.
             (run-process "git-bcompare" "--" fn))))
        ))

(defun bc3-gitdiff ()
  "Git diff current file content with INDEX, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "--" (file-relative-name buffer-file-name)))

(defun bc3-gitdiff-head ()
  "Git diff current buffer with HEAD, Use Beyond Compare 3."
  (interactive)
  (git--require-buffer-in-git)
  (run-process "git-bcompare" "HEAD" "--" buffer-file-name))

(defun bc1-current-file ()
  "Setting Beyond Compare first compared file."
  (interactive)
  (run-process "bc1" buffer-file-name))

(defun bc2-current-file ()
  "Setting Beyond Compare second compared file."
  (interactive)
  (run-process "bc2" buffer-file-name))

(defun bc3-autosave-file ()
  "Use Beyond Compare 3 compare with auto-save files, right is newer"
  (interactive)
  (run-process "bcompare" (make-auto-save-file-name) buffer-file-name))

(defun bc3-buffer-with-file ()
  "Use Beyond Compare 3 compare buffer with file, right is newer"
  (interactive)
  (let ((tempfile (make-temp-file "buffer-content-")))
    (write-region nil nil tempfile nil 'nomessage)
    (run-process "bcompare"
                 buffer-file-name
                 tempfile
                 "-lefttitle=File content（Old）"
                 "-righttitle=Buffer content（New）"
                 )))

(defun dired-bc1-current-file ()
  "Setting Beyond Compare first compared file in dired."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "bc1" fn)))

(defun dired-bc2-current-file ()
  "Setting Beyond Compare second compared file in dired."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "bc2" fn)))

(defun dired-cpa-current-file ()
  "run cpa at current file in dired buffer."
  (interactive)
  (let ((fn (dired-get-file-for-visit)))
    (run-process "cpa" fn)))

(defun ibuffer-bc1-current-file ()
  "Setting Beyond Compare first compared file in ibuffer."
  (interactive)
  (let ((fn (buffer-file-name (ibuffer-current-buffer t))))
    (run-process "bc1" fn)))

(defun ibuffer-bc2-current-file ()
  "Setting Beyond Compare second compared file in ibuffer."
  (interactive)
  (let ((fn (buffer-file-name (ibuffer-current-buffer t))))
    (run-process "bc2" fn)))

(defun region-to-file-force (file)
  "Prints string into file, matters not if file exists."
  (write-region (region-beginning) (region-end) file nil nil nil nil))

(defun bc1-current-region ()
  (interactive)
  (region-to-file-force "/tmp/bc3_one")
  (run-process "bc1" "/tmp/bc3_one"))

(defun bc2-current-region ()
  (interactive)
  (region-to-file-force "/tmp/bc3_two")
  (run-process "bc2" "/tmp/bc3_two"))

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

(provide 'beyond-compare-functions)

;;; beyond-compare-functions.el ends here
