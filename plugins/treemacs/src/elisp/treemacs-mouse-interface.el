;; -*- lexical-binding: t; -*-

;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions relating to using the mouse in treemacs.

;; NOTE: This module is lazy-loaded.

;;; Code:

(require 'xref)
(require 'easymenu)
(require 'hl-line)
(require 'treemacs-core-utils)
(require 'treemacs-tags)
(require 'treemacs-scope)
(require 'treemacs-follow-mode)
(require 'treemacs-filewatch-mode)
(require 'treemacs-logging)

(eval-when-compile
  (require 'cl-lib)
  (require 'treemacs-macros))

(treemacs-import-functions-from "treemacs-interface"
  treemacs-add-project-to-workspace)

(defvar treemacs--mouse-project-list-functions
  '(("Add Project.el project" . treemacs--builtin-project-mouse-selection-menu)))

(defun treemacs--mouse-drag-advice (fn &rest args)
  "Advice to wrap `adjust-window-trailing-edge' as FN and its ARGS.
Ensure that treemacs' window width can be changed with the mouse, even if it is
locked."
  (with-selected-window (or (treemacs-get-local-window) (selected-window))
    (let ((treemacs--width-is-locked)
          (window-size-fixed))
      (apply fn args))))

(advice-add #'adjust-window-trailing-edge :around #'treemacs--mouse-drag-advice)

(defun treemacs--builtin-project-mouse-selection-menu ()
  "Build a mouse selection menu for project.el projects."
  (pcase (if (fboundp 'project-known-project-roots)
             (->> (project-known-project-roots)
                  (-map #'treemacs-canonical-path)
                  (-sort #'string<))
           'unavailable)
    (`unavailable
     (list (vector "Project.el api is not available" #'ignore)))
    (`nil
     (list (vector "Project.el list is empty" #'ignore)))
    (projects
     (pcase (--reject (treemacs-is-path it :in-workspace) projects)
       (`nil
        (list (vector "All Project.el projects are alread in the workspace" #'ignore)))
       (candidates
        (--map (vector it (lambda () (interactive) (treemacs-add-project-to-workspace it))) candidates))))))

;;;###autoload
(defun treemacs-leftclick-action (event)
  "Move focus to the clicked line.
Must be bound to a mouse click, or EVENT will not be supplied."
  (interactive "e")
  (when (eq 'down-mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    ;; 7th element is the clicked image
    (when (->> event (cadr) (nth 7))
      (treemacs-do-for-button-state
       :on-file-node-closed (treemacs--expand-file-node btn)
       :on-file-node-open   (treemacs--collapse-file-node btn)
       :on-tag-node-closed  (treemacs--expand-tag-node btn)
       :on-tag-node-open    (treemacs--collapse-tag-node btn)
       :no-error            t))
    (treemacs--evade-image)))

;;;###autoload
(defun treemacs-doubleclick-action (event)
  "Run the appropriate double-click action for the current node.
In the default configuration this means to expand/collapse directories and open
files and tags in the most recently used window.

This function's exact configuration is stored in
`treemacs-doubleclick-actions-config'.

Must be bound to a mouse double click to properly handle a click EVENT."
  (interactive "e")
  (when (eq 'double-mouse-1 (elt event 0))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    (-when-let (state (treemacs--prop-at-point :state))
      (--if-let (cdr (assq state treemacs-doubleclick-actions-config))
          (progn
            (funcall it)
            (treemacs--evade-image))
        (treemacs-pulse-on-failure "No double click action defined for node of type %s."
          (propertize (format "%s" state) 'face 'font-lock-type-face))))))

;;;###autoload
(defun treemacs-single-click-expand-action (event)
  "A modified single-leftclick action that expands the clicked nodes.
Can be bound to <mouse1> if you prefer to expand nodes with a single click
instead of a double click.  Either way it must be bound to a mouse click, or
EVENT will not be supplied.

Clicking on icons will expand a file's tags, just like
`treemacs-leftclick-action'."
  (interactive "e")
  (when (eq 'mouse-1 (elt event 0))
    (select-window (->> event (cadr) (nth 0)))
    (goto-char (posn-point (cadr event)))
    (when (region-active-p)
      (keyboard-quit))
    ;; 7th element is the clicked image
    (if (->> event (cadr) (nth 7))
      (treemacs-do-for-button-state
       :on-file-node-closed (treemacs--expand-file-node btn)
       :on-file-node-open   (treemacs--collapse-file-node btn)
       :on-tag-node-closed  (treemacs--expand-tag-node btn)
       :on-tag-node-open    (treemacs--collapse-tag-node btn)
       :no-error            t)
      (-when-let (state (treemacs--prop-at-point :state))
        (funcall (cdr (assoc state treemacs-doubleclick-actions-config)))))
    (treemacs--evade-image)))

;;;###autoload
(defun treemacs-dragleftclick-action (event)
  "Drag a file/dir node to be opened in a window.
Must be bound to a mouse click, or EVENT will not be supplied."
  (interactive "e")
  (when (eq 'drag-mouse-1 (elt event 0))
    (let* ((info1 (elt (cdr event) 0))
           (info2 (elt (cdr event) 1))
           (source-window (elt info1 0))
           (target-window (elt info2 0))
           (source-pos (elt info1 1))
           (target-pos (elt info2 1))
           (treemacs-buffer (treemacs-get-local-buffer)))
      (if (eq source-window target-window)
          (treemacs--drag-move-files source-pos target-pos)
          (let* ((node (with-current-buffer treemacs-buffer (treemacs-node-at-point)))
                 (path (-some-> node (treemacs-button-get :path))))
            (treemacs-with-path path
              :file-action (progn (select-window target-window)
                                  (find-file path))
              :no-match-action (ignore)))))))

(defun treemacs--drag-move-files (source-pos target-pos)
  "Move files with a mouse-drag action.
SOURCE-POS: Start position of the mouse drag.
TARGET-POS: End position of the mouse drag."
  (let* ((source-btn (treemacs--button-in-line source-pos))
         (target-btn (treemacs--button-in-line target-pos))
         (source-key (-some-> source-btn (treemacs-button-get :key)))
         (target-key (-some-> target-btn (treemacs-button-get :key)))
         (target-dir (and target-key
                          (if (file-directory-p target-key)
                              target-key
                            (treemacs--parent-dir target-key))))
         (target-file (and source-key target-key
                           (treemacs-join-path target-dir (treemacs--filename source-key)))))
    (when (and treemacs-move-files-by-mouse-dragging
               source-key target-key
               (not (string= source-key target-key))
               (not (treemacs-is-path source-key :directly-in target-dir)))
      (treemacs-do-delete-single-node source-key)
      (treemacs--without-filewatch
       (rename-file source-key target-file))
      (run-hook-with-args 'treemacs-copy-file-functions source-key target-dir)
      (treemacs-do-insert-single-node target-file target-dir)
      (treemacs-update-single-file-git-state source-key)
      (treemacs-update-single-file-git-state target-file)
      (treemacs--on-file-deletion source-key)
      (treemacs-goto-file-node target-file)
      (treemacs-pulse-on-success "Moved %s to %s"
        (propertize (treemacs--filename target-file) 'face 'font-lock-string-face)
        (propertize target-dir 'face 'font-lock-string-face)))))

;;;###autoload
(defun treemacs-define-doubleclick-action (state action)
  "Define the behaviour of `treemacs-doubleclick-action'.
Determines that a button with a given STATE should lead to the execution of
ACTION.

The list of possible states can be found in `treemacs-valid-button-states'.
ACTION should be one of the `treemacs-visit-node-*' commands."
  (setf treemacs-doubleclick-actions-config (assq-delete-all state treemacs-doubleclick-actions-config))
  (push (cons state action) treemacs-doubleclick-actions-config))

;;;###autoload
(defun treemacs-node-buffer-and-position (&optional _)
  "Return source buffer or list of buffer and position for the current node.
This information can be used for future display.  Stay in the selected window
and ignore any prefix argument."
  (interactive "P")
  (treemacs-without-messages
    (treemacs--execute-button-action
     :file-action (find-file-noselect (treemacs-safe-button-get btn :path))
     :dir-action (find-file-noselect (treemacs-safe-button-get btn :path))
     :tag-action (treemacs--tag-noselect btn)
     :window (selected-window)
     :window-arg '(4)
     :ensure-window-split nil
     :no-match-explanation "")))

(defun treemacs--imenu-tag-noselect (file tag-path)
  "Return a list of the source buffer for FILE and the tag's from TAG-PATH."
  (let ((tag (-last-item tag-path))
        (path (-butlast tag-path)))
    (condition-case e
        (progn
          (find-file-noselect file)
          (let ((index (treemacs--get-imenu-index file)))
            (dolist (path-item path)
              (setq index (cdr (assoc path-item index))))
            (-let [(buf . pos)
                   (treemacs--extract-position (cdr (--first (equal (car it) tag) index)) path)]
              ;; some imenu implementations, like markdown, will only provide
              ;; a raw buffer position (an int) to move to
	      (list (or buf (get-file-buffer file)) pos))))
      (error
       (treemacs-log-err "Something went wrong when finding tag '%s': %s"
                      (propertize tag 'face 'treemacs-tags-face)
                      e)))))

(defun treemacs--tag-noselect (btn)
  "Return list of tag source buffer and position for BTN for future display."
  (cl-flet ((xref-definition
             (identifier)
             "Return the first definition of string IDENTIFIER."
             (car (xref-backend-definitions (xref-find-backend) identifier)))
            (xref-item-buffer
             (item)
             "Return the buffer in which xref ITEM is defined."
             (marker-buffer (save-excursion (xref-location-marker (xref-item-location item)))))
            (xref-item-position
             (item)
             "Return the buffer position where xref ITEM is defined."
             (marker-position (save-excursion (xref-location-marker (xref-item-location item))))))
    (-let [(tag-buf . tag-pos)
           (treemacs-with-button-buffer btn
             (let ((marker (treemacs-button-get :marker btn))
                   (path (treemacs-button-get :path btn)))
               (treemacs--extract-position marker path)))]
      (if tag-buf
          (list tag-buf tag-pos)
        (pcase treemacs-goto-tag-strategy
          ('refetch-index
           (let (file tag-path)
             (with-current-buffer (marker-buffer btn)
               (setq file (treemacs--nearest-path btn)
                     tag-path (treemacs-button-get btn :path)))
             (treemacs--imenu-tag-noselect file tag-path)))
          ('call-xref
           (let ((xref (xref-definition
                        (treemacs-with-button-buffer btn
                          (treemacs--get-label-of btn)))))
             (when xref
               (list (xref-item-buffer xref) (xref-item-position xref)))))
          ('issue-warning
           (treemacs-log-failure "Tag '%s' is located in a buffer that does not exist."
             (propertize (treemacs-with-button-buffer btn (treemacs--get-label-of btn)) 'face 'treemacs-tags-face)))
          (_ (error "[Treemacs] '%s' is an invalid value for treemacs-goto-tag-strategy" treemacs-goto-tag-strategy)))))))

;;;###autoload
(defun treemacs-rightclick-menu (event)
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

(provide 'treemacs-mouse-interface)

;;; treemacs-mouse-interface.el ends here
