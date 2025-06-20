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

;; Persistence of treemacs' workspaces into an org-mode compatible file.

;;; Code:

(require 's)
(require 'dash)
(require 'treemacs-workspaces)
(require 'treemacs-customization)
(require 'treemacs-logging)

(eval-when-compile
  (require 'rx)
  (require 'cl-lib)
  (require 'inline)
  (require 'treemacs-macros))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

(defconst treemacs--org-edit-buffer-name "*Edit Treemacs Workspaces*"
  "The name of the buffer used to edit treemacs' workspace.")

(defconst treemacs--last-error-persist-file
  (treemacs-join-path user-emacs-directory ".cache" "treemacs-persist-at-last-error")
  "File that stores the treemacs state as it was during the last load error.")

(make-obsolete-variable 'treemacs--last-error-persist-file 'treemacs-last-error-persist-file "v2.7")

(defconst treemacs--persist-kv-regex
  (rx bol (? " ") "- path :: " (1+ any) eol)
  "The regular expression to match org's \"key :: value\" lines.")

(defconst treemacs--persist-workspace-name-regex
  (rx bol "* " (1+ any) eol)
  "The regular expression to match lines with workspace names.")

(defconst treemacs--persist-project-name-regex
  (rx bol "** " (1+ any) eol)
  "The regular expression to match lines with projects names.")

(cl-defstruct (treemacs-iter
               (:conc-name treemacs-iter->)
               (:constructor treemacs-iter->create!))
  list)

(defvar treemacs--no-abbr-on-persist-prefixes nil
  "Prefixes for paths to be saved as is, without using `abbreviate-file-name'.
Will be set to all the `tramp-methods', after tramp has been loaded.")

(define-inline treemacs-iter->next! (self)
  "Get the next element of iterator SELF.

SELF: Treemacs-Iter struct."
  (inline-letevals (self)
    (inline-quote
     (let ((head (car (treemacs-iter->list ,self)))
           (tail (cdr (treemacs-iter->list ,self))))
       (setf (treemacs-iter->list ,self) tail)
       head))))

(define-inline treemacs-iter->peek (self)
  "Peek at the first element of SELF.

SELF: Treemacs-Iter struct."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (or (car (treemacs-iter->list ,self))
         ;; we still need something to make the `s-matches?' calls work
         "__EMPTY__"))))

(define-inline treemacs--should-not-run-persistence? ()
  "No saving and loading in noninteractive and CI environments."
  (inline-quote (or noninteractive (getenv "CI") (null treemacs-persist-file))))

(defun treemacs--read-workspaces (iter)
  "Read a list of workspaces from the lines in ITER.

Returns a list with 2 elements: the first one is a list of all enabled
workspaces, the second is a list of all non-disabled workspaces.

ITER: Treemacs-Iter Struct."
  (let ((workspaces)
        (comment-prefix "COMMENT "))
    (while (s-matches? treemacs--persist-workspace-name-regex (treemacs-iter->peek iter))
      (let ((workspace (treemacs-workspace->create!))
            (workspace-name (substring (treemacs-iter->next! iter) 2))
            (workspace-projects (treemacs--read-projects iter)))
        (when (s-starts-with? comment-prefix workspace-name)
          (setf workspace-name (substring workspace-name (length comment-prefix))
                (treemacs-workspace->is-disabled? workspace) t))
        (setf (treemacs-workspace->name workspace) workspace-name
              (treemacs-workspace->projects workspace) workspace-projects)
        (push workspace workspaces)))
    (--separate (treemacs-workspace->is-disabled? it)
                (nreverse workspaces))))

(defun treemacs--read-projects (iter)
  "Read a list of projects from ITER until another section is found.

ITER: Treemacs-Iter Struct"
  (let (projects)
    (while (s-matches? treemacs--persist-project-name-regex (treemacs-iter->peek iter))
      (let ((kv-lines nil)
            (project (treemacs-project->create!))
            (project-name (substring (treemacs-iter->next! iter) 3))
            (comment-prefix "COMMENT "))
        (when (s-starts-with? comment-prefix project-name)
          (setf project-name (substring project-name (length comment-prefix))
                (treemacs-project->is-disabled? project) t))
        (setf (treemacs-project->name project) project-name)
        (while (s-matches? treemacs--persist-kv-regex (treemacs-iter->peek iter))
          (push (treemacs-iter->next! iter) kv-lines))
        (if (null kv-lines)
            (treemacs-log-failure "Project %s has no path and will be ignored."
              (propertize (treemacs-project->name project)
                          'face 'font-lock-type-face))
          (dolist (kv-line kv-lines)
            (-let [(key val) (s-split " :: " kv-line)]
              (pcase (s-trim key)
                ("- path"
                 (setf (treemacs-project->path project) (treemacs-canonical-path val)))
                (_
                 (treemacs-log-failure "Encountered unknown project key-value in line [%s]" kv-line)))))
          (let ((action 'retry))
            (while (eq action 'retry)
              (setf (treemacs-project->path-status project)
                    (-> (treemacs-project->path project)
                        (treemacs--get-path-status)))
              (setq action
                    (cond
                     ((or (treemacs-project->is-disabled? project)
                          (not (treemacs-project->is-unreadable? project)))
                      'keep)
                     ((eq treemacs-missing-project-action 'ask)
                      (let ((completions
                             '(("Keep the project in the project list" . keep)
                               ("Retry" . retry)
                               ("Remove the project from the project list" . remove))))
                        (cdr (assoc (completing-read
                                     (format "Project %s at %s cannot be read."
                                             (treemacs-project->name project)
                                             (treemacs-project->path project))
                                     completions nil t)
                                    completions))))
                     (treemacs-missing-project-action))))
            (if (eq action 'remove)
                (treemacs-log-failure "The location of project %s at %s cannot be read. Project was removed from the project list."
                  (propertize (treemacs-project->name project) 'face 'font-lock-type-face)
                  (propertize (treemacs-project->path project) 'face 'font-lock-string-face))
              (push project projects))))))
    (nreverse projects)))

(defun treemacs--persist ()
  "Persist treemacs' state in `treemacs-persist-file'."
  (unless (or (treemacs--should-not-run-persistence?)
              (null (get 'treemacs :state-is-restored)))
    (unless (file-exists-p treemacs-persist-file)
      (make-directory (file-name-directory treemacs-persist-file) :with-parents))
    (condition-case e
        (let ((txt nil)
              (buffer nil)
              (no-kill nil)
              ;; no surprisese when using `abbreviate-file-name'
              (directory-abbrev-alist nil)
              (abbreviated-home-dir nil)
              (file-precious-flag t))
          (--if-let (get-file-buffer treemacs-persist-file)
              (setq buffer it
                    no-kill t)
            (setq buffer (find-file-noselect treemacs-persist-file :no-warn)
                  desktop-save-buffer nil))
          (with-current-buffer buffer
            (dolist (ws (--reject (null (treemacs-workspace->projects it))
                                  (append (treemacs-workspaces)
                                          (treemacs-disabled-workspaces))))
              (push (format "* %s%s\n"
                            (if (treemacs-workspace->is-disabled? ws) "COMMENT " "")
                            (treemacs-workspace->name ws))
                    txt)
              (dolist (pr (treemacs-workspace->projects ws))
                (push (format "** %s%s\n"
                              (if (treemacs-project->is-disabled? pr) "COMMENT " "")
                              (treemacs-project->name pr))
                      txt)
                (push (format
                       " - path :: %s\n"
                       (-let [path (treemacs-project->path pr)]
                         (if (--any? (string-prefix-p it path) treemacs--no-abbr-on-persist-prefixes)
                             path
                           (abbreviate-file-name path))))
                      txt)))
            (delete-region (point-min) (point-max))
            (insert (apply #'concat (nreverse txt)))
            (-let [inhibit-message t] (save-buffer))
            (unless no-kill (kill-buffer))))
      (error (treemacs-log-err "Error '%s' when persisting workspace." e)))))

(defun treemacs--read-persist-lines (&optional txt)
  "Read the relevant lines from given TXT or `treemacs-persist-file'.
Will read all lines, except those that start with # or contain only whitespace."
  (-some->> (or txt (when (file-exists-p treemacs-persist-file)
                      (with-temp-buffer
                        (insert-file-contents treemacs-persist-file)
                        (buffer-string))))
            (s-trim)
            (s-lines)
            (--reject (or (s-blank-str? it)
                          (s-starts-with? "#" it)))))

(cl-defun treemacs--validate-persist-lines
    (lines
     &optional
     (context :start)
     (prev nil)
     (paths nil)
     (proj-count 0)
     (ws-count 0))
  "Recursively verify the make-up of the given LINES, based on their CONTEXT.
Lines must start with a workspace name, followed by a project name, followed by
the project's path property, followed by either the next project or the next
workspace.

The previously looked at line type is given by CONTEXT.

The previously looked at line is given by PREV.

PATHS contains all the project paths previously seen in the current workspace.
These are used to make sure that no file path appears in the workspaces more
than once.

PROJ-COUNT counts the number of non-disabled projects in a workspace to make
sure that there is at least one project that will be displayed.

WS-COUNT counts the number of non-disabled workspaces to make sure that there is
at least one workspace that will be used.

A successful validation returns just the symbol \\='success, in case of an error
a list of 3 items is returned: the symbol \\='error, the exact line where the
error happened, and the error message.  In some circumstances (for example when
a project is missing a path property) it makes sense to display the error not in
the currently looked at line, but the one above, which is why the previously
looked at line PREV is given as well.

LINES: List of Strings
CONTEXT: Keyword
PREV: String
PATHS: List<String>
PROJ-COUNT: Int"
  (treemacs-block
   (cl-labels ((as-warning (txt) (propertize txt 'face 'warning)))
     (treemacs-unless-let (line (car lines))
         (pcase context
           (:property
            (treemacs-return-if (= 0 proj-count)
              `(error ,prev ,(as-warning "Workspace must contain at least 1 project that is not disabled.")))
            (treemacs-return-if (= 0 ws-count)
              `(error ,prev ,(as-warning "There must be at least 1 worspace that is not disabled.")))
            (treemacs-return
             'success))
           (:start
            (treemacs-return
             (list 'error :start (as-warning "Input is empty"))))
           (_
            (treemacs-return
             (list 'error prev (as-warning "Cannot end with a project or workspace name")))))
       (pcase context
         (:start
          (treemacs-return-if (not (s-matches? treemacs--persist-workspace-name-regex line))
            `(error ,line ,(as-warning "First item must be a workspace name")))
          (-let [ws-is-disabled? (s-starts-with? "* COMMENT" line)]
            (unless ws-is-disabled? (cl-incf ws-count)))
          (treemacs--validate-persist-lines (cdr lines) :workspace line nil 0 ws-count))
         (:workspace
          (treemacs-return-if (not (s-matches? treemacs--persist-project-name-regex line))
            `(error ,line ,(as-warning "Workspace name must be followed by project name")))
          (-let [proj-is-disabled? (s-starts-with? "** COMMENT" line)]
            (unless proj-is-disabled? (cl-incf proj-count))
            (treemacs--validate-persist-lines (cdr lines) :project line nil proj-count ws-count)))
         (:project
          (treemacs-return-if (not (s-matches? treemacs--persist-kv-regex line))
            `(error ,prev ,(as-warning "Project name must be followed by path declaration")))
          (-let [path (cadr (s-split " :: " line))]
            ;; Path not existing is only a hard error when org-editing, when loading on boot
            ;; its significance is determined by the customization setting
            ;; `treemacs-missing-project-action'. Remote files are skipped to avoid opening
            ;; Tramp connections.
            (treemacs-return-if (and (string= treemacs--org-edit-buffer-name (buffer-name))
                                     (not (s-starts-with? "** COMMENT" prev))
                                     (not (file-remote-p path))
                                     (not (file-exists-p path)))
              `(error ,line ,(format (as-warning "File '%s' does not exist") (propertize path 'face 'font-lock-string-face))))
            (treemacs-return-if (or (--any (treemacs-is-path path :in it) paths)
                                    (--any (treemacs-is-path it :in path) paths))
              `(error ,line ,(format (as-warning "Path '%s' appears in the workspace more than once.")
                                     (propertize path 'face 'font-lock-string-face))))
            (treemacs--validate-persist-lines (cdr lines) :property line (cons path paths) proj-count ws-count)))
         (:property
          (let ((line-is-workspace-name (s-matches? treemacs--persist-workspace-name-regex line))
                (line-is-project-name   (s-matches? treemacs--persist-project-name-regex line)))
            (cond
             (line-is-workspace-name
              (treemacs-return-if (= 0 proj-count)
                `(error ,prev ,(as-warning "Workspace must contain at least 1 project that is not disabled.")))
              (-let [ws-is-disabled? (s-starts-with? "* COMMENT" line)]
                (unless ws-is-disabled? (cl-incf ws-count)))
              (treemacs--validate-persist-lines (cdr lines) :workspace line nil 0 ws-count))
             (line-is-project-name
              (-let [proj-is-disabled? (s-starts-with? "** COMMENT" line)]
                (unless proj-is-disabled? (cl-incf proj-count)))
                (treemacs--validate-persist-lines (cdr lines) :project line paths proj-count ws-count))
             (t
              (treemacs-return-if (-none? #'identity (list line-is-workspace-name line-is-project-name))
                `(error ,prev ,(as-warning "Path property must be followed by the next workspace or project"))))))))))))

(defun treemacs--restore ()
  "Restore treemacs' state from `treemacs-persist-file'."
  (unless (treemacs--should-not-run-persistence?)
    (treemacs-unless-let (lines (treemacs--read-persist-lines))
        (setf treemacs--workspaces (list (treemacs-workspace->create! :name "Default"))
              (treemacs-current-workspace) (car treemacs--workspaces))
      ;; Don't persist during restore. Otherwise, if the user would quit
      ;; Emacs during restore, for example during the completing read for
      ;; missing project action, the whole persist file would be emptied.
      (let ((kill-emacs-hook (remq #'treemacs--persist kill-emacs-hook)))
        ;; run in a temp buffer since validation and read functions rely on elisp-based syntax tables
        ;; for their regexes
        (with-temp-buffer
          (condition-case e
              (pcase (treemacs--validate-persist-lines lines)
                ('success
                 (let* ((ws-lists (treemacs--read-workspaces (treemacs-iter->create! :list lines))))
                   (setf treemacs--disabled-workspaces (car ws-lists))
                   (setf treemacs--workspaces (cadr ws-lists))))
                (`(error ,line ,error-msg)
                 (treemacs--write-error-persist-state lines (format "'%s' in line '%s'" error-msg line))
                 (treemacs-log-err "Could not restore saved state, %s:\n%s\n%s"
                   (pcase line
                     (:start "found error in the first line")
                     (:end "found error in the last line")
                     (other (format "found error in line '%s'" other)))
                   error-msg
                   (format "Broken state was saved to %s"
                           (propertize treemacs-last-error-persist-file 'face 'font-lock-string-face)))))
            (error
             (progn
               (treemacs--write-error-persist-state lines e)
               (treemacs-log-err "Error '%s' when loading the persisted workspace.\n%s"
                 e
                 (format "Broken state was saved to %s"
                         (propertize treemacs-last-error-persist-file 'face 'font-lock-string-face)))))))))))

(define-inline treemacs--maybe-load-workspaces ()
  "First load of the workspaces, if it hasn't happened already."
  (inline-quote
   (unless (get 'treemacs :state-is-restored)
     (treemacs--restore)
     (put 'treemacs :state-is-restored t))))

(defun treemacs--write-error-persist-state (lines error)
  "Write broken state LINES and ERROR to `treemacs-last-error-persist-file'."
  (-let [txt (concat (format "# State when last error occurred on %s\n" (format-time-string "%F %T"))
                     (format "# Error was %s\n\n" error)
                     (apply #'concat (--map (concat it "\n") lines)))]
    (unless (file-exists-p treemacs-last-error-persist-file)
      (make-directory (file-name-directory treemacs-last-error-persist-file) :with-parents))
    (write-region txt nil treemacs-last-error-persist-file nil :silent)))

(add-hook 'kill-emacs-hook #'treemacs--persist)

(provide 'treemacs-persistence)

;;; treemacs-persistence.el ends here
