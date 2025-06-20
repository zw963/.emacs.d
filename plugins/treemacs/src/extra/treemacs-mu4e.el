;; -*- lexical-binding: t; -*-

;;; treemacs-mu4e.el --- mu4e integration for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((emacs "26.1") (treemacs "0.0") (pfuture "1.7") (dash "2.11.0") (s "1.10.0") (ht "2.2"))
;; Homepage: https://github.com/Alexander-Miller/treemacs
;; Version: 0

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

;; This package creates a thunderbird-inspired sidebar for mu4e using
;; treemacs' treelib api.
;;
;; Some of mu's directories are not part of a maildir hierarchy, but stand at the
;; top alone.  Like in thunderbird, they are grouped in a fake "Local Folders" tree.
;; Since the top of this does not really exist it is sometimes necessary to make
;; the mapping towards the "true" maildir' folder, e.g. when displaying the maildir
;; in mu4e and getting the message counts from python.

;;; Code:

(require 'mu4e)
(require 'pfuture)
(require 'treemacs)
(require 'treemacs-treelib)
(require 's)
(require 'ht)
(require 'dash)

(eval-when-compile
  (require 'cl-lib)
  (require 'inline))

(eval-when-compile
  (cl-declaim (optimize (speed 3) (safety 0))))

;;;;; Customization

(defgroup treemacs-mu4e nil
  "Treemacs+mu4e configuration options."
  :group 'treemacs-mu4e
  :prefix "treemacs-mu4e-")

(defface treemacs-mu4e-mailcount-face
  '((t :inherit font-lock-comment-face))
  "Face for message count annotations."
  :group 'treemacs-mu4e)

(defcustom treemacs-mu4e-local-folders "Local Folders"
  "Name for group of local folders."
  :group 'treemacs-mu4e
  :type 'string)

;;;;; Globals

(defconst treemacs-mu4e--buffer-name " *Treemacs Mu4e*")

(defconst treemacs-mu4e--count-script
  (expand-file-name "src/scripts/treemacs-count-mail.py" treemacs-dir))

(defconst treemacs-mu4e--maildir-map
  (make-hash-table :size 200 :test 'equal)
  "Maps mu's maildir names to maildir objects.")

(defconst treemacs-mu4e--label-map
  (make-hash-table :size 200 :test 'equal)
  "Maps maildir names to their display labels.")

(defconst treemacs-mu4e--weight-map (make-hash-table :size 200 :test 'equal)
  "Maps maildir names to their weights.")

(defvar treemacs-mu4e--mailcount-update-timer nil
  "Timer for debounced the maildir updates.")

(cl-defstruct (treemacs-maildir
               (:conc-name treemacs-maildir->)
               (:constructor treemacs-maildir->create!))
  (mu-dir nil :read-only t)
  label
  weight
  children
  parent)

;;;;; Maildir collection & setup

(define-inline treemacs-maildir->true-folder (self)
  "Get the maildir of SELF, but without the workaround for local folders.
Will replace the folder string's `treemacs-mu4e-local-folders' prefix with just
\"/\" again."
  (declare (side-effect-free t))
  (inline-letevals (self)
    (inline-quote
     (s-replace (concat "/" treemacs-mu4e-local-folders)
                ""
                (treemacs-maildir->mu-dir ,self)))))

(define-inline treemacs-mu4e--get-default-label (maildir)
  "Use the string after the last slash as MAILDIR's default label."
  (declare (pure t) (side-effect-free t))
  (inline-letevals (maildir)
    (inline-quote
     (-last-item (-reject #'s-blank? (s-split "/" ,maildir))))))

(define-inline treemacs-mu4e--maildir-sort-function (m1 m2)
  "Sort M1 and M2 based on their weight values."
  (declare (side-effect-free t))
  (inline-letevals (m1 m2)
    (inline-quote
     (< (treemacs-maildir->weight ,m1)
        (treemacs-maildir->weight ,m2)))))

(defun treemacs-mu4e--collect-maildirs ()
  "Collect all maildirs when this module is first loaded.

Maildir strings will be mapped to maildir objects in
`treemacs-mu4e--maildir-map'.

Local folders (without subdirs) will be collected under the
`treemacs-mu4e-local-folders' prefix.

Label and weight metadata will be sourced from possibly pre-filled
`treemacs-mu4e--label-map' and `treemacs-mu4e--weight-map'."

  (ht-clear! treemacs-mu4e--maildir-map)

  (let ((mu-maildirs (mu4e-get-maildirs)))
    (dolist (mu-maildir mu-maildirs)
      (let* ((is-leaf? (not (s-ends-with? "/" mu-maildir)))
             (strs (cdr (s-split "/" mu-maildir)))
             (strs (if (length= strs 1)
                       (list treemacs-mu4e-local-folders (car strs))
                     strs))
             (max (length strs))
             (steps))
        (dolist (n (number-sequence 1 max))
          (push (format (if (and is-leaf? (= n max))
                            "/%s"
                          "/%s/")
                        (s-join "/" (-take n strs)))
                steps))
        (let ((previous))
          (dolist (step (nreverse steps))
            (let* ((default-label (treemacs-mu4e--get-default-label step))
                   (maildir
                    (or (ht-get treemacs-mu4e--maildir-map step)
                        (-let ((new-maildir
                                (treemacs-maildir->create!
                                 :mu-dir step
                                 :parent previous
                                 :label (ht-get treemacs-mu4e--label-map step default-label)
                                 :weight (ht-get treemacs-mu4e--weight-map step 1000))))
                          (ht-set! treemacs-mu4e--maildir-map step new-maildir)
                          new-maildir))))
              (when previous
                (cl-pushnew maildir (treemacs-maildir->children previous)))
              (setf previous maildir))))))))

;;;;; Treelib setup

(defun treemacs-mu4e--top-level-maildirs-datasource ()
  "Data source for the very first level of maildirs."
  (->> (ht-values treemacs-mu4e--maildir-map)
       (--filter (null (treemacs-maildir->parent it)))
       (-sort #'treemacs-mu4e--maildir-sort-function)))

(defun treemacs-mu4e--child-maidirs-datasource (btn)
  "Data source for maildirs whose direct parent is BTN."
  (-let [parent-dir (treemacs-maildir->mu-dir (treemacs-button-get btn :maildir))]
    (->> (ht-get treemacs-mu4e--maildir-map parent-dir)
         (treemacs-maildir->children)
         (-sort #'treemacs-mu4e--maildir-sort-function))))

(defun treemacs-mu4e--visit-maildir (&optional _arg)
  "Open the maildir at point in mu4e."
  (-> (treemacs-current-button)
      (treemacs-button-get :maildir)
      (treemacs-maildir->true-folder)
      (mu4e~headers-jump-to-maildir))
  (select-window
   (--first (eq 'mu4e-headers-mode
                (buffer-local-value 'major-mode (window-buffer it)))
            (window-list))))

(treemacs-define-variadic-entry-node-type mu4e-top-maildirs
  :key 'treemacs-mu4e
  :children (treemacs-mu4e--top-level-maildirs-datasource)
  :child-type 'mu4e-maildir)

(treemacs-define-expandable-node-type mu4e-maildir
  :open-icon
  (cond
   ((null (treemacs-maildir->parent item))
    (treemacs-get-icon-value 'inbox))
   ((treemacs-maildir->children item)
    (treemacs-get-icon-value 'mail-plus))
   (t
    (treemacs-get-icon-value 'mail)))
  :closed-icon
  (cond
   ((null (treemacs-maildir->parent item))
    (treemacs-get-icon-value 'mail-inbox))
   ((treemacs-maildir->children item)
    (treemacs-get-icon-value 'mail-plus))
   (t
    (treemacs-get-icon-value 'mail)))
  :ret-action #'treemacs-mu4e--visit-maildir
  :label
  (if (treemacs-maildir->parent item)
      (propertize (treemacs-maildir->label item)
                  'face 'treemacs-directory-face)
    (propertize (treemacs-maildir->label item)
                'face 'treemacs-root-face))
  :key (treemacs-maildir->mu-dir item)
  :children (treemacs-mu4e--child-maidirs-datasource btn)
  :more-properties
  `(:maildir ,item
             :default-face
             ,(if (treemacs-maildir->parent item)
                  'treemacs-directory-face
                'treemacs-root-face)
             ,@(unless (treemacs-maildir->children item)
                 '(:leaf t)))
  :child-type 'mu4e-maildir)

;;;;; Configuration setup

(defun treemacs-mu4e-define-aliases (&rest pairs)
  "Define a set of display name PAIRS.
The first item is the actual name of the maildir, the second item is how it
should be displayed.

\(fn [MAILDIR DISPLAY-LABEL]...)"
  (treemacs-static-assert (= 0 (% (length pairs) 2))
    "Uneven number of name items.")
  (while pairs
    (let* ((name  (pop pairs))
           (label (pop pairs)))
      (ht-set! treemacs-mu4e--label-map name label)
      (--when-let (ht-get treemacs-mu4e--maildir-map name)
        (setf (treemacs-maildir->label it) label)))))

(defun treemacs-mu4e-define-weights (&rest pairs)
  "Define set of display weight PAIRS.
The first item is the actual name of the maildir, the second item is its sorting
weight in the view.  Higher values are sorted later.  The default weight is 100.

\(fn [MAILDIR WEIGHT]...)"
  (treemacs-static-assert (= 0 (% (length pairs) 2))
    "Uneven number of name items.")
  (while pairs
    (let* ((name   (pop pairs))
           (weight (pop pairs)))
      (ht-set! treemacs-mu4e--weight-map name weight)
      (--when-let (ht-get treemacs-mu4e--maildir-map name)
        (setf (treemacs-maildir->weight it) weight)))))

;;;;; Interactive commands

(defun treemacs-mu4e-print-all-maildirs ()
  "Provides an overview of all maildirs and their configuration.
This might be useful when filling up setting up `treemacs-mu4e-define-aliases'
and `treemacs-mu4e-define-weights'."
  (interactive)
  (pop-to-buffer (get-buffer-create "Treemacs Mu4e Maildirs"))
  (erase-buffer)
  (org-mode)
  (setq-local org-hide-emphasis-markers nil)
  (let ((roots (--filter
                (null (treemacs-maildir->parent it))
                (ht-values treemacs-mu4e--maildir-map))))
    (dolist (root roots)
      (treemacs-mu4e--print-maildir root 1)))
  (goto-char 0))

(defun treemacs-mu4e--print-maildir (maildir d)
  "Print MAILDIR at depth D as an org sub-tree."
  (insert (format
           "%s %s\n- label :: %s\n- weight :: %s\n"
           (make-string d ?*)
           (treemacs-maildir->mu-dir maildir)
           (treemacs-maildir->label maildir)
           (treemacs-maildir->weight maildir) ))
  (dolist (child (-sort #'treemacs-mu4e--maildir-sort-function
                          (treemacs-maildir->children maildir)))
    (treemacs-mu4e--print-maildir child (1+ d))))

;;;###autoload
(defun treemacs-mu4e ()
  "Select or display the Mu4e side-bar."
  (interactive)
  (--if-let (get-buffer-window treemacs-mu4e--buffer-name)
      (select-window it)
    (-let [start-fn
           (lambda ()
             (when (ht-empty? treemacs-mu4e--maildir-map)
               (treemacs-mu4e--collect-maildirs))
             (treemacs-mu4e--display))]
      (if (ignore-errors (mu4e-root-maildir))
          (funcall start-fn)
        (mu4e--init-handlers)
        (mu4e--start start-fn)))))

(defun treemacs-mu4e--display ()
  "Display the mu4e buffer in a side-window."
  (--when-let (get-buffer treemacs-mu4e--buffer-name) (kill-buffer it))
  (let* ((buf (get-buffer-create treemacs-mu4e--buffer-name))
         (window (display-buffer-in-side-window buf `((side . ,treemacs-position) (slot . 1)))))
    (select-window window)
    (treemacs-initialize mu4e-top-maildirs
      :with-expand-depth 1
      :and-do (setq-local treemacs-space-between-root-nodes t))
    (treemacs-mu4e--update-mailcounts)
    (treemacs--evade-image)))

;;;;; Async Mailcount

(defun treemacs-mu4e--update-mailcounts (&rest _)
  "Shell out to mu to update the message counts and redraw them."
  (treemacs-debounce treemacs-mu4e--mailcount-update-timer 3
    (-let [maildirs (-map #'treemacs-maildir->mu-dir
                          (ht-values treemacs-mu4e--maildir-map))]
      (pfuture-callback `("python"
                          "-O" "-S"
                          ,treemacs-mu4e--count-script
                          ,treemacs-mu4e-local-folders
                          ,@maildirs)
        :on-error
        (treemacs-log-failure "Mail count update error: %s" (pfuture-callback-output))
        :on-success
        (-let [source "treemacs-mu4e-mailcount"]
          (treemacs-clear-annotation-suffixes source)
          (pcase-dolist (`(,path ,suffix) (read (pfuture-callback-output)))
            (put-text-property 0 (length suffix) 'face 'treemacs-mu4e-mailcount-face suffix)
            (treemacs-set-annotation-suffix
             path suffix source)
            (-let [buffer (get-buffer treemacs-mu4e--buffer-name)]
              (when (buffer-live-p buffer)
                (treemacs-apply-annotations-in-buffer buffer)))))))))

(add-hook 'mu4e-index-updated-hook #'treemacs-mu4e--update-mailcounts)
(add-hook 'mu4e-message-changed-hook #'treemacs-mu4e--update-mailcounts)

(with-no-warnings
  (with-eval-after-load 'winum
    (add-to-list 'winum-ignored-buffers treemacs-mu4e--buffer-name)))

(provide 'treemacs-mu4e)

;;; treemacs-mu4e.el ends here
