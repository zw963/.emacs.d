;; -*- lexical-binding: t; -*-

;;; helm-swoop.el --- Efficiently hopping squeezed lines powered by helm interface -*- lexical-binding: t -*-

;; Copyright (C) 2019  Emacsorphanage Community
;; Copyright (C) 2013-2018  Shingo Fukuyama

;; Version: 3.0.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/emacsorphanage/helm-swoop
;; Created: Oct 24 2013
;; Keywords: convenience, helm, swoop, inner, buffer, search
;; Package-Requires: ((emacs "25.1") (helm "3.6"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; List the all lines to another buffer, which is able to squeeze
;; by any words you input.  At the same time, the original buffer's
;; cursor is jumping line to line according to moving up and down
;; the list.

;; Example config
;; ----------------------------------------------------------------
;; ;; helm from https://github.com/emacs-helm/helm
;; (require 'helm)

;; ;; Locate the helm-swoop folder to your path
;; ;; This line is unnecessary if you get this program from MELPA
;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-swoop")

;; (require 'helm-swoop)

;; ;; Change keybinds to whatever you like :)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; ;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; ;; Save buffer when helm-multi-swoop-edit complete
;; (setq helm-multi-swoop-edit-save t)

;; ;; If this value is t, split window inside the current window
;; (setq helm-swoop-split-with-multiple-windows nil)

;; ;; Split direction.  'split-window-vertically or 'split-window-horizontally
;; (setq helm-swoop-split-direction 'split-window-vertically)

;; ;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
;; (setq helm-swoop-move-to-line-cycle t)

;; ;; Optional face for line numbers
;; ;; Face name is `helm-swoop-line-number-face`
;; (setq helm-swoop-use-line-number-face t)

;; ----------------------------------------------------------------

;; * `M-x helm-swoop` when region active
;; * `M-x helm-swoop` when the cursor is at any symbol
;; * `M-x helm-swoop` when the cursor is not at any symbol
;; * `M-3 M-x helm-swoop` or `C-u 5 M-x helm-swoop` multi separated line culling
;; * `M-x helm-multi-swoop` multi-occur like feature
;; * `M-x helm-multi-swoop-all` apply all buffers
;; * `C-u M-x helm-multi-swoop` apply last selected buffers from the second time
;; * `M-x helm-swoop-same-face-at-point` list lines have the same face at the cursor is on
;; * During isearch `M-i` to hand the word over to helm-swoop
;; * During helm-swoop `M-i` to hand the word over to helm-multi-swoop-all
;; * While doing `helm-swoop` press `C-c C-e` to edit mode, apply changes to original buffer by `C-x C-s`

;; Helm Swoop Edit
;; While doing helm-swoop, press keybind [C-c C-e] to move to edit buffer.
;; Edit the list and apply by [C-x C-s].  If you'd like to cancel, [C-c C-g]

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)
(require 'helm-grep)

(declare-function migemo-search-pattern-get "ext:migemo")
(declare-function migemo-forward "ext:migemo")
(declare-function projectile-buffers-with-file-or-process "ext:projectile")
(declare-function projectile-project-buffers "ext:projectile")
(defvar projectile-buffers-filter-function)

;;; @ helm-swoop ----------------------------------------------

(defgroup helm-swoop nil
  "Open helm-swoop."
  :prefix "helm-swoop-" :group 'helm)

(defface helm-swoop-target-line-face
  '((t (:background "#e3e300" :foreground "#222222")))
  "Face for helm-swoop target line"
  :group 'helm-swoop)
(defface helm-swoop-target-line-block-face
  '((t (:background "#cccc00" :foreground "#222222")))
  "Face for target line"
  :group 'helm-swoop)
(defface helm-swoop-target-word-face
  '((t (:background "#7700ff" :foreground "#ffffff")))
  "Face for target word"
  :group 'helm-swoop)
(defface helm-swoop-line-number-face
  '((t (:foreground "#999999")))
  "Face for line numbers"
  :group 'helm-swoop)

(defcustom helm-swoop-speed-or-color nil
  "If nil, you can slightly boost invoke speed in exchange for text color."
  :group 'helm-swoop :type 'boolean)
(defcustom helm-swoop-use-line-number-face nil
  "Use face to line numbers on helm-swoop buffer."
  :group 'helm-swoop :type 'boolean)
(defcustom helm-swoop-split-with-multiple-windows nil
  "Split window when having multiple windows open."
  :group 'helm-swoop :type 'boolean)
(defcustom helm-swoop-move-to-line-cycle t
  "Return to the opposite side of line."
  :group 'helm-swoop :type 'boolean)
(defcustom helm-swoop-split-direction 'split-window-vertically
  "Split direction."
  :type '(choice (const :tag "vertically"   split-window-vertically)
                 (const :tag "horizontally" split-window-horizontally))
  :group 'helm-swoop)
(defcustom helm-swoop-use-fuzzy-match nil
  "If t, use fuzzy matching functions as well as exact matches."
  :group 'helm-swoop :type 'boolean)
(defcustom helm-swoop-min-overlay-length 2
  "Minimum pattern length before applying the overlay on the matched word."
  :group 'helm-swoop :type 'integer)

(defvar helm-swoop-split-window-function
  (lambda (buf &rest _args)
    (if helm-swoop-split-with-multiple-windows
        (funcall helm-swoop-split-direction)
      (when (one-window-p)
        (funcall helm-swoop-split-direction)))
    (other-window 1)
    (switch-to-buffer buf))
  "Change the way to split window only when `helm-swoop' is calling.")

(defcustom helm-swoop-after-goto-line-action-hook nil
  "The hook run after `helm-swoop--goto-line."
  :group 'helm-swoop
  :type 'hook)
(defcustom helm-swoop-before-goto-line-action-hook nil
  "The hook run before `helm-swoop--goto-line."
  :group 'helm-swoop
  :type 'hook)

(defcustom helm-swoop-flash-region-function 'helm-swoop-flash-word
  "The function used to flash the result when a search done."
  :group 'helm-swoop
  :type '(choice
          (const :tag "Default highlight function" helm-swoop-flash-word)
          (const :tag "Pulse highlight function" pulse-momentary-highlight-region)
          function))

(defcustom helm-swoop-fontify-buffer-size-limit 100000
  "If buffer size smaller than value, do fontify to make buffer highlighted.

If value is symbol `always', always do fontify."
  :group 'helm-swoop
  :type '(choice
          integer
          (const :tag "Always Fontify" always)))

(defvar helm-swoop-candidate-number-limit 19999)
(defvar helm-swoop-buffer "*Helm Swoop*")
(defvar helm-swoop-prompt "Swoop: ")
(defvar helm-swoop-last-point nil)
(defvar helm-swoop-invisible-targets nil)
(defvar helm-swoop-last-line-info nil)

;; Buffer local variables
(defvar helm-swoop-list-cache)
(defvar helm-swoop-pattern nil)            ; Keep helm-pattern value
(defvar helm-swoop-last-query nil)         ; Last search query for resume
(defvar-local helm-swoop-last-prefix-number 1) ; For multiline highlight

;; Global variables
(defvar helm-swoop-synchronizing-window nil
  "Window object where `helm-swoop' called from.")
(defvar helm-swoop-target-buffer nil
  "Buffer object where `helm-swoop' called from.")
(defvar helm-swoop-line-overlay nil
  "Overlay object to indicate other window's line.")

(defvar helm-swoop--basic-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (dolist (action '(next-line previous-line next-page previous-page
                                beginning-of-buffer end-of-buffer toggle-visible-mark))
      (let ((orig-fn (intern (format "helm-%s" action)))
            (new-fn (intern (format "helm-swoop-%s" action))))
        (defalias new-fn `(lambda (&optional arg)
                            (interactive "p")
                            (call-interactively ',orig-fn)
                            (helm-swoop--move-line-action))
          (format "Replacement of `%s' action for `helm-swoop'.

\(fn ARG)" orig-fn))
        (define-key map `[remap ,orig-fn] new-fn)))
    map))

(defvar helm-swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-swoop--basic-map)
    (define-key map (kbd "C-c C-e") 'helm-swoop-edit)
    (define-key map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    (define-key map (kbd "C-w") 'helm-swoop-yank-thing-at-point)
    map)
  "Keymap for helm-swoop.")

(defvar helm-multi-swoop--basic-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (dolist (action '(next-line previous-line next-page previous-page
                                beginning-of-buffer end-of-buffer toggle-visible-mark))
      (let ((orig-fn (intern (format "helm-%s" action)))
            (new-fn (intern (format "helm-multi-swoop-%s" action))))
        (defalias new-fn `(lambda (&optional arg)
                            (interactive "p")
                            (call-interactively ',orig-fn)
                            (helm-multi-swoop--move-line-action))
          (format "Replacement of `%s' action for `helm-multi-swoop'.

\(fn ARG)" orig-fn))
        (define-key map `[remap ,orig-fn] new-fn)))
    map))

(defvar helm-multi-swoop-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-multi-swoop--basic-map)
    (define-key map (kbd "C-c C-e") 'helm-multi-swoop-edit)
    map))

(defvar helm-c-source-swoop-match-functions
  '(helm-mm-exact-match
    helm-mm-match
    helm-mm-3-migemo-match))

(defvar helm-c-source-swoop-search-functions
  '(helm-mm-exact-search
    helm-mm-search
    helm-candidates-in-buffer-search-default-fn
    helm-mm-3-migemo-search))

(defvar helm-swoop-font-lock-exclude
  '(Man-mode
    adoc-mode
    bbdb-mode
    bongo-library-mode
    bongo-mode
    bongo-playlist-mode
    bookmark-bmenu-mode
    circe-channel-mode
    circe-query-mode
    circe-server-mode
    deadgrep-mode
    debbugs-gnu-mode
    dired-mode
    elfeed-search-mode
    elfeed-show-mode
    emms-playlist-mode
    emms-stream-mode
    erc-mode
    eshell-mode
    eww-mode
    forth-block-mode
    forth-mode
    fundamental-mode
    gnus-article-mode
    gnus-group-mode
    gnus-summary-mode
    help-mode
    helpful-mode
    jabber-chat-mode
    magit-popup-mode
    matrix-client-mode
    matrix-client-room-list-mode
    mu4e-headers-mode
    mu4e-view-mode
    nix-mode
    notmuch-search-mode
    notmuch-tree-mode
    occur-edit-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    rcirc-mode
    sauron-mode
    treemacs-mode
    twittering-mode
    vc-dir-mode
    w3m-mode
    woman-mode
    xref--xref-buffer-mode)
  "List of major-modes that are incompatible with `font-lock-ensure'.")

(defun helm-swoop-match-functions ()
  "Return match functions."
  (if helm-swoop-use-fuzzy-match
      (append helm-c-source-swoop-match-functions '(helm-fuzzy-match))
    helm-c-source-swoop-match-functions))

(defun helm-swoop-search-functions ()
  "Return search functions."
  (if helm-swoop-use-fuzzy-match
      (append helm-c-source-swoop-search-functions '(helm-fuzzy-search))
    helm-c-source-swoop-search-functions))

(defcustom helm-swoop-pre-input-function
  (lambda () (thing-at-point 'symbol))
  "This function can pre-input keywords when helm-swoop invoked."
  :group 'helm-swoop :type 'function)

(defun helm-swoop-pre-input-optimize (query)
  "Return QUERY processed by `regexp-quote'."
  (when query (regexp-quote query)))

(defsubst helm-swoop--goto-line (line)
  "Go to LINE concider narrowing."
  (goto-char (point-min))
  (forward-line (1- line)))

(defsubst helm-swoop--recenter ()
  "Recenter."
  (recenter (/ (window-height) 2)))

(defsubst helm-swoop--delete-overlay (identity &optional beg end)
  "Delete overlay if IDENTITY has in BEG to END."
  (overlay-recenter (or end (point-max)))
  (mapc (lambda (ov)
          (if (overlay-get ov identity)
              (delete-overlay ov)))
        (overlays-in (or beg (point-min)) (or end (point-max)))))

(defsubst helm-swoop--get-string-at-line ()
  "Get line string with no properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun helm-swoop--buffer-substring (beg end)
  "Get buffer substring BEG to END."
  (if helm-swoop-speed-or-color
      (let ((content (buffer-substring beg end)))
        (propertize content 'read-only nil))
    (buffer-substring-no-properties beg end)))

;;;###autoload
(defun helm-swoop-back-to-last-point (&optional cancel)
  "Go back to last position where `helm-swoop' was called.
If CANCEL is non-nil, store `helm-swoop-last-point'."
  (interactive)
  (if helm-swoop-last-point
      (let ((po (point)))
        (switch-to-buffer (cdr helm-swoop-last-point))
        (goto-char (car helm-swoop-last-point))
        (unless cancel
          (setq helm-swoop-last-point
                (cons po (buffer-name (current-buffer))))))))

(defun helm-swoop--split-lines-by (string regexp step)
  "Return STRING processed by `split-string' with REGEXP by STEP for multiline."
  (or step (setq step 1))
  (let ((from1 0) ;; last match point
        (from2 0) ;; last substring point
        list
        (i 1)) ;; from line 1
    (while (string-match regexp string from1)
      (setq i (1+ i))
      (if (eq 0 (% i step))
          (progn
            (setq list (cons (substring string from2 (match-beginning 0))
                             list))
            (setq from2 (match-end 0))
            (setq from1 (match-end 0)))
        (setq from1 (match-end 0))))
    (setq list (cons (substring string from2) list))
    (nreverse list)))

(defun helm-swoop--target-line-overlay-move (&optional buf)
  "Add color to the target line in BUF."
  (move-overlay
   helm-swoop-line-overlay
   (progn
     (search-backward
      "\n" nil t (% (line-number-at-pos) helm-swoop-last-prefix-number))
     (goto-char (point-at-bol)))
   ;; For multiline highlight
   (save-excursion
     (goto-char (point-at-bol))
     (or (re-search-forward "\n" nil t helm-swoop-last-prefix-number)
         ;; For the end of buffer error
         (point-max)))
   buf)
  (helm-swoop--unveil-invisible-overlay))

(defun helm-swoop--validate-regexp (regexp)
  "Validate REGEXP."
  (condition-case _err
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-swoop--target-word-overlay (identity &optional threshold)
  "Target ward overlay to IDENTITY with THRESHOLD."
  (interactive)
  (save-excursion
    (let ((pattern (split-string helm-pattern " "))
          (threshold (or threshold 2))
          ov)
      (mapc (lambda (wd)
              (when (and (helm-swoop--validate-regexp wd) (< threshold (length wd)))
                (goto-char (point-min))
                ;; Optional require migemo.el
                (if (and (featurep 'migemo) helm-migemo-mode)
                    (setq wd (migemo-search-pattern-get wd)))
                ;; For caret begging match
                (if (string-match "^\\^\\[0\\-9\\]\\+\\.\\(.+\\)" wd)
                    (setq wd (concat "^" (match-string 1 wd))))
                (overlay-recenter (point-max))
                (while (and (not (eobp)) (re-search-forward wd nil t))
                  (if (= (match-beginning 0) (match-end 0))
                      (forward-char 1)
                    (setq ov (make-overlay (match-beginning 0) (match-end 0)))
                    (overlay-put ov 'face 'helm-swoop-target-word-face)
                    (overlay-put ov identity t)))))
            pattern))))

(defun helm-swoop--restore-unveiled-overlay ()
  "Restore unveiled overlay."
  (when helm-swoop-invisible-targets
    (mapc (lambda (ov) (overlay-put (car ov) 'invisible (cdr ov)))
          helm-swoop-invisible-targets)
    (setq helm-swoop-invisible-targets nil)))

(defun helm-swoop--unveil-invisible-overlay ()
  "Show hidden text temporarily to view it during helm-swoop.
This function needs to call after latest helm-swoop-line-overlay set."
  (helm-swoop--restore-unveiled-overlay)
  (mapc (lambda (ov)
          (let ((type (overlay-get ov 'invisible)))
            (when type
              (overlay-put ov 'invisible nil)
              (setq helm-swoop-invisible-targets
                    (cons (cons ov type) helm-swoop-invisible-targets)))))
        (overlays-in (overlay-start helm-swoop-line-overlay)
                     (overlay-end helm-swoop-line-overlay))))

;; helm action ------------------------------------------------

(defun helm-swoop--move-line-action ()
  "Move line action."
  (with-helm-window
    (let* ((key (helm-swoop--get-string-at-line))
           (num (when (string-match "^[0-9]+" key)
                  (string-to-number (match-string 0 key)))))
      ;; Synchronizing line position
      (when (and key num)
        (with-selected-window helm-swoop-synchronizing-window
          (helm-swoop--goto-line num)
          (with-current-buffer helm-swoop-target-buffer
            (delete-overlay helm-swoop-line-overlay)
            (helm-swoop--target-line-overlay-move))
          (helm-swoop--recenter))
        (setq helm-swoop-last-line-info
              (cons helm-swoop-target-buffer num))))))

(defun helm-swoop--nearest-line (target list)
  "Return the nearest number of TARGET out of LIST."
  (when (and target list)
    (let (res)
      (cl-labels ((filter (fn elm list)
                          (let (r)
                            (mapc (lambda (e)
                                    (if (funcall fn elm e)
                                        (setq r (cons e r))))
                                  list) r)))
        (if (eq 1 (length list))
            (setq res (car list))
          (let* ((lts (filter '> target list))
                 (gts (filter '< target list))
                 (lt  (if lts (apply 'max lts)))
                 (gt  (if gts (apply 'min gts)))
                 (ltg (if lt (- target lt)))
                 (gtg (if gt (- gt target))))
            (setq res
                  (cond ((memq target list) target)
                        ((and (not lt) (not gt)) nil)
                        ((not gtg) lt)
                        ((not ltg) gt)
                        ((eq ltg gtg) gt)
                        ((< ltg gtg) lt)
                        ((> ltg gtg) gt)
                        (t 1))))))
      res)))

(defun helm-swoop--keep-nearest-position ()
  "Keep nearest position."
  (with-helm-window
    (let ((p (point-min))
          (buf (rx-to-string (buffer-name (car helm-swoop-last-line-info)) t))
          list bound nearest-line target-point)
      (save-excursion
        (goto-char p)
        (while (if p (setq p (re-search-forward (concat "^" buf "$") nil t)))
          (when (get-text-property (point-at-bol) 'helm-header)
            (forward-char 1)
            (setq bound (next-single-property-change (point) 'helm-header))
            (while (re-search-forward "^[0-9]+" bound t)
              (setq list (cons
                          (string-to-number (match-string 0))
                          list)))
            (setq nearest-line (helm-swoop--nearest-line
                                (cdr helm-swoop-last-line-info)
                                list))
            (goto-char p)
            (re-search-forward (concat "^"
                                       (number-to-string nearest-line)
                                       "\\s-") bound t)
            (setq target-point (point))
            (setq p nil))))
      (when target-point
        (goto-char target-point)
        (helm-mark-current-line)
        (if (equal helm-swoop-buffer (buffer-name (current-buffer)))
            (helm-swoop--move-line-action)
          (helm-multi-swoop--move-line-action))))))

(defun helm-swoop--pattern-match ()
  "Overlay target words."
  (with-helm-window
    (setq helm-swoop-pattern helm-pattern)
    (when (< helm-swoop-min-overlay-length (length helm-pattern))
      (helm-swoop--delete-overlay 'target-buffer)
      (helm-swoop--target-word-overlay 'target-buffer helm-swoop-min-overlay-length)
      (with-selected-window helm-swoop-synchronizing-window
        (helm-swoop--delete-overlay 'target-buffer)
        (helm-swoop--target-word-overlay 'target-buffer helm-swoop-min-overlay-length)))))

(defun helm-swoop-flash-word (match-beg match-end)
  "Flash ward in MATCH-BEG to MATCH-END."
  (interactive)
  (unwind-protect
      (let ((ov (make-overlay match-beg match-end)))
        (when ov
          (overlay-put ov 'face 'helm-swoop-target-word-face)
          (overlay-put ov 'helm-swoop-overlay-word-frash t)))
    (run-with-idle-timer
     0.6 nil (lambda () (helm-swoop--delete-overlay 'helm-swoop-overlay-word-frash)))))

;; core ------------------------------------------------

(defun helm-swoop--match-part (candidate)
  "Extract the proper part of CANDIDATE."
  (replace-regexp-in-string (rx bol (+? digit) " ") "" candidate))

(defun helm-swoop--maybe-fontify! ()
  "Ensure the entired buffer is highlighted."
  (let ((fontify-safe-p (not (or (derived-mode-p 'magit-mode)
                                 (bound-and-true-p magit-blame-mode)
                                 (memq major-mode helm-swoop-font-lock-exclude)
                                 (not (derived-mode-p 'prog-mode))))))
    (when (and helm-swoop-speed-or-color
               font-lock-mode
               fontify-safe-p
               (or (eq 'always helm-swoop-fontify-buffer-size-limit)
                   (< (buffer-size) helm-swoop-fontify-buffer-size-limit)))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer))))))

(defun helm-swoop--get-content (buffer &optional linum)
  "Get the whole content in BUFFER and add line number at the head.
If LINUM is number, lines are separated by LINUM."
  (let ((buf (get-buffer buffer)))
    (when buf
      (with-current-buffer buf
        (helm-swoop--maybe-fontify!)
        (let ((bufstr (helm-swoop--buffer-substring (point-min) (point-max))))
          (with-temp-buffer
            (insert bufstr)
            (goto-char (point-min))
            (let ((i 1))
              (insert (format "%s " i))
              (while (re-search-forward "\n" nil t)
                (setq i (1+ i))
                (if helm-swoop-use-line-number-face
                    (insert (propertize (format "%s" i) 'font-lock-face 'helm-swoop-line-number-face) " ")
                  (insert (format "%s " i))))
              ;; Delete empty lines
              (unless linum
                (goto-char (point-min))
                (while (re-search-forward "^[0-9]+\\s-*$" nil t)
                  (replace-match ""))))
            (helm-swoop--buffer-substring (point-min) (point-max))))))))

(defun helm-swoop--goto-line-action (line &optional other-window)
  "Goto LINE action.  If OTHER-WINDOW is non-nil, a new window will be open."
  (run-hooks 'helm-swoop-before-goto-line-action-hook)
  (when other-window
    (switch-to-buffer-other-window (current-buffer)))
  (helm-swoop--goto-line
   (when (string-match "^[0-9]+" line)
     (string-to-number (match-string 0 line))))
  (let ((regex (mapconcat 'identity (split-string helm-pattern " ") "\\|")))
    (when (or (and (featurep 'migemo) helm-migemo-mode (migemo-forward regex nil t))
              (re-search-forward regex nil t))
      (funcall helm-swoop-flash-region-function
               (match-beginning 0) (match-end 0))
      (goto-char (match-beginning 0))
      (run-hooks 'helm-swoop-after-goto-line-action-hook)))
  (helm-swoop--recenter))

(defun helm-c-source-swoop ()
  "C source swoop."
  `((name . ,(buffer-name helm-swoop-target-buffer))
    (candidates . ,(if helm-swoop-list-cache
                       (progn
                         (helm-swoop--split-lines-by
                          helm-swoop-list-cache "\n" helm-swoop-last-prefix-number))
                     (helm-swoop--split-lines-by
                      (setq helm-swoop-list-cache
                            (helm-swoop--get-content helm-swoop-target-buffer t))
                      "\n" helm-swoop-last-prefix-number)))
    (get-line . ,#'helm-swoop--buffer-substring)
    (keymap . ,helm-swoop-map)
    (header-line . ,(substitute-command-keys
                     "[\\<helm-swoop-map>\\[helm-swoop-edit]] Edit mode, \
[\\<helm-swoop-map>\\[helm-multi-swoop-all-from-helm-swoop]] apply all buffers"))
    (action . (("Go to Line" . helm-swoop--goto-line-action)
               ("Edit" . helm-swoop--edit)
               ("Go to Line in Other Window" . (lambda (line) (helm-swoop--goto-line-action line t)))))
    ,(if (and helm-swoop-last-prefix-number
              (> helm-swoop-last-prefix-number 1))
         '(multiline))
    (match . ,(helm-swoop-match-functions))
    (match-part . ,#'helm-swoop--match-part)
    ;; (search . ,(helm-swoop-search-functions))
    ))

(defun helm-c-source-multi-swoop (buf func action multiline)
  "C source multi swoop in BUF for FUNC, ACTION, MULTILINE."
  `((name . ,buf)
    (candidates . ,(funcall func))
    (action . ,action)
    (header-line . ,(concat buf
                            (substitute-command-keys
                             "    [\\<helm-multi-swoop-map>\\[helm-multi-swoop-edit]] Edit mode")))
    (keymap . ,helm-multi-swoop-map)
    (requires-pattern . 2)
    ,(if (and multiline
              (> multiline 1))
         '(multiline))
    (match . ,(helm-swoop-match-functions))
    (match-part . ,#'helm-swoop--match-part)
    ;; (search . ,(helm-swoop-search-functions))
    ))

(defun helm-swoop--set-prefix (multiline)
  "Set prefix MULTILINE."
  ;; Enable scrolling margin
  (setq helm-swoop-last-prefix-number
        (or multiline 1))) ;; multiline is for resume

;; Delete cache when buffer is saved or file changes on disk
(defun helm-swoop--clear-cache ()
  "Delete cache when buffer is saved or file change on disk."
  (if (boundp 'helm-swoop-list-cache) (setq helm-swoop-list-cache nil)))
(add-hook 'after-save-hook 'helm-swoop--clear-cache)
(add-hook 'after-revert-hook 'helm-swoop--clear-cache)

(defadvice narrow-to-region (around helm-swoop-advice-narrow-to-region activate)
  "Advice for `narrow-to-region'."
  (helm-swoop--clear-cache)
  ad-do-it)
(defadvice narrow-to-defun (around helm-swoop-advice-narrow-to-defun activate)
  "Advice for `narrow-to-defun'."
  (helm-swoop--clear-cache)
  ad-do-it)
(defadvice narrow-to-page (around helm-swoop-advice-narrow-to-page activate)
  "Advice for `narrow-to-page'."
  (helm-swoop--clear-cache)
  ad-do-it)
(defadvice widen (around helm-swoop-advice-widen activate)
  "Advice for `widen'."
  (helm-swoop--clear-cache)
  ad-do-it)

(defun helm-swoop--restore ()
  "Restore."
  (when (= 1 helm-exit-status)
    (helm-swoop-back-to-last-point t)
    (helm-swoop--restore-unveiled-overlay))
  (setq helm-swoop-invisible-targets nil)
  (remove-hook 'helm-after-update-hook 'helm-swoop--pattern-match)
  (remove-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position)
  (setq helm-swoop-last-query helm-swoop-pattern)
  (mapc (lambda (ov)
          (when (or (eq 'helm-swoop-target-line-face (overlay-get ov 'face))
                    (eq 'helm-swoop-target-line-block-face
                        (overlay-get ov 'face)))
            (delete-overlay ov)))
        (overlays-in (point-min) (point-max)))
  (helm-swoop--delete-overlay 'target-buffer)
  (deactivate-mark t))

;;;###autoload
(cl-defun helm-swoop (&key query source (multiline current-prefix-arg))
  "List the all lines to another buffer, which is able to squeeze by
 any words you input. At the same time, the original buffer's cursor
 is jumping line to line according to moving up and down the list."
  (interactive)
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point (cons (point) (buffer-name (current-buffer))))
  (setq helm-swoop-last-line-info
        (cons (current-buffer) (line-number-at-pos)))
  (unless (boundp 'helm-swoop-last-query)
    (set (make-local-variable 'helm-swoop-last-query) ""))
  (setq helm-swoop-target-buffer (current-buffer))
  (helm-swoop--set-prefix (prefix-numeric-value multiline))
  ;; Overlay
  (setq helm-swoop-line-overlay (make-overlay (point) (point)))
  (overlay-put helm-swoop-line-overlay
               'face (if (< 1 helm-swoop-last-prefix-number)
                         'helm-swoop-target-line-block-face
                       'helm-swoop-target-line-face))
  ;; Cache
  (cond ((not (boundp 'helm-swoop-list-cache))
         (set (make-local-variable 'helm-swoop-list-cache) nil))
        ((buffer-modified-p)
         (setq helm-swoop-list-cache nil)))
  (unwind-protect
      (progn
        ;; For synchronizing line position
        (add-hook 'helm-after-update-hook 'helm-swoop--pattern-match)
        (add-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position t)
        (cond (query
               (if (string-match
                    "\\(\\^\\[0\\-9\\]\\+\\.\\)\\(.*\\)" query)
                   query ;; NEED FIX #1 to appear as a "^"
                 query))
              (mark-active
               (let ((st (buffer-substring-no-properties
                          (region-beginning) (region-end))))
                 (if (string-match "\n" st)
                     (message "Multi line region is not allowed")
                   (setq query (helm-swoop-pre-input-optimize st)))))
              ((setq query (helm-swoop-pre-input-optimize
                            (funcall helm-swoop-pre-input-function))))
              (t (setq query "")))
        ;; First behavior
        (helm-swoop--recenter)
        (move-beginning-of-line 1)
        (helm-swoop--target-line-overlay-move)
        ;; Execute helm
        (let ((helm-display-function helm-swoop-split-window-function)
              (helm-display-source-at-screen-top nil)
              (helm-completion-window-scroll-margin 5))
          (helm :sources
                (or source
                    (helm-c-source-swoop))
                :buffer helm-swoop-buffer
                :input query
                :prompt helm-swoop-prompt
                :preselect
                ;; Get current line has content or else near one
                (if (string-match "^[\t\n\s]*$" (helm-swoop--get-string-at-line))
                    (save-excursion
                      (if (re-search-forward "[^\t\n\s]" nil t)
                          (format "^%s\s" (line-number-at-pos))
                        (re-search-backward "[^\t\n\s]" nil t)
                        (format "^%s\s" (line-number-at-pos))))
                  (format "^%s\s" (line-number-at-pos)))
                :candidate-number-limit helm-swoop-candidate-number-limit)))
    ;; Restore helm's hook and window function etc
    (helm-swoop--restore)))

;; Receive word from isearch ---------------
;;;###autoload
(defun helm-swoop-from-isearch ()
  "Invoke `helm-swoop' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (let (search-nonincremental-instead)
      (isearch-exit))
    (helm-swoop :query query)))
;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; Receive word from evil search ---------------
(defun helm-swoop-from-evil-search ()
  "Invoke `helm-swoop' from evil isearch."
  (interactive)
  (if (string-match "\\(isearch-\\|evil.*search\\)" (symbol-name real-last-command))
      (helm-swoop :query (if isearch-regexp
                             isearch-string
                           (regexp-quote isearch-string)))
    (helm-swoop)))
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Receive word from evil search ---------------
(defun helm-swoop-yank-thing-at-point ()
  "Insert string at which the point helm-swoop started."
  (interactive)
  (let (amend buf)
    (with-selected-window helm-swoop-synchronizing-window
      (setq buf (get-buffer (cdr helm-swoop-last-point)))
      (when buf
        (with-current-buffer buf
          (save-excursion
            (goto-char (car helm-swoop-last-point))
            (setq amend (thing-at-point 'symbol))))))
    (when amend
      (with-selected-window (minibuffer-window)
        (insert amend)))))

;; For helm-resume ------------------------
(defadvice helm-resume-select-buffer
    (around helm-swoop-if-selected-as-resume activate)
  "Resume if *Helm Swoop* buffer selected when helm-resume with prefix."
  (if (boundp 'helm-swoop-last-query)
      ad-do-it
    ;; If the buffer hasn't called helm-swoop, just hide from options
    (let ((helm-buffers (delete helm-swoop-buffer helm-buffers)))
      ad-do-it))
  (when (and (equal ad-return-value helm-swoop-buffer)
             (boundp 'helm-swoop-last-query))
    (helm-swoop :query helm-swoop-last-query
                :multiline helm-swoop-last-prefix-number)
    (setq ad-return-value nil)))

(defadvice helm-resume (around helm-swoop-resume activate)
  "Resume if the last used helm buffer is helm-swoop-buffer."
  (if (equal helm-last-buffer helm-swoop-buffer)
      (if (boundp 'helm-swoop-last-query)
          (if (not (ad-get-arg 0))
              (helm-swoop :query helm-swoop-last-query
                          :multiline helm-swoop-last-prefix-number))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers))) ad-do-it))
    ad-do-it))

;;; @ helm-swoop-edit -----------------------------------------

(defvar helm-swoop-edit-target-buffer)
(defvar helm-swoop-edit-buffer "*Helm Swoop Edit*")
(defvar helm-swoop-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'helm-swoop--edit-complete)
    (define-key map (kbd "C-c C-g") 'helm-swoop--edit-cancel)
    (define-key map (kbd "C-c C-q C-k") 'helm-swoop--edit-delete-all-lines)
    map))

(defun helm-swoop--clear-edit-buffer (prop)
  "Clear edit buffer with PROP."
  (let ((inhibit-read-only t))
    (mapc (lambda (ov)
            (when (overlay-get ov prop)
              (delete-overlay ov)))
          (overlays-in (point-min) (point-max)))
    (set-text-properties (point-min) (point-max) nil)
    (goto-char (point-min))
    (erase-buffer)))

(defun helm-swoop--collect-edited-lines ()
  "Create a list of edited lines with each its own line number."
  (interactive)
  (let (res)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\)\s" nil t)
      (setq res
            (cons (cons (string-to-number (match-string 1))
                        (buffer-substring-no-properties
                         (point)
                         (save-excursion
                           (if (re-search-forward
                                "^\\([0-9]+\\)\s\\|^\\(\\-+\\)" nil t)
                               (1- (match-beginning 0))
                             (goto-char (point-max))
                             (re-search-backward "\n" nil t)))))
                  res)))
    res))

(defun helm-swoop--edit (candidate)
  "This function will only be called from `helm-swoop-edit' with CANDIDATE."
  (interactive)
  (setq helm-swoop-edit-target-buffer helm-swoop-target-buffer)
  (delete-overlay helm-swoop-line-overlay)
  (helm-swoop--delete-overlay 'target-buffer)
  (with-current-buffer (get-buffer-create helm-swoop-edit-buffer)

    (helm-swoop--clear-edit-buffer 'helm-swoop-edit)
    (let ((bufstr ""))
      ;; Get target line number to edit
      (with-current-buffer helm-swoop-buffer
        ;; Use selected line by [C-SPC] or [M-SPC]
        (mapc (lambda (ov)
                (when (eq 'helm-visible-mark (overlay-get ov 'face))
                  (setq bufstr (concat (buffer-substring-no-properties
                                        (overlay-start ov) (overlay-end ov))
                                       bufstr))))
              (overlays-in (point-min) (point-max)))
        (if (equal "" bufstr)
            ;; Not found selected line
            (setq bufstr (buffer-substring-no-properties
                          (point-min) (point-max)))
          ;; Attach title
          (setq bufstr (concat "Helm Swoop\n" bufstr))))

      ;; Set for edit buffer
      (insert bufstr)
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t front-sticky t))

      ;; Set for editable context
      (let ((inhibit-read-only t))
        ;; Title and explanation
        (goto-char (point-min))
        (let ((ov (make-overlay (point) (point-at-eol))))
          (overlay-put ov 'helm-swoop-edit t)
          (overlay-put ov 'face 'font-lock-function-name-face)
          (overlay-put ov 'after-string
                       (propertize
                        (substitute-command-keys
                         (concat
                          " [\\<helm-swoop-edit-map>\\[helm-swoop--edit-complete]] Complete"
                          ", [\\<helm-swoop-edit-map>\\[helm-swoop--edit-cancel]] Cancel"
                          ", [\\<helm-swoop-edit-map>\\[helm-swoop--edit-delete-all-lines]] Delete All"))
                        'face 'helm-bookmark-addressbook)))
        ;; Line number and editable area
        (while (re-search-forward "^\\([0-9]+\s\\)\\(.*\\)$" nil t)
          (let* ((bol1 (match-beginning 1))
                 (eol1 (match-end 1))
                 (bol2 (match-beginning 2))
                 (eol2 (match-end 2)))
            ;; Line number
            (add-text-properties bol1 eol1
                                 '(face font-lock-function-name-face
                                        intangible t))
            ;; Editable area
            (remove-text-properties bol2 eol2 '(read-only t))
            ;; For line tail
            (set-text-properties eol2 (or (1+ eol2) (point-max))
                                 '(read-only t rear-nonsticky t))))
        (helm-swoop--target-word-overlay 'edit-buffer 0))))

  (other-window 1)
  (switch-to-buffer helm-swoop-edit-buffer)
  (goto-char (point-min))
  (if (string-match "^[0-9]+" candidate)
      (re-search-forward
       (concat "^" (match-string 0 candidate)) nil t))
  (use-local-map helm-swoop-edit-map))

(defun helm-swoop--edit-complete ()
  "Apply change and kill temporary edit buffer."
  (interactive)
  (let ((lines (helm-swoop--collect-edited-lines)))
    (with-current-buffer helm-swoop-edit-target-buffer
      ;; Replace from the end of buffer
      (save-excursion
        (mapc (lambda (cell)
                (let ((k (car cell))
                      (v (cdr cell)))
                  (goto-char (point-min))
                  (delete-region (point-at-bol k) (point-at-eol k))
                  (goto-char (point-at-bol k))
                  (insert v)))
              lines)))
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-swoop-edit-buffer)))
  (message "Successfully helm-swoop-edit applied to original buffer"))

(defun helm-swoop--edit-delete-all-lines ()
  "Apply change and kill temporary edit buffer."
  (interactive)
  (let ((lines (helm-swoop--collect-edited-lines)))
    (with-current-buffer helm-swoop-edit-target-buffer
      ;; Replace from the end of buffer
      (save-excursion
        (mapc (lambda (cell)
                (let ((k (car cell)))
                  (goto-char (point-min))
                  (delete-region (point-at-bol k)
                                 (if (eq (point-at-eol k) (point-max))
                                     (point-at-eol k)
                                   (1+ (point-at-eol k))))))
              lines)))
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-swoop-edit-buffer)))
  (message "Successfully helm-swoop-edit applied to original buffer"))

(defun helm-swoop--edit-cancel ()
  "Cancel edit and kill temporary buffer."
  (interactive)
  (select-window helm-swoop-synchronizing-window)
  (kill-buffer (get-buffer helm-swoop-edit-buffer))
  (message "helm-swoop-edit canceled"))

(defun helm-swoop-edit ()
  "Execute `helm-swoop--edit'."
  (interactive)
  (helm-exit-and-execute-action 'helm-swoop--edit))

;;; @ helm-multi-swoop ----------------------------------------
(defvar helm-multi-swoop-buffer-list "*helm-multi-swoop buffers list*"
  "Buffer name.")
(defvar helm-multi-swoop-ignore-buffers-match "^\\*"
  "Regexp to eliminate buffers you don't want to see.")
(defvar helm-multi-swoop-candidate-number-limit 250)
(defvar helm-multi-swoop-last-selected-buffers nil)
(defvar helm-multi-swoop-last-query nil)
(defvar helm-multi-swoop-query nil)
(defvar helm-multi-swoop-buffer "*Helm Multi Swoop*")
(defvar helm-multi-swoop-all-from-helm-swoop-last-point nil
  "For the last position, when helm-multi-swoop-all-from-helm-swoop canceled.")
(defvar helm-multi-swoop-move-line-action-last-buffer nil)

(defvar helm-multi-swoop-buffers-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "RET")
      (lambda () (interactive)
        (helm-exit-and-execute-action 'helm-multi-swoop--exec)))
    (delq nil map)))

(defvar helm-multi-swoop-projectile-buffers-filter
  #'projectile-buffers-with-file-or-process)
;; action -----------------------------------------------------

(defun helm-multi-swoop--move-line-action ()
  "Move line action."
  (with-helm-window
    (let* ((key (buffer-substring (point-at-bol) (point-at-eol)))
           (num (when (string-match "^[0-9]+" key)
                  (string-to-number (match-string 0 key))))
           (source (helm-get-current-source))
           (buf (let ((name (assoc-default 'name source)))
                  (when name (get-buffer name)))))
      (when buf
        ;; Synchronizing line position
        (with-selected-window helm-swoop-synchronizing-window
          (with-current-buffer buf
            (when (not (eq buf helm-multi-swoop-move-line-action-last-buffer))
              (set-window-buffer nil buf)
              (helm-swoop--pattern-match))
            (helm-swoop--goto-line num)
            (helm-swoop--target-line-overlay-move buf)
            (helm-swoop--recenter))
          (setq helm-multi-swoop-move-line-action-last-buffer buf))
        (setq helm-swoop-last-line-info (cons buf num))))))

(defun helm-multi-swoop--get-marked-buffers ()
  "Get marked buffers."
  (let ((buf (get-buffer helm-multi-swoop-buffer-list))
        lst)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (dolist (ov (cl-delete-if-not
                     (lambda (o)
                       (eq 'helm-visible-mark (overlay-get o 'face)))
                     (overlays-in (point-min) (point-max))))
          (let ((word (string-trim
                       (buffer-substring-no-properties
                        (overlay-start ov) (overlay-end ov)))))
            (unless (string-empty-p word)
              (push word lst)))))
      (nreverse (sort lst #'string<)))))

;; core --------------------------------------------------------

(cl-defun helm-multi-swoop--exec (ignored &key query buflist func action)
  "Multi swoop exec with IGNORED QUERY BUFLIST FUNC ACTION."
  (interactive)
  (setq helm-swoop-synchronizing-window (selected-window))
  (setq helm-swoop-last-point
        (or helm-multi-swoop-all-from-helm-swoop-last-point
            (cons (point) (buffer-name (current-buffer)))))
  (setq helm-swoop-last-line-info
        (cons (current-buffer) (line-number-at-pos)))
  (unless (get-buffer helm-multi-swoop-buffer-list)
    (get-buffer-create helm-multi-swoop-buffer-list))
  (helm-swoop--set-prefix (prefix-numeric-value current-prefix-arg))
  (let ((buffs (or buflist
                   (helm-multi-swoop--get-marked-buffers)
                   `(,(buffer-name helm-swoop-target-buffer))
                   (error "No buffer selected")))
        (prefix-arg (prefix-numeric-value
                     (or current-prefix-arg helm-swoop-last-prefix-number 1)))
        contents preserve-position)
    (helm-swoop--set-prefix prefix-arg)
    (setq helm-multi-swoop-last-selected-buffers buffs)
    ;; Create buffer sources
    (mapc (lambda (buf)
            (when (get-buffer buf)
              (with-current-buffer (get-buffer buf)
                (let* ((func
                        (or func
                            (lambda ()
                              (helm-swoop--split-lines-by
                               (helm-swoop--get-content buf t)
                               "\n" prefix-arg))))
                       (action
                        (or action
                            `(("Go to Line"
                               . (lambda (line)
                                   (switch-to-buffer ,buf)
                                   (helm-swoop--goto-line
                                    (when (string-match "^[0-9]+" line)
                                      (string-to-number
                                       (match-string 0 line))))
                                   (when (re-search-forward
                                          (mapconcat 'identity
                                                     (split-string
                                                      helm-pattern " ") "\\|")
                                          nil t)
                                     (funcall helm-swoop-flash-region-function
                                              (match-beginning 0) (match-end 0))
                                     (goto-char (match-beginning 0)))
                                   (helm-swoop--recenter)))
                              ("Edit" . helm-multi-swoop--edit)))))
                  (setq preserve-position
                        (cons (cons buf (point)) preserve-position))
                  (setq
                   contents
                   (cons
                    (helm-c-source-multi-swoop buf func action prefix-arg)
                    contents))))))
          buffs)
    (unwind-protect
        (progn
          (add-hook 'helm-after-update-hook 'helm-swoop--pattern-match)
          (add-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position t)
          (setq helm-swoop-line-overlay
                (make-overlay (point) (point)))
          (overlay-put helm-swoop-line-overlay
                       'face 'helm-swoop-target-line-face)
          (helm-swoop--target-line-overlay-move)
          ;; Execute helm
          (let ((helm-display-function helm-swoop-split-window-function)
                (helm-display-source-at-screen-top nil)
                (helm-completion-window-scroll-margin 5))
            (helm :sources contents
                  :buffer helm-multi-swoop-buffer
                  :input (or query helm-multi-swoop-query "")
                  :prompt helm-swoop-prompt
                  :candidate-number-limit
                  helm-multi-swoop-candidate-number-limit
                  :preselect
                  (regexp-quote
                   (format "%s %s" (line-number-at-pos)
                           (helm-swoop--get-string-at-line))))))
      ;; Restore
      (progn
        (when (= 1 helm-exit-status)
          (helm-swoop-back-to-last-point t)
          (helm-swoop--restore-unveiled-overlay))
        (setq helm-swoop-invisible-targets nil)
        (remove-hook 'helm-after-update-hook 'helm-swoop--pattern-match)
        (remove-hook 'helm-after-update-hook 'helm-swoop--keep-nearest-position)
        (setq helm-multi-swoop-last-query helm-swoop-pattern)
        (helm-swoop--restore-unveiled-overlay)
        (setq helm-multi-swoop-query nil)
        (setq helm-multi-swoop-all-from-helm-swoop-last-point nil)
        (mapc (lambda (buf)
                (let ((current-buf (buffer-name (current-buffer))))
                  (with-current-buffer (car buf)
                    ;; Delete overlay
                    (delete-overlay helm-swoop-line-overlay)
                    (helm-swoop--delete-overlay 'target-buffer)
                    ;; Restore each buffer's position
                    (unless (equal (car buf) current-buf)
                      (goto-char (cdr buf))))))
              preserve-position)))))

(defun helm-multi-swoop--get-buffer-list ()
  "Get buffer list."
  (let (buflist1 buflist2)
    ;; eliminate buffers start with whitespace and dired buffers
    (mapc (lambda (buf)
            (setq buf (buffer-name buf))
            (unless (string-match "^\\s-" buf)
              (unless (eq 'dired-mode (with-current-buffer buf major-mode))
                (setq buflist1 (cons buf buflist1)))))
          (buffer-list))
    ;; eliminate buffers match pattern
    (mapc (lambda (buf)
            (unless (string-match helm-multi-swoop-ignore-buffers-match buf)
              (setq buflist2 (cons buf buflist2))))
          buflist1)
    buflist2))

(defun helm-c-source-helm-multi-swoop-buffers ()
  "Show buffer list to select."
  `((name . "helm-multi-swoop select buffers")
    (candidates . helm-multi-swoop--get-buffer-list)
    (header-line . "[C-SPC]/[M-SPC] select, [RET] next step")
    (keymap . ,helm-multi-swoop-buffers-map)
    (match . ,(helm-swoop-match-functions))
    ;; (search . ,(helm-swoop-search-functions))
    ))

(defun helm-multi-swoop--get-query (query)
  "Get query from QUERY."
  (cond (query
         (setq helm-multi-swoop-query query))
        (mark-active
         (let ((st (buffer-substring-no-properties
                    (region-beginning) (region-end))))
           (if (string-match "\n" st)
               (message "Multi line region is not allowed")
             (setq helm-multi-swoop-query
                   (helm-swoop-pre-input-optimize st)))))
        ((setq helm-multi-swoop-query
               (helm-swoop-pre-input-optimize
                (funcall helm-swoop-pre-input-function))))
        (t (setq helm-multi-swoop-query ""))))

;;;###autoload
(defun helm-multi-swoop (&optional query buflist)
  "Multi swoop for QUERY in BUFLIST.

Usage:
  \\[execute-extended-command] helm-multi-swoop
  1. Select any buffers by [C-SPC] or [M-SPC]
  2. Press [RET] to start `helm-multi-swoop'

\\[universal-argument] \\[execute-extended-command] helm-multi-swoop
If you have done helm-multi-swoop before, you can skip select buffers step.
Last selected buffers will be applied to helm-multi-swoop."
  (interactive)
  (setq helm-multi-swoop-query (helm-multi-swoop--get-query query))
  (if (equal current-prefix-arg '(4))
      (helm-multi-swoop--exec nil
                              :query helm-multi-swoop-query
                              :buflist buflist)
    (if buflist
        (helm-multi-swoop--exec nil
                                :query query
                                :buflist buflist)
      (helm :sources (helm-c-source-helm-multi-swoop-buffers)
            :buffer helm-multi-swoop-buffer-list
            :prompt "Mark any buffers by [C-SPC] or [M-SPC]: "))))

;;;###autoload
(defun helm-multi-swoop-all (&optional query)
  "Apply all buffers to helm-multi-swoop for QUERY."
  (interactive)
  (setq helm-multi-swoop-query (helm-multi-swoop--get-query query))
  (helm-multi-swoop--exec nil
                          :query helm-multi-swoop-query
                          :buflist (helm-multi-swoop--get-buffer-list)))

(defun get-buffers-matching-mode (mode)
  "Return a list of buffers where their `major-mode' is equal to MODE."
  (let (buffer-mode-matches)
    (mapc (lambda (buf)
            (when (get-buffer buf)
              (with-current-buffer (get-buffer buf)
                (if (eq mode major-mode)
                    (add-to-list 'buffer-mode-matches (buffer-name buf))))))
          (buffer-list))
    buffer-mode-matches))

(defun helm-multi-swoop-by-mode (mode &optional query)
  "Apply all buffers whose mode is MODE to helm-multi-swoop for QUERY."
  (setq helm-multi-swoop-query (helm-multi-swoop--get-query query))
  (if (get-buffers-matching-mode mode)
      (helm-multi-swoop--exec nil
                              :query helm-multi-swoop-query
                              :buflist (get-buffers-matching-mode mode))
    (message "there are no buffers in that mode right now")))

;;;###autoload
(defun helm-multi-swoop-org (&optional query)
  "Applie all `org-mode' buffers to helm-multi-swoop for QUERY."
  (interactive)
  (helm-multi-swoop-by-mode 'org-mode query))

;;;###autoload
(defun helm-multi-swoop-current-mode (&optional query)
  "Applie all buffers of the same mode as the current buffer to helm-multi-swoop for QUERY."
  (interactive)
  (helm-multi-swoop-by-mode major-mode query))

;;;###autoload
(defun helm-multi-swoop-projectile (&optional query)
  "Apply all opened buffers of the current project to helm-multi-swoop for QUERY."
  (interactive)
  (setq helm-multi-swoop-query (helm-multi-swoop--get-query query))
  (if (require 'projectile nil 'noerror)
      ;; set filter function that is used in projectile-project-buffers
      (let ((projectile-buffers-filter-function
             helm-multi-swoop-projectile-buffers-filter))
        (helm-multi-swoop--exec nil
                                :query helm-multi-swoop-query
                                :buflist (mapcar #'buffer-name
                                                 (projectile-project-buffers))))
    (error "Package 'projectile' is not available")))


(defun helm-swoop--wrap-function-with-pre-input-function (target-fn pre-input-fn)
  "Wrap function with pre input function for TARGET-FN and PRE-INPUT-FN."
  (let ((helm-swoop-pre-input-function pre-input-fn))
    (funcall target-fn)))

;;;###autoload
(defun helm-swoop-without-pre-input ()
  "Start helm-swoop without pre input query."
  (interactive)
  (helm-swoop--wrap-function-with-pre-input-function
   'helm-swoop (lambda () nil)))

;;;###autoload
(defun helm-swoop-symble-pre-input ()
  "Start helm-swoop without pre input query."
  (interactive)
  (helm-swoop--wrap-function-with-pre-input-function
   'helm-swoop (lambda () (format "\\_<%s\\_> " (thing-at-point 'symbol)))))

;; option -------------------------------------------------------

(defun helm-multi-swoop-all-from-isearch ()
  "Invoke `helm-multi-swoop-all' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (let (search-nonincremental-instead)
      (isearch-exit))
    (helm-multi-swoop-all query)))
;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "C-x M-i") 'helm-multi-swoop-all-from-isearch)

(defun helm-multi-swoop-all-from-helm-swoop ()
  "Invoke `helm-multi-swoop-all' from helm-swoop."
  (interactive)
  (helm-swoop--restore)
  (delete-overlay helm-swoop-line-overlay)
  (setq helm-multi-swoop-all-from-helm-swoop-last-point helm-swoop-last-point)
  (let ((query helm-pattern))
    (run-with-timer 0 nil (lambda () (helm-multi-swoop-all query))))
  (helm-exit-minibuffer))

(defun helm-multi-swoop-current-mode-from-helm-swoop ()
  "Invoke `helm-multi-swoop-all' from helm-swoop."
  (interactive)
  (helm-swoop--restore)
  (delete-overlay helm-swoop-line-overlay)
  (setq helm-multi-swoop-all-from-helm-swoop-last-point helm-swoop-last-point)
  (let ((query helm-pattern))
    (run-with-timer 0 nil (lambda () (helm-multi-swoop-current-mode query))))
  (helm-exit-minibuffer))
;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

(defadvice helm-resume (around helm-multi-swoop-resume activate)
  "Resume if the last used helm buffer is *Helm Swoop*."
  (if (equal helm-last-buffer helm-multi-swoop-buffer)
      (if (boundp 'helm-multi-swoop-last-query)
          (if (not (ad-get-arg 0))
              (helm-multi-swoop helm-multi-swoop-last-query
                                helm-multi-swoop-last-selected-buffers))
        ;; Temporary apply second last buffer
        (let ((helm-last-buffer (cadr helm-buffers)))
          ad-do-it))
    ad-do-it))

;;; @ helm-multi-swoop-edit -----------------------------------
(defvar helm-multi-swoop-edit-save t
  "Save each buffer you edit when editing is complete.")
(defvar helm-multi-swoop-edit-buffer "*Helm Multi Swoop Edit*")

(defvar helm-multi-swoop-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'helm-multi-swoop--edit-complete)
    (define-key map (kbd "C-c C-g") 'helm-multi-swoop--edit-cancel)
    (define-key map (kbd "C-c C-q C-k") 'helm-multi-swoop--edit-delete-all-lines)
    map))

(defun helm-multi-swoop--edit (candidate)
  "This function will only be called from `helm-swoop-edit' for CANDIDATE."
  (interactive)
  (delete-overlay helm-swoop-line-overlay)
  (helm-swoop--delete-overlay 'target-buffer)
  (with-current-buffer (get-buffer-create helm-multi-swoop-edit-buffer)
    (helm-swoop--clear-edit-buffer 'helm-multi-swoop-edit)
    (let ((bufstr "")
          (buf (get-buffer helm-multi-swoop-buffer))
          mark)
      ;; Get target line number to edit
      (when buf
        (with-current-buffer buf
          ;; Set overlay to helm-source-header for editing marked lines
          (save-excursion
            (goto-char (point-min))
            (let ((beg (point))
                  end)
              (overlay-recenter (point-max))
              (while (setq beg (text-property-any beg (point-max)
                                                  'face 'helm-source-header))
                (setq end (next-single-property-change beg 'face))
                (overlay-put (make-overlay beg end) 'source-header t)
                (setq beg end)
                (goto-char end))))
          ;; Use selected line by [C-SPC] or [M-SPC]
          (mapc (lambda (ov)
                  (when (overlay-get ov 'source-header)
                    (setq bufstr (concat (buffer-substring
                                          (overlay-start ov) (overlay-end ov))
                                         bufstr)))
                  (when (eq 'helm-visible-mark (overlay-get ov 'face))
                    (let ((str (buffer-substring (overlay-start ov) (overlay-end ov))))
                      (unless (equal "" str) (setq mark t))
                      (setq bufstr (concat (buffer-substring
                                            (overlay-start ov) (overlay-end ov))
                                           bufstr)))))
                (overlays-in (point-min) (point-max)))
          (if mark
              (progn (setq bufstr (concat "Helm Multi Swoop\n" bufstr))
                     (setq mark nil))
            (setq bufstr (concat "Helm Multi Swoop\n"
                                 (buffer-substring
                                  (point-min) (point-max)))))))

      ;; Set for edit buffer
      (insert bufstr)
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t front-sticky t))

      ;; Set for editable context
      (let ((inhibit-read-only t))
        ;; Title and explanation
        (goto-char (point-min))
        (let ((ov (make-overlay (point) (point-at-eol))))
          (overlay-put ov 'helm-multi-swoop-edit t)
          (overlay-put ov 'face 'font-lock-function-name-face)
          (overlay-put ov 'after-string
                       (propertize
                        (substitute-command-keys
                         (concat
                          " [\\<helm-swoop-edit-map>\\[helm-swoop--edit-complete]] Complete"
                          ", [\\<helm-swoop-edit-map>\\[helm-swoop--edit-cancel]] Cancel"
                          ", [\\<helm-swoop-edit-map>\\[helm-swoop--edit-delete-all-lines]] Delete All"))
                        'face 'helm-bookmark-addressbook)))
        ;; Line number and editable area
        (while (re-search-forward "^\\([0-9]+\s\\)\\(.*\\)$" nil t)
          (let* ((bol1 (match-beginning 1))
                 (eol1 (match-end 1))
                 (bol2 (match-beginning 2))
                 (eol2 (match-end 2)))

            ;; Line number
            (add-text-properties bol1 eol1
                                 '(face font-lock-function-name-face
                                        intangible t))
            ;; Editable area
            (remove-text-properties bol2 eol2 '(read-only t))
            ;; (add-text-properties bol2 eol2 '(font-lock-face helm-match))

            ;; For line tail
            (set-text-properties eol2 (or (1+ eol2) (point-max))
                                 '(read-only t rear-nonsticky t))))
        (helm-swoop--target-word-overlay 'edit-buffer 0))))

  (other-window 1)
  (switch-to-buffer helm-multi-swoop-edit-buffer)
  (goto-char (point-min))
  (if (string-match "^[0-9]+" candidate)
      (re-search-forward
       (concat "^" (match-string 0 candidate)) nil t))
  (use-local-map helm-multi-swoop-edit-map))

(defun helm-multi-swoop--separate-text-property-into-list (prop)
  "Separete text property into list for PROP."
  (interactive)
  (let (list end)
    (save-excursion
      (goto-char (point-min))
      (while (setq end (next-single-property-change (point) prop))
        ;; Must eliminate last return because of unexpected edit result
        (setq list (cons
                    (let ((str (buffer-substring-no-properties (point) end)))
                      (if (string-match "\n\n\\'" str)
                          (replace-regexp-in-string "\n\\'" "" str)
                        str))
                    list))
        (goto-char end))
      (setq list (cons (buffer-substring-no-properties (point) (point-max))
                       list)))
    (nreverse list)))

(defun helm-multi-swoop--collect-edited-lines ()
  "Create a list of edited lines with each its own line number."
  (interactive)
  (let* ((list (helm-multi-swoop--separate-text-property-into-list 'helm-header))
         (length (length list))
         (i 1) ;; 0th list is header
         pairs)
    (while (<= i length)
      (let (contents)
        ;; Make ((number . line) (number . line) (number . line) ...)
        (with-temp-buffer
          (insert (format "%s" (nth (1+ i) list)))
          (goto-char (point-min))
          (while (re-search-forward "^\\([0-9]+\\)\s" nil t)
            (setq contents
                  (cons (cons (string-to-number (match-string 1))
                              (buffer-substring-no-properties
                               (point)
                               (save-excursion
                                 (if (re-search-forward
                                      "^\\([0-9]+\\)\s\\|^\\(\\-+\\)" nil t)
                                     (1- (match-beginning 0))
                                   (goto-char (point-max))
                                   (re-search-backward "\n" nil t)))))
                        contents))))
        ;; Make ((buffer-name (number . line) (number . line) ...)
        ;;       (buffer-name (number . line) (number . line) ...) ...)
        (setq pairs (cons (cons (nth i list) contents) pairs)))
      (setq i (+ i 2)))
    (delete '(nil) pairs)))

(defun helm-multi-swoop--edit-complete ()
  "Delete all extracted lines, and apply change to buffers and kill temporary edit buffer."
  (interactive)
  (let ((lines (helm-multi-swoop--collect-edited-lines))
        read-only)
    (mapc (lambda (elm)
            (with-current-buffer (car elm)
              (unless buffer-read-only
                (save-excursion
                  (mapc (lambda (cell)
                          (let ((k (car cell))
                                (v (cdr cell)))
                            (goto-char (point-min))
                            (delete-region (point-at-bol k) (point-at-eol k))
                            (goto-char (point-at-bol k))
                            (insert v)))
                        (cdr elm))))
              (if helm-multi-swoop-edit-save
                  (if buffer-read-only
                      (setq read-only t)
                    (save-buffer)))))
          lines)
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-multi-swoop-edit-buffer))
    (if read-only
        (message "Couldn't save some buffers because of read-only")
      (message "Successfully helm-multi-swoop-edit applied to original buffer"))))

(defun helm-multi-swoop--edit-delete-all-lines ()
  "Delete all extracted lines, and apply change to buffers and kill temporary edit buffer."
  (interactive)
  (let ((lines (helm-multi-swoop--collect-edited-lines))
        read-only)
    (mapc (lambda (elm)
            (with-current-buffer (car elm)
              (unless buffer-read-only
                (save-excursion
                  (mapc (lambda (cell)
                          (let ((k (car cell)))
                            (goto-char (point-min))
                            (delete-region (point-at-bol k)
                                           (if (eq (point-at-eol k) (point-max))
                                               (point-at-eol k)
                                             (1+ (point-at-eol k))))))
                        (cdr elm))))
              (if helm-multi-swoop-edit-save
                  (if buffer-read-only
                      (setq read-only t)
                    (save-buffer)))))
          lines)
    (select-window helm-swoop-synchronizing-window)
    (kill-buffer (get-buffer helm-multi-swoop-edit-buffer))
    (if read-only
        (message "Couldn't save some buffers because of read-only")
      (message "Successfully helm-multi-swoop-edit applied to original buffer"))))

(defun helm-multi-swoop--edit-cancel ()
  "Cancel edit and kill temporary buffer."
  (interactive)
  (select-window helm-swoop-synchronizing-window)
  (kill-buffer (get-buffer helm-multi-swoop-edit-buffer))
  (message "helm-multi-swoop-edit canceled"))

;;;###autoload
(defun helm-multi-swoop-edit ()
  "Multi swoop edit."
  (interactive)
  (helm-exit-and-execute-action 'helm-multi-swoop--edit))

;;; @ helm-swoop-same-face-at-point -----------------------------------

(defsubst helm-swoop--get-at-face (&optional point)
  "Get at face at POINT."
  (or point (setq point (point)))
  (let ((face (or (get-char-property point 'read-face-name)
                  (get-char-property point 'face))))
    face))

(defun helm-swoop--cull-face-include-line (face)
  "Cull face include line for FACE."
  (let ((po (point-min)) res)
    (save-excursion
      (while (setq po (next-single-property-change po 'face))
        (when (equal face (helm-swoop--get-at-face po))
          (goto-char po)
          (setq res (cons (format "%s %s"
                                  (line-number-at-pos po)
                                  (buffer-substring (point-at-bol) (point-at-eol)))
                          res))
          (let ((ov (make-overlay po (or (next-single-property-change po 'face)
                                         (point-max)))))
            (overlay-put ov 'face 'helm-swoop-target-word-face)
            (overlay-put ov 'target-buffer 'helm-swoop-target-word-face)))))
    (nreverse (delete-dups res))))

(defun helm-swoop-same-face-at-point (&optional face)
  "Same face at point for FACE."
  (interactive)
  (or face (setq face (helm-swoop--get-at-face)))
  (helm-swoop :query ""
              :source
              `((name . "helm-swoop-same-face-at-point")
                (candidates . ,(helm-swoop--cull-face-include-line face))
                (header-line . ,(format "%s" face))
                (action
                 . (("Go to Line"
                     . (lambda (line)
                         (helm-swoop--goto-line
                          (when (string-match "^[0-9]+" line)
                            (string-to-number (match-string 0 line))))
                         (let ((po (point))
                               (poe (point-at-eol)))
                           (while (<= (setq po (next-single-property-change po 'face)) poe)
                             (when (eq 'helm-swoop-target-word-face (helm-swoop--get-at-face po))
                               (goto-char po))))
                         (helm-swoop--recenter)))
                    ("Edit" . helm-swoop--edit))))))

(defun helm-multi-swoop-same-face-at-point (&optional face)
  "Same face at point for FACE."
  (interactive)
  (or face (setq face (helm-swoop--get-at-face)))
  (helm-multi-swoop--exec
   nil
   :query ""
   :func (lambda () (helm-swoop--cull-face-include-line face))
   :action (lambda (line)
             (switch-to-buffer (assoc-default 'name (helm-get-current-source)))
             (helm-swoop--goto-line
              (when (string-match "^[0-9]+" line)
                (string-to-number (match-string 0 line))))
             (let ((po (point))
                   (poe (point-at-eol)))
               (while (<= (setq po (next-single-property-change po 'face)) poe)
                 (when (eq 'helm-swoop-target-word-face (helm-swoop--get-at-face po))
                   (goto-char po))))
             (helm-swoop--recenter))
   :buflist (helm-multi-swoop--get-buffer-list)))

(provide 'helm-swoop)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; helm-swoop.el ends here
