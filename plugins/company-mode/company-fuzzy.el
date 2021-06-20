;;; company-fuzzy.el --- Fuzzy matching for `company-mode'  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Shen, Jen-Chieh
;; Created date 2019-08-01 16:54:34

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Fuzzy matching for `company-mode'.
;; Keyword: auto auto-complete complete fuzzy matching
;; Version: 1.2.1
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (s "1.12.0") (ht "2.0"))
;; URL: https://github.com/jcs-elpa/company-fuzzy

;; This file is NOT part of GNU Emacs.

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
;;
;; Fuzzy matching for `company-mode'.
;;

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'ffap)
(require 's)
(require 'subr-x)
(require 'ht)

(defgroup company-fuzzy nil
  "Fuzzy matching for `company-mode'."
  :prefix "company-fuzzy-"
  :group 'company
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/company-fuzzy"))

(defcustom company-fuzzy-sorting-backend 'alphabetic
  "Type for sorting/scoring backend."
  :type '(choice (const :tag "none" none)
                 (const :tag "alphabetic" alphabetic)
                 (const :tag "flx" flx))
  :group 'company-fuzzy)

(defcustom company-fuzzy-prefix-on-top t
  "Have the matching prefix on top."
  :type 'boolean
  :group 'company-fuzzy)

(defcustom company-fuzzy-sorting-function nil
  "Function that gives all candidates and let you do your own sorting."
  :type '(choice (const :tag "None" nil)
                 function)
  :group 'company-fuzzy)

(defcustom company-fuzzy-sorting-score-function nil
  "Function that gives candidates with same score and let you do your own sorting."
  :type '(choice (const :tag "None" nil)
                 function)
  :group 'company-fuzzy)

(defcustom company-fuzzy-show-annotation t
  "Show annotation from source."
  :type 'boolean
  :group 'company-fuzzy)

(defcustom company-fuzzy-annotation-format " <%s>"
  "Annotation string format."
  :type 'string
  :group 'company-fuzzy)

(defcustom company-fuzzy-history-backends '(company-yasnippet)
  "List of backends that kept the history to do fuzzy sorting."
  :type 'list
  :group 'company-fuzzy)

(defcustom company-fuzzy-trigger-symbols '("." "->")
  "List of symbols that allow trigger company when there is no prefix."
  :type 'list
  :group 'company-fuzzy)

(defcustom company-fuzzy-completion-separator "[ \t\r\n]\\|\\_<\\|\\_>"
  "Use to identify the completion unit."
  :type 'string
  :group 'company-fuzzy)

(defface company-fuzzy-annotation-face
  '((t (:inherit company-tooltip-annotation)))
  "Face for annotation."
  :group 'company-fuzzy)

(defvar-local company-fuzzy--prefix ""
  "Record down the company current search reg/characters.")

(defvar-local company-fuzzy--backends nil
  "Company fuzzy backends we are going to use.")

(defvar-local company-fuzzy--recorded-backends nil
  "Record down company local backends in current buffer.")

(defvar-local company-fuzzy--is-trigger-prefix-p nil
  "Flag to see if currently completion having a valid prefix.")

(defvar-local company-fuzzy--ht-backends-candidates (ht-create)
  "Store candidates by backend as id.")

(defvar-local company-fuzzy--ht-history (ht-create)
  "Store list data of history data '(backend . candidates)'.")

;;
;; (@* "External" )
;;

(declare-function flx-score "ext:flx.el")

;;
;; (@* "Mode" )
;;

(defun company-fuzzy--enable ()
  "Record down all other backend to `company-fuzzy--backends'."
  (unless company-fuzzy--recorded-backends
    (setq company-fuzzy--recorded-backends company-backends)
    (setq company-fuzzy--backends (company-fuzzy--normalize-backend-list company-fuzzy--recorded-backends))
    (setq-local company-backends '(company-fuzzy-all-other-backends))
    (setq-local company-transformers (append company-transformers '(company-fuzzy--sort-candidates)))
    (advice-add 'company--insert-candidate :before #'company-fuzzy--insert-candidate)))

(defun company-fuzzy--disable ()
  "Revert all other backend back to `company-backends'."
  (when company-fuzzy--recorded-backends
    (setq-local company-backends company-fuzzy--recorded-backends)
    (setq company-fuzzy--recorded-backends nil)
    (setq company-fuzzy--backends nil)
    (setq-local company-transformers (delq 'company-fuzzy--sort-candidates company-transformers))
    (advice-remove 'company--insert-candidate #'company-fuzzy--insert-candidate)))

;;;###autoload
(define-minor-mode company-fuzzy-mode
  "Minor mode 'company-fuzzy-mode'."
  :lighter " ComFuz"
  :group company-fuzzy
  (if company-fuzzy-mode (company-fuzzy--enable) (company-fuzzy--disable)))

(defun company-fuzzy-turn-on-company-fuzzy-mode ()
  "Turn on the 'company-fuzzy-mode'."
  (company-fuzzy-mode 1))

;;;###autoload
(define-globalized-minor-mode global-company-fuzzy-mode
  company-fuzzy-mode company-fuzzy-turn-on-company-fuzzy-mode
  :group 'company-fuzzy
  :require 'company-fuzzy)

;;
;; (@* "Utilies" )
;;

(defun company-fuzzy--valid-candidates-p (candidates)
  "Return non-nil if CANDIDATES is list of valid candidates."
  (ignore-errors (stringp (nth 0 candidates))))

(defun company-fuzzy--symbol-start ()
  "Return symbol start point from current cursor position."
  (ignore-errors
    (save-excursion
      (forward-char -1)
      (re-search-backward company-fuzzy-completion-separator)
      (point))))

(defun company-fuzzy--generic-prefix ()
  "Return the most generic prefix."
  (let ((start (company-fuzzy--symbol-start)))
    (ignore-errors
      (string-trim (substring (buffer-string) (1- start) (1- (point)))))))

(defun company-fuzzy--trigger-prefix-p ()
  "Check if current prefix a trigger prefix."
  (company-fuzzy--contain-list-string company-fuzzy-trigger-symbols
                                      company-fuzzy--prefix))

(defun company-fuzzy--string-match (regexp string &optional start)
  "Safe way to execute function `string-match'.
See function `string-match' for arguments REGEXP, STRING and START."
  (or (ignore-errors (string-match regexp string start))
      (ignore-errors (string-match (regexp-quote regexp) string start))))

(defun company-fuzzy--string-match-p (regexp string &optional start)
  "Safe way to execute function `string-match-p'.
See function `string-match-p' for arguments REGEXP, STRING and START."
  (or (ignore-errors (string-match-p regexp string start))
      (ignore-errors (string-match-p (regexp-quote regexp) string start))))

(defun company-fuzzy--string-prefix-p (prefix string &optional ignore-case)
  "Safe way to execute function `string-prefix-p'.
See function `string-prefix-p' for arguments PREFIX, STRING and IGNORE-CASE."
  (ignore-errors (string-prefix-p prefix string ignore-case)))

(defun company-fuzzy--contain-list-string (in-list in-str)
  "Return non-nil if IN-STR is listed in IN-LIST.

The reverse mean the check from regular expression is swapped."
  (cl-some (lambda (elm) (string= elm in-str)) in-list))

(defun company-fuzzy--contain-list-symbol (in-list in-symbol)
  "Return non-nil if IN-SYMBOL is listed in IN-LIST."
  (cl-some (lambda (elm) (equal elm in-symbol)) in-list))

(defun company-fuzzy--normalize-backend-list (backends)
  "Normalize all BACKENDS as list."
  (let ((result-lst '()))
    (dolist (backend backends)
      (if (listp backend)
          (let ((index 0))
            (dolist (back backend)
              (when (company-fuzzy--string-prefix-p "company-" (symbol-name back))
                (push (nth index backend) result-lst))
              (setq index (1+ index))))
        (push backend result-lst)))
    (setq result-lst (reverse result-lst))
    (cl-remove-duplicates result-lst)))

(defun company-fuzzy--get-backend-by-candidate (candidate)
  "Return the backend symbol by using CANDIDATE as search index."
  (let ((match (ht-find (lambda (_backend cands)
                          (company-fuzzy--contain-list-string cands candidate))
                        company-fuzzy--ht-backends-candidates)))
    (car match)))

(defun company-fuzzy--call-backend (backend command key)
  "Safely call BACKEND by COMMAND and KEY."
  (ignore-errors (funcall backend command key)))

(defun company-fuzzy--backend-command (candidate command)
  "Find the backend from the CANDIDATE then call the COMMAND."
  (let ((backend (company-fuzzy--get-backend-by-candidate candidate)))
    (if (or (string-empty-p candidate) (not backend)) nil
      (company-fuzzy--call-backend backend command candidate))))

;;
;; (@* "Annotation" )
;;

(defun company-fuzzy--get-backend-string (backend)
  "Get BACKEND's as a string."
  (if backend (s-replace "company-" "" (symbol-name backend)) ""))

(defun company-fuzzy--backend-string (candidate backend)
  "Form the BACKEND string by CANDIDATE."
  (if (and company-fuzzy-show-annotation candidate)
      (let ((backend-str (company-fuzzy--get-backend-string backend)))
        (when (string-empty-p backend-str) (setq backend-str "unknown"))
        (propertize
         (format company-fuzzy-annotation-format backend-str)
         'face 'company-fuzzy-annotation-face))
    ""))

(defun company-fuzzy--source-anno-string (candidate backend)
  "Return the source annotation string by CANDIDATE and BACKEND."
  (if (and candidate backend)
      (company-fuzzy--call-backend backend 'annotation candidate)
    ""))

(defun company-fuzzy--extract-annotation (candidate)
  "Extract annotation from CANDIDATE."
  (let* ((backend (company-fuzzy--get-backend-by-candidate candidate))
         (backend-str (company-fuzzy--backend-string candidate backend))
         (orig-anno (company-fuzzy--source-anno-string candidate backend)))
    (concat orig-anno backend-str)))

;;
;; (@* "Highlighting" )
;;

(defun company-fuzzy--pre-render (str &optional annotation-p)
  "Prerender color with STR and flag ANNOTATION-P."
  (unless annotation-p
    (let* ((str-len (length str))
           (prefix (or (company-fuzzy--backend-prefix-candidate str 'match)
                       ""))
           (cur-selection (nth company-selection company-candidates))
           (splitted-section (remove "" (split-string str " ")))
           (process-selection (nth 0 splitted-section))
           (selected (string= cur-selection process-selection))
           (selected-face (if selected
                              'company-tooltip-common-selection
                            'company-tooltip-common))
           (selected-common-face (if selected
                                     'company-tooltip-selection
                                   'company-tooltip))
           (splitted-c (remove "" (split-string prefix ""))))
      (set-text-properties 0 str-len nil str)
      (font-lock-prepend-text-property 0 str-len 'face selected-common-face str)
      (dolist (c splitted-c)
        (let ((pos (company-fuzzy--string-match-p (regexp-quote c) str)))
          (while (and (numberp pos) (< pos str-len))
            (font-lock-prepend-text-property pos (1+ pos) 'face selected-face str)
            (setq pos (company-fuzzy--string-match-p (regexp-quote c) str (1+ pos))))))))
  str)

;;
;; (@* "Sorting / Scoring" )
;;

(defun company-fuzzy--sort-prefix-on-top (candidates)
  "Sort CANDIDATES that match prefix on top of all other selection."
  (let ((prefix-matches '()) prefix)
    (dolist (cand candidates)
      (setq prefix (company-fuzzy--backend-prefix-candidate cand 'match))
      (when (company-fuzzy--string-prefix-p prefix cand)
        (push cand prefix-matches)
        (setq candidates (remove cand candidates))))
    (setq prefix-matches (sort prefix-matches #'string-lessp))
    (setq candidates (append prefix-matches candidates)))
  candidates)

(defun company-fuzzy--sort-candidates (candidates)
  "Sort all CANDIDATES base on type of sorting backend."
  (setq candidates (company-fuzzy--ht-all-candidates))  ; Get all candidates here.
  (unless company-fuzzy--is-trigger-prefix-p
    (cl-case company-fuzzy-sorting-backend
      (none candidates)
      (alphabetic (setq candidates (sort candidates #'string-lessp)))
      (flx
       (require 'flx)
       (let ((scoring-table (ht-create)) (scoring-keys '())
             prefix scoring score)
         (dolist (cand candidates)
           (setq prefix (company-fuzzy--backend-prefix-candidate cand 'match)
                 scoring (flx-score cand prefix)
                 score (if scoring (nth 0 scoring) 0))
           (when scoring
             (ht-set scoring-table score (push cand (ht-get scoring-table score)))))
         ;; Get all keys, and turn into a list.
         (maphash (lambda (score-key _cands) (push score-key scoring-keys)) scoring-table)
         (setq scoring-keys (sort scoring-keys #'>)  ; Sort keys in order.
               candidates '())  ; Clean up, and ready for final output.
         (dolist (key scoring-keys)
           (let ((cands (ht-get scoring-table key)))
             (setq cands (reverse cands))  ; Respect to backend order.
             (when (functionp company-fuzzy-sorting-score-function)
               (setq cands (funcall company-fuzzy-sorting-score-function cands)))
             (setq candidates (append candidates cands)))))))
    (when company-fuzzy-prefix-on-top
      (setq candidates (company-fuzzy--sort-prefix-on-top candidates)))
    (when (functionp company-fuzzy-sorting-function)
      (setq candidates (funcall company-fuzzy-sorting-function candidates))))
  candidates)

;;
;; (@* "Completion" )
;;

(defun company-fuzzy--insert-candidate (candidate)
  "Insertion for CANDIDATE."
  (when company-fuzzy-mode
    ;; NOTE: Here we force to change `company-prefix' so the completion
    ;; will do what we expected.
    (let ((backend (company-fuzzy--get-backend-by-candidate candidate)))
      (setq company-prefix (company-fuzzy--backend-prefix backend 'complete)))))

;;
;; (@* "Prefix" )
;;

(defun company-fuzzy--backend-prefix-complete (backend)
  "Return prefix for each BACKEND while doing completion.

This function is use when function `company-fuzzy--insert-candidate' is
called.  It returns the current selection prefix to prevent completion
completes in an odd way."
  (cl-case backend
    (company-files (company-files 'prefix))
    (t (company-fuzzy--backend-prefix backend 'match))))

(defun company-fuzzy--backend-prefix-match (backend)
  "Return prefix for each BACKEND while matching candidates.

This function is use for scoring and matching algorithm.  It returns a prefix
that best describe the current possible candidate.

For instance, if there is a candidate function `buffer-file-name' and with
current prefix `bfn'.  It will just return `bfn' because the current prefix
does best describe the for this candidate."
  (cl-case backend
    (company-capf (thing-at-point 'symbol))
    (company-files
     ;; NOTE: For `company-files', we will return the last section of the path
     ;; for the best match.
     ;;
     ;; Example, if I have path `/path/to/dir'; then it shall return `dir'.
     (let ((prefix (company-files 'prefix)))
       (when prefix
         (let* ((splitted (split-string prefix "/" t))
                (len-splitted (length splitted))
                (last (nth (1- len-splitted) splitted)))
           last))))
    (company-yasnippet (thing-at-point 'symbol))
    (t company-fuzzy--prefix)))

(defun company-fuzzy--backend-prefix-get (backend)
  "Return prefix for each BACKEND while getting candidates.

This function is use for simplify prefix, in order to get as much candidates
as possible for fuzzy work.

For instance, if I have prefix `bfn'; then most BACKEND will not return
function `buffer-file-name' as candidate.  But with this function will use a
letter `b' instead of full prefix `bfn'.  So the BACKEND will return something
that may be relavent to the first character `b'.

P.S. Not all backend work this way."
  (cl-case backend
    (company-files
     (let ((prefix (company-files 'prefix)))
       (when prefix
         (let* ((splitted (split-string prefix "/" t))
                (len-splitted (length splitted))
                (last (nth (1- len-splitted) splitted))
                (new-prefix prefix))
           (when (< 1 len-splitted)
             (setq new-prefix
                   (substring prefix 0 (- (length prefix) (length last)))))
           new-prefix))))
    (company-yasnippet "")
    (t (ignore-errors (substring company-fuzzy--prefix 0 1)))))

(defun company-fuzzy--backend-prefix (backend type)
  "Get the BACKEND prefix by TYPE."
  (cl-case type
    (complete (company-fuzzy--backend-prefix-complete backend))
    (match (company-fuzzy--backend-prefix-match backend))
    (get (company-fuzzy--backend-prefix-get backend))))

(defun company-fuzzy--backend-prefix-candidate (cand type)
  "Get the backend prefix by CAND and TYPE."
  (let ((backend (company-fuzzy--get-backend-by-candidate cand)))
    (cl-case type
      (complete (company-fuzzy--backend-prefix-complete backend))
      (match (company-fuzzy--backend-prefix-match backend))
      (get (company-fuzzy--backend-prefix-get backend)))))

;;
;; (@* "Fuzzy Matching" )
;;

(defun company-fuzzy--trim-trailing-re (regex)
  "Trim incomplete REGEX.
If REGEX ends with \\|, trim it, since then it matches an empty string."
  (if (company-fuzzy--string-match "\\`\\(.*\\)[\\]|\\'" regex) (match-string 1 regex) regex))

(defun company-fuzzy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (setq str (company-fuzzy--trim-trailing-re str))
  (if (company-fuzzy--string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (concat (match-string 1 str)
              (let ((lst (string-to-list (match-string 2 str))))
                (apply #'concat
                       (cl-mapcar
                        #'concat
                        (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                              lst)))
                        (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                lst))))
              (match-string 3 str))
    str))

(defun company-fuzzy--match-string (prefix candidates)
  "Return new CANDIDATES that match PREFIX."
  (when (stringp prefix)
    (let ((new-cands '()) (fuz-str (company-fuzzy--regex-fuzzy prefix)))
      (dolist (cand candidates)
        (when (company-fuzzy--string-match-p fuz-str cand)
          (push cand new-cands)))
      new-cands)))

;;
;; (@* "Core" )
;;

(defun company-fuzzy--ht-all-candidates ()
  "Return all candidates from the data."
  (let ((all-candidates '()))
    (maphash (lambda (_backend cands)
               (setq all-candidates (append all-candidates cands)))
             company-fuzzy--ht-backends-candidates)
    (delete-dups all-candidates)))

(defun company-fuzzy-all-candidates ()
  "Return the list of all candidates."
  (setq company-fuzzy--ht-backends-candidates (ht-create)  ; Clean up.
        company-fuzzy--is-trigger-prefix-p (company-fuzzy--trigger-prefix-p))
  (dolist (backend company-fuzzy--backends)
    (let ((prefix-get (company-fuzzy--backend-prefix backend 'get))
          (prefix-com (company-fuzzy--backend-prefix backend 'complete))
          temp-candidates)
      (when prefix-get
        (setq temp-candidates (company-fuzzy--call-backend backend 'candidates prefix-get)))
      ;; NOTE: Do the very basic filtering for speed up.
      ;;
      ;; The function `company-fuzzy--match-string' does the very first
      ;; basic filtering in order to lower the performance before sending
      ;; to function `flx-score'.
      (when (and (not company-fuzzy--is-trigger-prefix-p)
                 (company-fuzzy--valid-candidates-p temp-candidates)
                 prefix-com)
        (setq temp-candidates (company-fuzzy--match-string prefix-com temp-candidates)))
      ;; NOTE: History work.
      ;;
      ;; Here we check if BACKEND a history type of backend. And if it does; then
      ;; it will ensure considering the history candidates to the new candidates.
      (when (company-fuzzy--contain-list-symbol company-fuzzy-history-backends backend)
        (let ((cands-history (ht-get company-fuzzy--ht-history backend)))
          (setq temp-candidates (append cands-history temp-candidates))
          (delete-dups temp-candidates)
          (ht-set company-fuzzy--ht-history backend temp-candidates)))
      ;; NOTE: Made the final completion.
      ;;
      ;; This is the final ensure step before processing it to scoring phase.
      ;; We confirm candidates by adding it to `company-fuzzy--ht-backends-candidates'.
      ;; The function `company-fuzzy--valid-candidates-p' is use to ensure the
      ;; candidates returns a list of strings, which this is the current only valid
      ;; type to this package.
      (when (company-fuzzy--valid-candidates-p temp-candidates)
        (delete-dups temp-candidates)
        (ht-set company-fuzzy--ht-backends-candidates backend (copy-sequence temp-candidates))))
    nil))

(defun company-fuzzy--get-prefix ()
  "Set the prefix just right before completion."
  (setq company-fuzzy--is-trigger-prefix-p nil
        company-fuzzy--prefix (or (ignore-errors (company-fuzzy--generic-prefix))
                                  (ffap-file-at-point))))

(defun company-fuzzy-all-other-backends (command &optional arg &rest ignored)
  "Backend source for all other backend except this backend, COMMAND, ARG, IGNORED."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-fuzzy-all-other-backends))
    (prefix (company-fuzzy--get-prefix))
    (annotation (company-fuzzy--extract-annotation arg))
    (candidates (company-fuzzy-all-candidates))
    (pre-render (company-fuzzy--pre-render arg (nth 0 ignored)))
    ((or doc-buffer kind) (company-fuzzy--backend-command arg command))))

(provide 'company-fuzzy)
;;; company-fuzzy.el ends here
