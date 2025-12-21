;;; ee-transient.el --- Transient menu for eee.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ecal Exec

;; Author: EVal Exec <execvy@gmail.com>
;; Keywords: convenience

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:
(require 'cl-lib)
(require 'transient)

(declare-function ediff-regions-internal "ediff")
(declare-function ediff-make-cloned-buffer "ediff-utils")


;; * Helper functions and vars

(defvar-local ee--rewrite-overlays nil
  "List of active rewrite overlays in the buffer.")

(defun ee--rewrite-sanitize-overlays ()
  "Ensure gptel's rewrite overlays in buffer are consistent."
  (setq ee--rewrite-overlays
        (cl-delete-if-not #'overlay-buffer
                          ee--rewrite-overlays)))

(defvar ee--set-buffer-locally nil
  "Set model parameters from `ee-menu' buffer-locally.

Affects the system message too.")

(defun ee--set-with-scope (sym value &optional scope)
  "Set SYMBOL's symbol-value to VALUE with SCOPE.

If SCOPE is t, set it buffer-locally.
If SCOPE is 1, reset it after the next ee-request. (oneshot)
Otherwise, clear any buffer-local value and set its default
global value."
  (pcase scope
    (1 (put sym 'ee-history (symbol-value sym))
       (set sym value)
       (letrec ((restore-value
                 (lambda ()
                   (remove-hook 'ee-post-request-hook restore-value)
                   (run-at-time         ; Required to work around let bindings
                    0 nil (lambda (s)        ; otherwise this change is overwritten!
                            (set s (get s 'ee-history))
                            (put s 'ee-history nil))
                    sym))))
         (add-hook 'ee-post-request-hook restore-value)))
    ('t (set (make-local-variable sym) value))
    (_ (kill-local-variable sym)
       (set sym value))))

(defun ee--get-directive (args)
  "Find the additional directive in the transient ARGS.

Meant to be called when `ee-menu' is active."
  (cl-some (lambda (s) (and (stringp s) (string-prefix-p ":" s)
                       (substring s 1)))
                  args))

(defun ee--instructions-make-overlay (text &optional ov)
  "Make or move overlay OV with TEXT."
  (save-excursion
    ;; Move point to overlay position
    (cond
     ((use-region-p)
      (if (pos-visible-in-window-p (region-beginning))
          (goto-char (region-beginning))))
     ((ee--in-response-p)
      (ee-beginning-of-response)
      (skip-chars-forward "\n \t"))
     (t (text-property-search-backward 'gptel 'response)
        (skip-chars-forward "\n \t")))
    ;; Make overlay
    (if (and ov (overlayp ov))
        (move-overlay ov (point) (point) (current-buffer))
      (setq ov (make-overlay (point) (point) nil t)))
    (overlay-put ov 'before-string nil)
    ;; (unless (or (bobp) (eq (char-before) "\n"))
    ;;   (overlay-put ov 'before-string (propertize "\n" 'font-lock-face 'shadow)))
    (overlay-put ov 'category 'gptel)
    (overlay-put
     ov 'after-string
     (concat (propertize (concat "DIRECTIVE: " text)
                         'font-lock-face '(:inherit shadow :weight bold  :box t))
      "\n"))
    ov))

(defconst ee--read-with-prefix-help
  (concat
   (propertize "(" 'face 'default)
   (propertize "TAB" 'face 'help-key-binding)
   (propertize " to expand, " 'face 'default)
   (propertize "M-n" 'face 'help-key-binding)
   (propertize "/" 'face 'default)
   (propertize "M-p" 'face 'help-key-binding)
   (propertize " for next/previous): " 'face 'default))
  "Help string ;TODO: ")

(defun ee--read-with-prefix (prefix)
  "Show string PREFIX in the minibuffer after the minibuffer prompt.

PREFIX is shown in an overlay.  Repeated calls to this function
will toggle its visibility state."
  (unless (minibufferp)
    (user-error "This command is intended to be used in the minibuffer."))
  (let* ((update
         (lambda (ov s)
           (overlay-put
            ov 'after-string
            (and s (concat (propertize (concat "\n" s "\n") 'face 'shadow)
                           (make-separator-line))))))
         (max-width (- (window-width) (minibuffer-prompt-end)))
         (max (or max-mini-window-height 0.4))
         (max-height (- (or (and (natnump max) max)
                            (floor (* max (frame-height))))
                        5)))
    (when (and prefix (not (string-empty-p prefix)) (> max-height 1))
      (unless visual-line-mode (visual-line-mode 1))
      (goto-char (minibuffer-prompt-end))
      (pcase-let ((`(,prop . ,ov)
                   (get-char-property-and-overlay
                    (point-min) 'gptel)))
        (unless ov
          (setq ov (make-overlay
                    (point-min) (minibuffer-prompt-end) nil t)))
        (pcase prop
          ('partial
           (if (> (length prefix) max-width)
               (progn
                 (overlay-put ov 'gptel 'prefix)
                 (let ((disp-size
                        (cl-loop for char across prefix
                                 for idx upfrom 0
                                 with n = 0 with max-length = (* max-height max-width)
                                 if (eq char ?\n) do (cl-incf n)
                                 if (> n max-height) return idx
                                 if (> idx max-length)
                                 return idx
                                 finally return nil)))
                   (funcall update ov
                            (if disp-size
                                (truncate-string-to-width
                                 prefix disp-size  nil nil 'ellipsis)
                              prefix))))
             (overlay-put ov 'gptel 'hide)
             (funcall update ov nil)))
          ('prefix (overlay-put ov 'gptel 'hide)
                 (funcall update ov nil))
          (_ (overlay-put ov 'gptel 'partial)
             (funcall update ov (truncate-string-to-width
                                 prefix max-width nil nil
                                 'ellipsis))))))))

(defun ee--transient-read-variable (prompt initial-input history)
  "Read value from minibuffer and interpret the result as a Lisp object.

PROMPT, INITIAL-INPUT and HISTORY are as in the Transient reader
documention."
  (ignore-errors
    (read-from-minibuffer prompt initial-input read-expression-map t history)))

(defun ee-system-prompt--format (&optional message)
  "Format the system MESSAGE for display in gptel's transient menus.

Handle formatting for system messages when the active
`ee-model' does not support system messages."
  (setq message (or message ee--system-message))
  (if (ee--model-capable-p 'nosystem)
      (concat (propertize "[No system message support for model "
                          'face 'transient-heading)
              (propertize (ee--model-name ee-model)
                          'face 'warning)
              (propertize "]" 'face 'transient-heading))
    (if message
        (ee--describe-directive
         message (max (- (window-width) 12) 14) "⮐ ")
      "[No system message set]")))

(defvar ee--crowdsourced-prompts-url
  "https://raw.githubusercontent.com/f/awesome-chatgpt-prompts/main/prompts.csv"
  "URL for crowdsourced LLM system prompts.")

(defvar ee--crowdsourced-prompts
  (make-hash-table :test #'equal)
  "Crowdsourced LLM system prompts.")

(defun ee--crowdsourced-prompts ()
  "Acquire and read crowdsourced LLM system prompts.

These are stored in the variable `ee--crowdsourced-prompts',
which see."
  (when (hash-table-p ee--crowdsourced-prompts)
    (when (hash-table-empty-p ee--crowdsourced-prompts)
      (unless ee-crowdsourced-prompts-file
        (run-at-time 0 nil #'ee-system-prompt)
        (user-error "No crowdsourced prompts available"))
      (unless (and (file-exists-p ee-crowdsourced-prompts-file)
                   (time-less-p
                    (time-subtract (current-time) (days-to-time 14))
                    (file-attribute-modification-time
                     (file-attributes ee-crowdsourced-prompts-file))))
        (when (y-or-n-p
               (concat
                "Fetch crowdsourced system prompts from "
                (propertize "https://github.com/f/awesome-chatgpt-prompts" 'face 'link)
                "?"))
          ;; Fetch file
          (message "Fetching prompts...")
          (let ((dir (file-name-directory ee-crowdsourced-prompts-file)))
            (unless (file-exists-p dir) (mkdir dir 'create-parents))
            (if (url-copy-file ee--crowdsourced-prompts-url
                               ee-crowdsourced-prompts-file
                               'ok-if-already-exists)
		(message "Fetching prompts... done.")
              (message "Could not retrieve new prompts.")))))
      (if (not (file-readable-p ee-crowdsourced-prompts-file))
          (progn (message "No crowdsourced prompts available")
                 (call-interactively #'ee-system-prompt))
        (with-temp-buffer
          (insert-file-contents ee-crowdsourced-prompts-file)
          (goto-char (point-min))
          (forward-line 1)
          (while (not (eobp))
            (when-let ((act (read (current-buffer))))
              (forward-char)
              (save-excursion
                (while (re-search-forward "\"\"" (line-end-position) t)
                  (replace-match "\\\\\"")))
              (when-let ((prompt (read (current-buffer))))
                (puthash act prompt ee--crowdsourced-prompts)))
            (forward-line 1)))))
    ee--crowdsourced-prompts))


;; * Transient classes and methods for gptel

(defclass ee-lisp-variable (transient-lisp-variable)
  ((display-nil :initarg :display-nil)  ;String to display if value if nil
   (display-map :initarg :display-map :initform nil)) ;Display string from alist display-map
  "Lisp variables that show :display-nil instead of nil.")

(cl-defmethod transient-format-value ((obj ee-lisp-variable))
  (let ((display-value
         (with-slots (value display-nil display-map) obj
           (cond ((null value) display-nil)
                 (display-map (cdr (assoc value display-map)))
                 (t value)))))
    (propertize
     (if (stringp display-value) display-value (prin1-to-string display-value))
     'face 'transient-value)))

(cl-defmethod transient-infix-set ((obj ee-lisp-variable) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)
           ee--set-buffer-locally))

(defclass ee--switches (ee-lisp-variable)
  ((display-if-true :initarg :display-if-true :initform "True")
   (display-if-false :initarg :display-if-false :initform "False"))
  "Boolean lisp variable class for ee-transient.")

(cl-defmethod transient-infix-read ((obj ee--switches))
  "Cycle through the mutually exclusive switches."
  (not (oref obj value)))

(cl-defmethod transient-format-value ((obj ee--switches))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if value 'transient-inactive-value 'transient-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if value 'transient-value 'transient-inactive-value))))))

(defclass ee--scope (ee--switches)
  ((display-if-true :initarg :display-if-true :initform "buffer")
   (display-if-false :initarg :display-if-false :initform "global"))
  "Singleton lisp variable class for `ee--set-buffer-locally'.

This is used only for setting this variable via `ee-menu'.")

(cl-defmethod transient-infix-read ((obj ee--scope))
  "Cycle through the mutually exclusive switches."
  (with-slots (value) obj
    (pcase value
      ('t (message "Parameters will be set for the next request only"))
      ('nil (message "Parameters will be set buffer-locally"))
      (1 (message "Parameters will be set globally")))
    (pcase value ('t 1) ('nil t) (1 nil))))

(cl-defmethod transient-format-value ((obj ee--scope))
  (with-slots (value display-if-true display-if-false) obj
      (format
       (propertize "(%s)" 'face 'transient-delimiter)
       (concat
        (propertize display-if-false
                    'face (if (null value) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize display-if-true
                    'face (if (eq value t) 'transient-value 'transient-inactive-value))
        (propertize "|" 'face 'transient-delimiter)
        (propertize "oneshot" 'face
                    (if (eql value 1) 'transient-value 'transient-inactive-value))))))

(cl-defmethod transient-infix-set ((obj ee--scope) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

(defclass ee-provider-variable (transient-lisp-variable)
  ((model       :initarg :model)
   (model-value :initarg :model-value)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "Class used for ee-backends.")

(cl-defmethod transient-format-value ((obj ee-provider-variable))
  (propertize (concat
               (ee-backend-name (oref obj value)) ":"
               (ee--model-name
                (buffer-local-value (oref obj model) transient--original-buffer)))
              'face 'transient-value))

(cl-defmethod transient-infix-set ((obj ee-provider-variable) value)
  (pcase-let ((`(,backend-value ,model-value) value))
    (funcall (oref obj set-value)
             (oref obj variable)
             (oset obj value backend-value)
             ee--set-buffer-locally)
    (funcall (oref obj set-value)
             (oref obj model)
             (oset obj model-value model-value)
             ee--set-buffer-locally))
  (transient-setup))

(defclass ee-option-overlaid (transient-option)
  ((display-nil :initarg :display-nil)
   (overlay :initarg :overlay))
  "Transient options for overlays displayed in the working buffer.")

(cl-defmethod transient-format-value ((obj ee-option-overlaid))
  "set up the in-buffer overlay for additional directive, a string.

Also format its value in the Transient menu."
  (let ((value (oref obj value))
        (ov    (oref obj overlay))
        (argument (oref obj argument)))
    ;; Making an overlay
    (if (or (not value) (string-empty-p value))
        (when ov (delete-overlay ov))
      (with-current-buffer transient--original-buffer
        (oset obj overlay (ee--instructions-make-overlay value ov)))
      (letrec ((ov-clear-hook
                (lambda () (when-let* ((ov (oref obj overlay))
                                  ((overlayp ov)))
                        (remove-hook 'transient-exit-hook
                                     ov-clear-hook)
                        (delete-overlay ov)))))
        (add-hook 'transient-exit-hook ov-clear-hook)))
    ;; Updating transient menu display
    (if value
        (propertize (concat argument (truncate-string-to-width value 25 nil nil "..."))
                    'face 'transient-value)
      (propertize
       (concat "(" (symbol-name (oref obj display-nil)) ")")
       'face 'transient-inactive-value))))


;; * Transient Prefixes

;; BUG: The `:incompatible' spec doesn't work if there's a `:description' below it.
;;;###autoload (autoload 'ee-menu "ee-transient" nil t)
(transient-define-prefix ee-menu ()
  "Change parameters of prompt to send to the LLM."
  ;; :incompatible '(("-m" "-n" "-k" "-e"))
  [:description ee-system-prompt--format
   [""
    :if (lambda () (not (ee--model-capable-p 'nosystem)))
    "Instructions"
    ("s" "Set system message" ee-system-prompt :transient t)
    (ee--infix-add-directive)]
   [:pad-keys t
    ""
    "Context"
    (ee--infix-context-add-region)
    (ee--infix-context-add-buffer)
    (ee--infix-context-add-file)
    (ee--infix-context-remove-all)
    (ee--suffix-context-buffer)]]
  [["Request Parameters"
    :pad-keys t
    (ee--infix-variable-scope)
    (ee--infix-provider)
    (ee--infix-max-tokens)
    (ee--infix-num-messages-to-send
     :if (lambda () (and ee-expert-commands
                    (or ee-mode ee-track-response))))
    (ee--infix-temperature :if (lambda () ee-expert-commands))
    (ee--infix-use-context)
    (ee--infix-track-response
     :if (lambda () (and ee-expert-commands (not ee-mode))))
    (ee--infix-track-media
     :if (lambda () (and ee-mode (ee--model-capable-p 'media))))]
   [" <Prompt from"
    ("m" "Minibuffer instead" "m")
    ("y" "Kill-ring instead" "y")
    ""
    ("i" "Respond in place" "i")]
    [" >Response to"
    ("e" "Echo area" "e")
    ("g" "gptel session" "g"
     :class transient-option
     :prompt "Existing or new gptel session: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer
        prompt (generate-new-buffer-name
                (concat "*" (ee-backend-name ee-backend) "*"))
        nil (lambda (buf-name)
              (if (consp buf-name) (setq buf-name (car buf-name)))
              (let ((buf (get-buffer buf-name)))
                (and (buffer-local-value 'ee-mode buf)
                     (not (eq (current-buffer) buf))))))))
    ("b" "Any buffer" "b"
     :class transient-option
     :prompt "Output to buffer: "
     :reader
     (lambda (prompt _ _history)
       (read-buffer prompt (buffer-name (other-buffer)) nil)))
    ("k" "Kill-ring" "k")]]
  [["Send"
    (ee--suffix-send)
    ("M-RET" "Regenerate" ee--regenerate :if ee--in-response-p)]
   [:description (lambda () (concat (and ee--rewrite-overlays "Continue ")
                               "Rewrite"))
    :if (lambda () (or (use-region-p)
                  (and ee--rewrite-overlays
                       (ee--rewrite-sanitize-overlays))))
    ("r"
     (lambda () (if (get-char-property (point) 'ee-rewrite)
               "Iterate" "Rewrite"))
     ee-rewrite)]
   ["Tweak Response" :if ee--in-response-p :pad-keys t
    ("SPC" "Mark" ee--mark-response)
    ("P" "Previous variant" ee--previous-variant
     :if ee--at-response-history-p
     :transient t)
    ("N" "Next variant" ee--previous-variant
     :if ee--at-response-history-p
     :transient t)
    ("E" "Ediff previous" ee--ediff
     :if ee--at-response-history-p)]
   ["Dry Run" :if (lambda () (or ee-log-level ee-expert-commands))
    ("I" "Inspect query (Lisp)"
     (lambda ()
       "Inspect the query that will be sent as a lisp object."
       (interactive)
       (ee--sanitize-model)
       (ee--inspect-query
        (ee--suffix-send
         (cons "I" (transient-args transient-current-command))))))
    ("J" "Inspect query (JSON)"
     (lambda ()
       "Inspect the query that will be sent as a JSON object."
       (interactive)
       (ee--sanitize-model)
       (ee--inspect-query
        (ee--suffix-send
         (cons "I" (transient-args transient-current-command)))
        'json)))]]
  (interactive)
  (ee--sanitize-model)
  (transient-setup 'ee-menu))

;; ** Prefix for setting the system prompt.

(defun ee--setup-directive-menu (sym msg &optional external)
  "Return a list of transient infix definitions for setting gptel
directives.

SYM is the symbol whose value is set to the selected directive..
MSG is the meaning of symbol, used when messaging.
If EXTERNAL is non-nil, include external sources of directives."
  (cl-loop for (type . prompt) in ee-directives
           ;; Avoid clashes with the custom directive key
           with unused-keys = (delete ?s (number-sequence ?a ?z))
           with width = (window-width)
           for name = (symbol-name type)
           for key = (seq-find (lambda (k) (member k unused-keys)) name (seq-first unused-keys))
           do (setq unused-keys (delete key unused-keys))
           ;; The explicit declaration ":transient transient--do-return" here
           ;; appears to be required for Transient v0.5 and up.  Without it, these
           ;; are treated as suffixes when invoking `ee-system-prompt' directly,
           ;; and infixes when going through `ee-menu'.
           ;; TODO: Raise an issue with Transient.
           collect
           (list (key-description (list key))
                 (concat (capitalize name) " "
                         (propertize " " 'display '(space :align-to 20))
                         (propertize
                          (concat "(" (ee--describe-directive prompt (- width 30)) ")")
                          'face 'shadow))
                 `(lambda () (interactive)
                    (message "%s: %s" ,msg ,(ee--describe-directive prompt 100 "⮐ "))
                    (ee--set-with-scope ',sym ',prompt ee--set-buffer-locally))
	         :transient 'transient--do-return)
           into prompt-suffixes
           finally return
           (nconc
            prompt-suffixes
            (list (list "DEL" "None"
                        `(lambda () (interactive)
                           (message "%s unset" ,msg)
                           (ee--set-with-scope ',sym nil ee--set-buffer-locally))
                        :transient 'transient--do-return))
            (and external
                 (list (list "SPC" "Pick crowdsourced prompt"
                             'ee--read-crowdsourced-prompt
		             ;; NOTE: Quitting the completing read when picking a
		             ;; crowdsourced prompt will cause the transient to exit
		             ;; instead of returning to the system prompt menu.
                             :transient 'transient--do-exit))))))

;;;###autoload (autoload 'ee-system-prompt "ee-transient" nil t)
(transient-define-prefix ee-system-prompt ()
  "Set the LLM system message for LLM interactions.

The \"system message\" establishes directives for the chat
session and modifies the behavior of the LLM. Some examples of
system prompts are:

You are a helpful assistant. Answer as concisely as possible.
Reply only with shell commands and no prose.
You are a poet. Reply only in verse.

More extensive system messages can be useful for specific tasks.

Customize `ee-directives' for task-specific prompts."
  [:description ee-system-prompt--format
   [(ee--suffix-system-message)]
   [(ee--infix-variable-scope)]]
   [:class transient-column
    :setup-children
    (lambda (_) (transient-parse-suffixes
            'ee-system-prompt
            (ee--setup-directive-menu
             'ee--system-message "Directive" t)))
    :pad-keys t])


;; * Transient Infixes

;; ** Infixes for context aggregation

(transient-define-infix ee--infix-use-context ()
  "Describe target destination for context injection.

gptel will include with the LLM request any additional context
added with `ee-add'.  This context can be ignored, included
with the system message or included with the user prompt.

Where in the request this context is included depends on the
value of `ee-use-context', set from here."
  :description "Include context"
  :class 'ee-lisp-variable
  :variable 'ee-use-context
  :format " %k %d %v"
  :set-value #'ee--set-with-scope
  :display-nil "No"
  :display-map '((nil    . "No")
                 (system . "with system message")
                 (user   . "with user prompt"))
  :key "-i"
  :reader (lambda (prompt &rest _)
            (let* ((choices '(("No"                  . nil)
                              ("with system message" . system)
                              ("with user prompt"    . user)))
                   (destination (completing-read prompt choices nil t)))
              (cdr (assoc destination choices)))))

;; ** Infixes for model parameters

(transient-define-infix ee--infix-variable-scope ()
  "Set gptel's model parameters and system message in this buffer or globally."
  :argument "scope"
  :variable 'ee--set-buffer-locally
  :class 'ee--scope
  :format "  %k %d %v"
  :key "="
  :description (propertize "Scope" 'face 'transient-inactive-argument))

(transient-define-infix ee--infix-num-messages-to-send ()
  "Number of recent messages to send with each exchange.

By default, the full conversation history is sent with every new
prompt. This retains the full context of the conversation, but
can be expensive in token size. Set how many recent messages to
include."
  :description "previous responses"
  :class 'ee-lisp-variable
  :variable 'ee--num-messages-to-send
  :set-value #'ee--set-with-scope
  :display-nil 'all
  :format " %k %v %d"
  :key "-n"
  :prompt "Number of past messages to include for context (leave empty for all): "
  :reader 'ee--transient-read-variable)

(transient-define-infix ee--infix-max-tokens ()
  "Max tokens per response.

This is roughly the number of words in the response. 100-300 is a
reasonable range for short answers, 400 or more for longer
responses."
  :description "Response length (tokens)"
  :class 'ee-lisp-variable
  :variable 'ee-max-tokens
  :set-value #'ee--set-with-scope
  :display-nil 'auto
  :key "-c"
  :prompt "Response length in tokens (leave empty: default, 80-200: short, 200-500: long): "
  :reader 'ee--transient-read-variable)

(transient-define-infix ee--infix-provider ()
  "AI Provider for Chat."
  :description "GPT Model"
  :class 'ee-provider-variable
  :prompt "Model: "
  :variable 'ee-backend
  :set-value #'ee--set-with-scope
  :model 'ee-model
  :key "-m"
  :reader (lambda (prompt &rest _)
            (cl-loop
             for (name . backend) in ee--known-backends
             nconc (cl-loop for model in (ee-backend-models backend)
                            collect (list (concat name ":" (ee--model-name model))
                                          backend model))
             into models-alist
             with completion-extra-properties =
             `(:annotation-function
               ,(lambda (comp)
		  (let* ((model (nth 2 (assoc comp models-alist)))
			 (desc (get model :description))
			 (caps (get model :capabilities))
			 (context (get model :context-window))
			 (input-cost (get model :input-cost))
			 (output-cost (get model :output-cost))
			 (cutoff (get model :cutoff-date)))
		    (when (or desc caps context input-cost output-cost cutoff)
		      (concat
		       (propertize " " 'display `(space :align-to 40))
		       (when desc (truncate-string-to-width desc 70 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 112))
		       (when caps (truncate-string-to-width (prin1-to-string caps) 21 nil ? t t))
		       " " (propertize " " 'display `(space :align-to 134))
		       (when context (format "%5dk" context))
		       " " (propertize " " 'display `(space :align-to 142))
		       (when input-cost (format "$%5.2f in" input-cost))
		       (if (and input-cost output-cost) "," " ")
		       " " (propertize " " 'display `(space :align-to 153))
		       (when output-cost (format "$%6.2f out" output-cost))
		       " " (propertize " " 'display `(space :align-to 166))
		       cutoff)))))
             finally return
             (cdr (assoc (completing-read prompt models-alist nil t)
                         models-alist)))))

(transient-define-infix ee--infix-temperature ()
  "Temperature of request."
  :description "Temperature (0 - 2.0)"
  :class 'ee-lisp-variable
  :variable 'ee-temperature
  :set-value #'ee--set-with-scope
  :key "-t"
  :prompt "Temperature controls the response randomness (0.0-2.0, leave empty for default): "
  :reader 'ee--transient-read-variable)

(transient-define-infix ee--infix-track-response ()
  "Distinguish between user messages and LLM responses.

When creating a prompt to send to the LLM, gptel distinguishes
between text entered by the user and past LLM responses.  This is
required for multi-turn conversations, and is always the case in
dedicated chat buffers (in `ee-mode').

In regular buffers, you can toggle this behavior here or by
customizing `ee-track-response'.  When response tracking is
turned off, all text will be assigned the \"user\" role when
querying the LLM."
  :description "Track LLM responses"
  :class 'ee--switches
  :variable 'ee-track-response
  :set-value #'ee--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-v")

(transient-define-infix ee--infix-track-media ()
  "Send media from \"standalone\" links in the prompt.

When the active `ee-model' supports it, gptel can send images
or other media from links in the buffer to the LLM.  Only
\"standalone\" links are considered: these are links on their own
line with no surrounding text.

What link types are sent depends on the mime-types the model
supports.  See `ee-track-media' for more information."
  :description "Send media from links"
  :class 'ee--switches
  :variable 'ee-track-media
  :set-value #'ee--set-with-scope
  :display-if-true "Yes"
  :display-if-false "No"
  :key "-I")

;; ** Infixes for adding and removing context

(declare-function ee-context--at-point "ee-context")
(declare-function ee-add "ee-context")

(transient-define-suffix ee--infix-context-add-region ()
  "Add current region to gptel's context."
  :transient 'transient--do-stay
  :key "-r"
  :if (lambda () (or (use-region-p)
                (and (fboundp 'ee-context--at-point)
                     (ee-context--at-point))))
  :description
  (lambda ()
    (if (and (fboundp 'ee-context--at-point)
             (ee-context--at-point))
        "Remove context at point"
      "Add region to context"))
  (interactive)
  (ee-add)
  (transient-setup))

(transient-define-suffix ee--infix-context-add-buffer ()
  "Add a buffer to gptel's context."
  :transient 'transient--do-stay
  :key "-b"
  :description "Add a buffer to context"
  (interactive)
  (ee-add '(4))
  (transient-setup))

(declare-function ee-add-file "ee-context")
(declare-function ee-context-remove-all "ee-context")

(transient-define-suffix ee--infix-context-add-file ()
  "Add a file to gptel's context."
  :transient 'transient--do-stay
  :key "-f"
  :description "Add a file to context"
  (interactive)
  (call-interactively #'ee-add-file)
  (transient-setup))

(transient-define-suffix ee--infix-context-remove-all ()
  "Clear gptel's context."
  :if (lambda () ee-context--alist)
  :transient 'transient--do-stay
  :key "-d"
  :description "Remove all"
  (interactive)
  (when (y-or-n-p "Remove all context? ")
    (ee-context-remove-all)
    (transient-setup)))

;; ** Infix for additional directive

(transient-define-infix ee--infix-add-directive ()
  "Additional directive intended for the next query only.

This is useful to define a quick task on top of a more extensive
or detailed system message.

For example, with code/text selected:

- Rewrite this function to do X while avoiding Y.
- Change the tone of the following paragraph to be more direct.

Or in an extended conversation:

- Phrase you next response in ten words or less.
- Pretend for now that you're an anthropologist."
  :class 'ee-option-overlaid
  ;; :variable 'ee--instructions
  :display-nil 'none
  :overlay nil
  :argument ":"
  :prompt (concat "Add instructions for next request only "
                  ee--read-with-prefix-help)
  :reader (lambda (prompt initial history)
            (let* ((directive
                    (car-safe (ee--parse-directive ee--system-message 'raw)))
                   (cycle-prefix (lambda () (interactive)
                                   (ee--read-with-prefix directive)))
                   (minibuffer-local-map
                    (make-composed-keymap
                     (define-keymap "TAB" cycle-prefix "<tab>" cycle-prefix)
                     minibuffer-local-map))
                   (extra (minibuffer-with-setup-hook cycle-prefix
                            (read-string prompt (or initial " ") history))))
              (unless (string-empty-p extra) extra)))
  :format " %k %d %v"
  :key "d"
  :argument ":"
  :description "Add instruction"
  :transient t)


;; * Transient Suffixes

;; ** Suffix to send prompt

(transient-define-suffix ee--suffix-send (args)
  "Send ARGS."
  :key "RET"
  :description "Send prompt"
  (interactive (list (transient-args
                      (or transient-current-command 'ee-menu))))
  (let ((stream ee-stream)
        (in-place (and (member "i" args) t))
        (output-to-other-buffer-p)
        (backend ee-backend)
        (model ee-model)
        (backend-name (ee-backend-name ee-backend))
        (buffer) (position)
        (callback) (ee-buffer-name)
        (system-extra (ee--get-directive args))
        (dry-run (and (member "I" args) t))
        ;; Input redirection: grab prompt from elsewhere?
        (prompt
         (cond
          ((member "m" args)
           (read-string
            (format "Ask %s: " (ee-backend-name ee-backend))
            (and (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))))
          ((member "y" args)
           (unless (car-safe kill-ring)
             (user-error "`kill-ring' is empty!  Nothing to send"))
           (if current-prefix-arg
               (read-from-kill-ring "Prompt from kill-ring: ")
             (current-kill 0))))))

    ;; Output redirection: Send response elsewhere?
    (cond
     ((member "e" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if resp
                  (message "%s response: %s" backend-name resp)
                (message "%s response error: %s" backend-name (plist-get info :status))))))
     ((member "k" args)
      (setq stream nil)
      (setq callback
            (lambda (resp info)
              (if (not resp)
                  (message "%s response error: %s" backend-name (plist-get info :status))
                (kill-new resp)
                (message "%s response: \"%s\" copied to kill-ring."
                         backend-name
                         (truncate-string-to-width resp 30))))))
     ((setq ee-buffer-name
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "g" s)
                                 (substring s 1)))
                     args))
      (setq output-to-other-buffer-p t)
      (let ((reduced-prompt             ;For inserting into the gptel buffer as
                                        ;context, not the prompt used for the
                                        ;request itself
             (or prompt
                 (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (buffer-substring-no-properties
                    (save-excursion
                      (text-property-search-backward
                       'gptel 'response
                       (when (get-char-property (max (point-min) (1- (point)))
                                                'gptel)
                         t))
                      (point))
                    (ee--at-word-end (point)))))))
        (cond
         ((buffer-live-p (get-buffer ee-buffer-name))
          ;; Insert into existing gptel session
          (progn
            (setq buffer (get-buffer ee-buffer-name))
            (with-current-buffer buffer
              (goto-char (point-max))
              (unless (or buffer-read-only
                          (get-char-property (point) 'read-only))
                (insert reduced-prompt))
              (setq position (point))
              (when (and ee-mode (not dry-run))
                (ee--update-status " Waiting..." 'warning)))))
         ;; Insert into new gptel session
         (t (setq buffer
                  (gptel ee-buffer-name
                         (condition-case nil
                             (ee--get-api-key)
                           ((error user-error)
                            (setq ee-api-key
                                  (read-passwd
                                   (format "%s API key: "
                                           (ee-backend-name
                                            ee-backend))))))
                         reduced-prompt))
            ;; Set backend and model in new session from current buffer
            (with-current-buffer buffer
              (setq ee-backend backend)
              (setq ee-model model)
              (unless dry-run
                (ee--update-status " Waiting..." 'warning))
              (setq position (point)))))))
     ((setq ee-buffer-name
            (cl-some (lambda (s) (and (stringp s) (string-prefix-p "b" s)
                                 (substring s 1)))
                     args))
      (setq output-to-other-buffer-p t)
      (setq buffer (get-buffer-create ee-buffer-name))
      (with-current-buffer buffer (setq position (point)))))

    (prog1 (ee-request prompt
             :buffer (or buffer (current-buffer))
             :position position
             :in-place (and in-place (not output-to-other-buffer-p))
             :stream stream
             :system
             (if system-extra
                 (ee--merge-additional-directive system-extra)
               ee--system-message)
             :callback callback
             :dry-run dry-run)

      (unless dry-run
        (ee--update-status " Waiting..." 'warning))

      ;; NOTE: Possible future race condition here if Emacs ever drops the GIL.
      ;; The HTTP request callback might modify the buffer before the in-place
      ;; text is killed below.
      (when in-place
        ;; Kill the latest prompt
        (let ((beg
               (if (use-region-p)
                   (region-beginning)
                 (save-excursion
                   (text-property-search-backward
                    'gptel 'response
                    (when (get-char-property (max (point-min) (1- (point)))
                                             'gptel)
                      t))
                   (point))))
              (end (if (use-region-p) (region-end) (point))))
          (unless output-to-other-buffer-p
            ;; store the killed text in ee-history
            (ee--attach-response-history
             (list (buffer-substring-no-properties beg end))))
          (kill-region beg end)))

      (when output-to-other-buffer-p
        (message (concat "Prompt sent to buffer: "
                         (propertize ee-buffer-name 'face 'help-key-binding)))
        (display-buffer
         buffer '((display-buffer-reuse-window
                   display-buffer-pop-up-window)
                  (reusable-frames . visible)))))))

(defun ee--merge-additional-directive (additional &optional full)
  "Merge ADDITIONAL gptel directive with the full system message.

The ADDITIONAL directive is typically specified from `ee-menu'
and applies only to the next gptel request, see
`ee--infix-add-directive'.

FULL defaults to the active, full system message.  It may be a
string, a list of prompts or a function, see `ee-directives'
for details."
  (setq full (or full ee--system-message))
  (cl-typecase full
    (string (concat full "\n\n" additional))
    (list (let ((copy (copy-sequence full)))
            (setcar copy (concat (car copy) "\n\n" additional))
            copy))
    (function (lambda () (ee--merge-additional-directive
                     additional (funcall full))))
    (otherwise additional)))

;; Allow calling from elisp
(put 'ee--suffix-send 'interactive-only nil)

;; ** Suffix to regenerate response

(defun ee--regenerate ()
  "Regenerate gptel response at point."
  (interactive)
  (when (ee--in-response-p)
    (pcase-let* ((`(,beg . ,end) (ee--get-bounds))
                 (history (get-char-property (point) 'ee-history))
                 (prev-responses (cons (buffer-substring-no-properties beg end)
                                       history)))
      (when ee-mode                  ;Remove prefix/suffix
        (save-excursion
          (goto-char beg)
          (when (looking-back (concat "\n+" (regexp-quote (ee-response-prefix-string)))
                              (point-min) 'greedy)
            (setq beg (match-beginning 0)))
          (goto-char end)
          (when (looking-at
                 (concat "\n+" (regexp-quote (ee-prompt-prefix-string))))
            (setq end (match-end 0)))))
      (delete-region beg end)
      (ee--attach-response-history prev-responses)
      (call-interactively #'ee--suffix-send))))

;; ** Set system message
(defun ee--read-crowdsourced-prompt ()
  "Pick a crowdsourced system prompt for gptel.

This uses the prompts in the variable
`ee--crowdsourced-prompts', which see."
  (interactive)
  (if (not (hash-table-empty-p (ee--crowdsourced-prompts)))
      (let ((choice
             (completing-read
              "Pick and edit prompt: "
              (lambda (str pred action)
                (if (eq action 'metadata)
                    `(metadata
                      (affixation-function .
                       (lambda (cands)
                         (mapcar
                          (lambda (c)
                            (list c ""
                             (concat (propertize " " 'display '(space :align-to 22))
                              " " (propertize (gethash c ee--crowdsourced-prompts)
                               'face 'completions-annotations))))
                          cands))))
                  (complete-with-action action ee--crowdsourced-prompts str pred)))
              nil t)))
        (when-let ((prompt (gethash choice ee--crowdsourced-prompts)))
            (setq ee--system-message prompt)
            (call-interactively #'ee--suffix-system-message)))
    (message "No prompts available.")))

(transient-define-suffix ee--suffix-system-message (&optional cancel)
  "Edit LLM system message.

CANCEL is used to avoid touching dynamic system messages,
generated from functions."
  :transient 'transient--do-exit
  :description "Set or edit system message"
  :format " %k   %d"
  :key "s"
  (interactive
   (list (and (functionp ee--system-message)
              (not (y-or-n-p
                    "Active directive is dynamically generated: Edit its current value instead?")))))
  (if cancel (progn (message "Edit canceled")
                    (call-interactively #'ee-menu))
    (ee--edit-directive 'ee--system-message)))

;; MAYBE: Eventually can be simplified with string-edit, after we drop support
;; for Emacs 28.2.
(defun ee--edit-directive (sym &optional callback-cmd)
  "Edit a gptel directive in a dedicated buffer.

Store the result in SYM, a symbol.  If CALLBACK-CMD is specified,
it is run after exiting the edit."
  (let ((orig-buf (current-buffer))
        (msg-start (make-marker))
        (directive (symbol-value sym)))
    (when (functionp directive)
      (setq directive (funcall directive)))
    ;; TODO: Handle editing list-of-strings directives
    (with-current-buffer (get-buffer-create "*ee-system*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (text-mode)
        (setq header-line-format
              (concat
               "Edit your system message below and press "
               (propertize "C-c C-c" 'face 'help-key-binding)
               " when ready, or "
               (propertize "C-c C-k" 'face 'help-key-binding)
               " to abort."))
        (insert
         "# Example: You are a helpful assistant. Answer as concisely as possible.\n"
         "# Example: Reply only with shell commands and no prose.\n"
         "# Example: You are a poet. Reply only in verse.\n\n")
        (add-text-properties
         (point-min) (1- (point))
         (list 'read-only t 'face 'font-lock-comment-face))
        ;; TODO: make-separator-line requires Emacs 28.1+.
        ;; (insert (propertize (make-separator-line) 'rear-nonsticky t))
        (set-marker msg-start (point))
        (save-excursion
          ;; If it's a list, insert only the system message part
          (insert (car-safe (ee--parse-directive directive 'raw)))
          (push-mark nil 'nomsg))
        (activate-mark))
      (display-buffer (current-buffer)
                      `((display-buffer-below-selected)
                        (body-function . ,#'select-window)
                        (window-height . ,#'fit-window-to-buffer)))
      (let ((quit-to-menu
             (lambda ()
               "Cancel system message update and return."
               (interactive)
               (quit-window)
               (display-buffer
                orig-buf
                `((display-buffer-reuse-window
                   display-buffer-use-some-window)
                  (body-function . ,#'select-window)))
               (when (commandp callback-cmd)
                 (call-interactively callback-cmd)))))
        (use-local-map
         (make-composed-keymap
          (define-keymap
            "C-c C-c" (lambda ()
                        "Confirm system message and return."
                        (interactive)
                        (let ((system-message
                               (buffer-substring-no-properties msg-start (point-max))))
                          (with-current-buffer orig-buf
                            (ee--set-with-scope sym
                                                   (if (cdr-safe directive) ;Handle list of strings
                                                       (prog1 directive (setcar directive system-message))
                                                     system-message)
                                                   ee--set-buffer-locally)))
                        (funcall quit-to-menu))
            "C-c C-k" quit-to-menu)
          text-mode-map))))))

;; ** Suffix for displaying and removing context
(declare-function ee-context--buffer-setup "ee-context")
(declare-function ee-context--collect "ee-context")

(transient-define-suffix ee--suffix-context-buffer ()
  "Display all contexts from all buffers & files."
  :transient 'transient--do-exit
  :key " C"
  :if (lambda () ee-context--alist)
  :description
  (lambda ()
    (pcase-let*
        ((contexts (and ee-context--alist (ee-context--collect)))
         (buffer-count (length contexts))
         (`(,file-count ,ov-count)
          (if (> buffer-count 0)
              (cl-loop for (buf-file . ovs) in contexts
                       if (bufferp buf-file)
                       sum (length ovs) into ov-count
                       else count (stringp buf-file) into file-count
                       finally return (list file-count ov-count))
            (list 0 0))))
      (concat "Inspect "
              (format
               (propertize "(%s)" 'face 'transient-delimiter)
               (propertize
                (concat
                 (and (> ov-count 0)
                      (format "%d region%s in %d buffer%s"
                              ov-count (if (> ov-count 1) "s" "")
                              (- buffer-count file-count)
                              (if (> ( - buffer-count file-count) 1) "s" "")))
                 (and (> file-count 0)
                      (propertize
                       (format "%s%d file%s"
                               (if (> ov-count 0) ", " "") file-count
                               (if (> file-count 1) "s" "")))))
                'face (if (zerop (length contexts))
                          'transient-inactive-value
                        'transient-value))))))
  (interactive)
  (ee-context--buffer-setup))

(provide 'ee-transient)
;;; ee-transient.el ends here

;; Local Variables:
;; outline-regexp: "^;; \\*+"
;; eval: (outline-minor-mode 1)
;; End:
