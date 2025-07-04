;;; sis.el --- Minimize manual input source (input method) switching -*- lexical-binding: t; -*-

;; URL: https://github.com/laishulu/emacs-smart-input-source
;; Created: March 27th, 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1"))
;; Version: 1.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package Minimize manual input source (input method) switching.
;;
;;For more information see the README in the GitHub repo.

;;; Code:
(require 'subr-x)

(defvar sis-external-ism "macism"
  "Path of external ism.")

(defvar sis-do-get nil
  "Function to get the current input source.

Should return a string which is the id of the input source.")

(defvar sis-do-set nil
  "Function to set the input source.

Should accept a string which is the id of the input source.")

(defvar sis-english-pattern "[a-zA-Z]"
  "Pattern to identify a character as english.")

(defvar sis-english-source "com.apple.keylayout.US"
  "Input source for english.")

(defvar sis-other-pattern
  "[\u4E00-\u9FFF\u3400-\u4DBF\u3000-\u303F\uFF00-\uFFEF]"
  "Pattern to identify a character as other lang.

Default value is CJK characters and punctuations.")

(defvar sis-other-source "com.sogou.inputmethod.sogou.pinyin"
  "Input source for other lang.")

(defvar sis-blank-pattern "[:blank:]"
  "Pattern to identify a character as blank.")

(defvar sis-auto-refresh-seconds 0.2
  "Idle timer interval to auto refresh input source status from OS.

Emacs-nativ input method don't need it. nil to disable the timer.
Set after the modes may have no effect.")

(defvar sis-change-hook nil
  "Hook to run when input source changes.")

(defvar sis-default-cursor-color nil
  "Default cursor color, used for English.

nil means obtained from the envrionment.")

(defvar sis-other-cursor-color "green"
  "Cursor color for other language.")

(defvar sis-respect-start 'english
  "Switch to specific input source when the /respect mode/ is enabled.")

(defvar sis-respect-evil-normal-escape t
  "<escape> to english in normal state when the /respect mode/ is enabled.")

(defvar sis-respect-minibuffer-triggers (list)
  "Commands trigger to set input source in minibuffer.

Each trigger should be a cons cell: (cons FN DETECTOR).
- FN: function to trigger the context following.
- DETECTOR:
  - args: nil
  - return:
    - nil: left the determination to later detectors.
    - \\='english: english context.
    - \\='other: other language context.

Example of adding a trigger:

  (add-to-list \\='sis-respect-minibuffer-triggers
               (cons \\='org-roam-node-find (lambda () \\='other)))

If no trigger returns a none-nil result, english will be used as default.")

(defvar sis-respect-prefix-and-buffer t
  "Preserve buffer input source when the /respect mode/ is enabled.")

(defvar sis-respect-go-english-triggers nil
  "Triggers to save input source to buffer and then go to english.

Set after the modes may have no effect.")

(defvar sis-respect-restore-triggers nil
  "Triggers to restore the input source from buffer.

Set after the modes may have no effect.")

(defvar sis-prefix-override-keys
  (list "C-c" "C-x" "C-h" "C-r" "M-SPC")
  "Prefix keys to be overrided.")

(defvar sis-prefix-override-recap-triggers
  (list 'evil-local-mode 'yas-minor-mode)
  "Commands trigger the recap of the prefix override.

Some functions take precedence of the override, need to recap after.
Set after the modes may have no effect.")

(defvar sis-context-fixed nil
  "Context is fixed to a specific language in the /follow context mode/.

Possible values:
nil: dynamic context
\\='english: English context
\\='other: other language context.")

(defvar sis-context-detectors
  (list (lambda (&rest _) sis-context-fixed)
        (lambda (back-detect fore-detect)
          (when (sis--context-english-p back-detect fore-detect)
            'english))
        (lambda (back-detect fore-detect)
          (when (sis--context-other-p back-detect fore-detect)
            'other)))

  "Detectors to detect the context.

Each detector should:
- have two arguments:
  - back-detect: which is the result of (sis--back-detect-chars).
  - fore-detect: which is the result of (sis--fore-detect-chars).
- return one of the following values:
  - nil: left the determination to later detectors.
  - \\='english: English context.
  - \\='other: other language context.")

(defvar sis-context-aggressive-line t
  "Aggressively detect context across blank lines.")

(defvar sis-context-hooks
  '(evil-insert-state-entry-hook)
  "Hooks trigger the set of input source following context.")

(defvar sis-context-triggers
  (list '('+org/insert-item-below 'sis--context-line nil)
        '('+org/insert-item-above 'sis--context-line nil))
  "Commands trigger the set of input source following context.

Each trigger should be a list: (FN PRE-FN-DETECTOR POST-FN-DETECTOR).
- FN: function to trigger the context following.
- PRE-FN-DETECTOR:
  - args: none
  - return:
    - nil: left the determination to later detectors.
    - \\='english: English context.
    - \\='other: other language context.
- POST-FN-DETECTOR:
  - args: none
  - return:
    - nil: left the determination to later detectors.
    - \\='english: English context.
    - \\='other: other language context.
Input source will be switched to (or (PRE-FN-DETECTOR) (POST-FN-DETECTOR)) after
FN is invoked.")

(defvar sis--context-triggers-adviced nil "Context triggers adviced.")

(defvar sis-inline-english-activated-hook nil
  "Hook to run after inline english region activated.")

(defvar sis-inline-english-deactivated-hook nil
  "Hook to run after inline english region deactivated.")

(defvar sis-inline-other-activated-hook nil
  "Hook to run after inline other language region activated.")

(defvar sis-inline-other-deactivated-hook nil
  "Hook to run after inline other language region deactivated.")

(defface sis-inline-face
  '()
  "Face of the inline region overlay."
  :group 'sis)

(set-face-attribute
 'sis-inline-face nil
 :foreground (face-attribute 'font-lock-constant-face :foreground)
 :inverse-video t)

(defvar sis-inline-not-max-point t
  "Make sure there are other characters after inline region.

Insert new line when the whole buffer ends with the region, to avoid
autocomplete rendering a large area with the region background.")

(defvar sis-inline-tighten-head-rule 'one
  "Rule to delete head spaces.

Possible values:
0: don't delete space
1: delete 1 space if exists
\\='zero: always ensure no space
\\='one: always ensure one space
custom function: the cursor will be moved to the beginning of the inline region,
                 and the function will be called with an argument which is the
                 end position of the leading whitespaces in the inline region.")

(defvar sis-inline-tighten-tail-rule 'one
  "Rule to delete tail spaces.

Possible values:
0: don't delete space
1: delete 1 space if exists
\\='zero: always ensure no space
\\='one: always ensure one space
custom function: the cursor will be moved to the end of the inline region, and
                   the function will be called with an argument which is the
                   beginning of the tailing whitespaces in the inline region.")

(defvar sis-inline-single-space-close nil
  "Single space closes the inline region.")

(defvar sis-inline-with-english t
  "With the inline region.")

(defvar sis-inline-with-other nil
  "With the inline other lang region.")

;;;###autoload
(define-minor-mode sis-log-mode
  "Log the execution of this package."
  :global t
  :init-value nil)

;;
;; Following symbols are not supposed to be used directly by end user.
;;

(declare-function evil-normal-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-visual-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-motion-state-p "ext:evil-states.el" (&optional state) t)
(declare-function evil-operator-state-p
                  "ext:evil-states.el" (&optional state) t)
(declare-function company--active-p "ext:company.el" () t)
(declare-function company-complete-selection "ext:company.el" () t)
(declare-function mac-input-source "ext:macfns.c" (&optional SOURCE FORMAT) t)
(declare-function mac-select-input-source "ext:macfns.c"
                  (SOURCE &optional SET-KEYBOARD-LAYOUT-OVERRIDE-P) t)
(declare-function w32-get-ime-open-status "ext:w32fns.c" () t)
(declare-function w32-set-ime-open-status "ext:w32fns.c" (status) t)
(defvar transient--showp)
(defvar evil-normal-state-map)

(defun sis--do-nothing-advice (&rest _)
  "Advice to make existing function do nothing.")

(defun sis--original-advice (fn &rest args)
  "Advice to override existing advice on FN with ARGS."
  (apply fn args))

(defsubst sis--string-match-p (regexp str &optional start)
  "Robust wrapper of `string-match-p'.

Works when REGEXP or STR is not a string REGEXP, STR, START all has the same
meanings as `string-match-p'."
  (and (stringp regexp)
       (stringp str)
       (string-match-p regexp str start)))
;;
;; Following codes are mainly about input source manager
;;

(defvar sis--ism nil "The input source manager.")
(defvar sis--ism-inited nil "Input source manager initialized.")

(defvar sis--current nil
  "Current input source.")

(defvar sis--previous nil
  "Previous input source.")

(defvar sis--for-buffer nil
  "Saved buffer input source.")
(make-variable-buffer-local 'sis--for-buffer)

(defvar sis--for-buffer-locked nil
  "Buffer input source is locked.")
(make-variable-buffer-local 'sis--for-buffer-locked)

(defun sis--init-ism ()
  "Init input source manager."
  ;; `sis-do-get' and `sis-do-set' takes the first precedence.

  ;; external ism
  (when (stringp sis-external-ism)
    (let ((ism-path (executable-find sis-external-ism)))
      (when ism-path (setq sis--ism ism-path))))

  ;; try EMP/w32 when do-get or do-set is missing
  (unless (and (functionp sis-do-get)
               (functionp sis-do-set))
    (cond
     ((and (member (window-system) (list 'ns 'mac))
           (fboundp 'mac-input-source))
      ;; EMP
      (setq sis--ism 'emp))
     ((and (equal (window-system) 'w32)
           (fboundp 'w32-get-ime-open-status))
      ;; w32, input sources are fixed
      (setq sis-english-source nil)
      (setq sis-other-source t)
      (setq sis--ism 'w32))))

  ;; make `sis-do-set' and `sis-do-get'
  (when sis--ism
    ;; avoid override user customized sis-do-get
    (unless (functionp sis-do-get)
      (setq sis-do-get (sis--mk-get-fn)))
    ;; avoid override user customized sis-do-set
    (unless (functionp sis-do-set)
      (setq sis-do-set (sis--mk-set-fn))))

  ;; successfully inited
  (when (and (functionp sis-do-get)
             (functionp sis-do-set))
    ;; a t `sis--ism' means customized by `sis-do-get' and `sis-do-set'
    (unless sis--ism (setq sis--ism t)))

  ;; just inited, successfully or not
  (setq sis--ism-inited t))

(defmacro sis--ensure-ism (&rest body)
  "Only run BODY with valid ism."
  `(progn
     (unless sis--ism-inited
       (sis--init-ism))
     (when sis--ism
       ,@body)))

(defmacro sis--ensure-dir (&rest body)
  "Ensure BODY run in home directory."
  `(let ((default-directory "~"))
     ,@body))

(defsubst sis--normalize-to-lang (lang)
  "Normalize LANG in the form of source id or lang to lang."
  (cond
   (; english
    (member lang (list 'english sis-english-source))
    'english)
   (; other
    (member lang (list 'other sis-other-source))
    'other)))

(defsubst sis--normalize-to-source (source)
  "Normalize SOURCE in the form of source id or lang to source."
  (cond
   (; english
    (member source (list 'english sis-english-source))
    sis-english-source)
   (; other
    (member source (list 'other sis-other-source))
    sis-other-source)))

(defun sis--mk-get-fn ()
  "Make a function to be bound to `sis-do-get'."
  (cond
   (; EMP
    (equal sis--ism 'emp)
    #'mac-input-source)
   (;w32
    (equal sis--ism 'w32)
    #'w32-get-ime-open-status)
   (; external ism
    sis--ism
    (lambda ()
      (sis--ensure-dir
       (string-trim (shell-command-to-string sis--ism)))))))

(defun sis--mk-set-fn ()
  "Make a function to be bound to `sis-do-set'."
  (cond
   (; EMP
    (equal sis--ism 'emp)
    (lambda (source) (mac-select-input-source source)))
   (;w32
    (equal sis--ism 'w32)
    #'w32-set-ime-open-status)
   (; external ism
    sis--ism
    (lambda (source)
      (sis--ensure-dir
       (start-process "set-input-source" nil sis--ism source))))))

(defun sis--update-state (source)
  "Update input source state.

SOURCE should be \\='english or \\='other."

  (setq sis--previous sis--current)
  (setq sis--current source)
  (unless sis--for-buffer-locked
    (setq sis--for-buffer source))
  (when (not (eq sis--previous sis--current))
    (run-hooks 'sis-change-hook)))

(defsubst sis--get ()
  "Get the input source id."
  (sis--ensure-ism
   (sis--update-state (sis--normalize-to-lang (funcall sis-do-get)))))

(defsubst sis--set (source)
  "Set the input source according to source SOURCE."
  (sis--ensure-ism
   (sis--update-state (sis--normalize-to-lang source))
   (funcall sis-do-set (sis--normalize-to-source source))
   (when sis-log-mode
     (message "Do set input source: [%s]@%s, for-buffer: %s, locked: %s"
              source (current-buffer)
              sis--for-buffer sis--for-buffer-locked))))

;;;###autoload
(defun sis-get ()
  "Get input source."
  (interactive)
  (sis--get)
  (message (sis--normalize-to-source sis--current)))

(defsubst sis--save-to-buffer ()
  "Save buffer input source."
  (sis--get))

(defsubst sis--restore-from-buffer ()
  "Restore buffer input source."
  (setq sis--for-buffer-locked nil)
  (sis--set (or sis--for-buffer 'english)))

(defun sis--set-english ()
  "Function to set input source to \\='english."
  (sis--set 'english))

(defun sis--set-other ()
  "Function to set input source to \\='other."
  (sis--set 'other))

;;;###autoload
(defun sis-set-english ()
  "Command to set input source to \\='english."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (sis--set-english))

;;;###autoload
(defun sis-set-other ()
  "Command to set input source to \\='other."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (sis--set-other))

;;;###autoload
(defun sis-switch ()
  "Switch input source between \\='english and \\='other."
  (interactive)
  (setq sis--for-buffer-locked nil)
  (cond
   (; current is \\='english
    (eq sis--current 'english)
    (sis--set-other))
   (; current is \\='other
    (eq sis--current 'other)
    (sis--set-english))))

;;;###autoload
(defun sis-ism-lazyman-config (english-source other-source &optional ism-type)
  "Config ism for lazy man.

Run after the modes may have no effect.
ENGLISH-SOURCE: ENGLISH input source, nil means default,
                ignored by ISM-TYPE of \\='fcitx, \\='fcitx5, \\='native,
                \\='w32.
OTHER-SOURCE: OTHER language input source, nil means default,
              ignored by ISM-TYPE of \\='fcitx, \\='fcitx5, \\='w32.
TYPE: TYPE can be \\='native, \\='w32, \\='emp, \\='macism, \\='im-select,
      \\='fcitx, \\='fcitx5,\\='ibus.
      nil TYPE fits both \\='emp and \\='macism."
  (when english-source
    (setq sis-english-source english-source))
  (when other-source
    (setq sis-other-source other-source))
  (when ism-type
    (setq sis-external-ism (pcase ism-type
                             ('native 'native)
                             ('emp 'emp)
                             ('w32 'w32)
                             ('macism "macism")
                             ('im-select "im-select.exe")
                             ('fcitx "fcitx-remote")
                             ('fcitx5 "fcitx5-remote")
                             ('ibus "ibus"))))

  (cond
   (; Emacs native input method, set do-get and do-set
    (eq ism-type 'native)
    (setq default-input-method other-source)
    (setq sis-english-source nil)
    ;; Don't use `input-method-activate-hook',
    ;; because evil will make a buffer local one
    (advice-add 'activate-input-method :filter-return
                (lambda (res)
                  (sis--update-state (sis--normalize-to-lang
                                      current-input-method))
                  res))
    ;; Don't use `input-method-deactivate-hook',
    ;; because evil will make a buffer local one
    (advice-add 'deactivate-input-method :filter-return
                (lambda (res)
                  (sis--update-state (sis--normalize-to-lang
                                      current-input-method))
                  res))
    (setq sis-do-get (lambda() current-input-method))
    (setq sis-do-set #'activate-input-method))
   (; for builtin supoort, use the default do-get and do-set
    (memq ism-type (list nil 'emp 'w32 'macism 'im-select))

    (; for WSL/Windows Subsystem for Linux, use the default do-get, set do-set
     if (eq system-type 'gnu/linux)
     (setq sis-do-set
           (lambda(source)
             (sis--ensure-dir
              (make-process :name "set-input-source"
                            :command (list sis--ism source)
                            :connection-type 'pipe ))))
     t))
   (; fcitx and fcitx5, use the default do-get, set do-set
    (memq ism-type (list 'fcitx 'fcitx5))
    (unless sis-english-source
      (setq sis-english-source "1"))
    (unless sis-other-source
      (setq sis-other-source "2"))
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (pcase source
                          ("1" (start-process "set-input-source"
                                              nil sis--ism "-c"))
                          ("2" (start-process "set-input-source"
                                              nil sis--ism "-o")))))))
   (; ibus, set do-get and do-set
    (eq ism-type 'ibus)
    (setq sis-do-get (lambda ()
                       (sis--ensure-dir
                        (string-trim
                         (shell-command-to-string
                          (format "%s engine" sis--ism))))))
    (setq sis-do-set (lambda(source)
                       (sis--ensure-dir
                        (start-process "set-input-source"
                                       nil sis--ism "engine" source)))))))

;;
;; Following codes are mainly about auto update mode
;;

(defvar sis--auto-refresh-timer nil
  "Timer for `sis--auto-refresh-timer-function'.")

(defvar sis--auto-refresh-manager-timer nil
  "Timer to manage `sis--auto-refresh-timer'.")

(defvar sis--auto-refresh-timer-scale 1
  "Interval scale during this idle period.")

(defun sis--auto-refresh-timer-function ()
  "Auto refresh input source on idle timer."
  (when sis--auto-refresh-timer
    (cancel-timer sis--auto-refresh-timer))
  (sis--save-to-buffer)
  (setq sis--auto-refresh-timer
        (run-with-idle-timer
         ;; every time the wait period increases by auto-refresh-seconds
         (time-add (current-idle-time)
                   (* sis-auto-refresh-seconds sis--auto-refresh-timer-scale))
         nil
         #'sis--auto-refresh-timer-function))
  (setq sis--auto-refresh-timer-scale
        (* 1.05 sis--auto-refresh-timer-scale)))

;;;###autoload
(define-minor-mode sis-auto-refresh-mode
  "Automaticly refresh input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-auto-refresh-mode
    (when sis-auto-refresh-seconds
      (when sis--auto-refresh-manager-timer
        (cancel-timer sis--auto-refresh-manager-timer))
      (setq sis--auto-refresh-manager-timer
            (run-with-idle-timer sis-auto-refresh-seconds t
                                 #'sis--auto-refresh-timer-restart))))
   (; turn off the mode
    (not sis-auto-refresh-mode)
    (when sis--auto-refresh-manager-timer
      (cancel-timer sis--auto-refresh-manager-timer))
    (when sis--auto-refresh-timer (cancel-timer sis--auto-refresh-timer)))))

(defun sis--auto-refresh-timer-restart ()
  "Restart `sis--auto-refresh-timer'."
  (when (and sis-auto-refresh-seconds sis-auto-refresh-mode)
    (setq sis--auto-refresh-timer-scale 1)
    (sis--auto-refresh-timer-function)))

(defun sis--try-enable-auto-refresh-mode ()
  "Try to enable auto refresh mode."
  (when sis-auto-refresh-seconds
    (sis-auto-refresh-mode t)))

;;
;; Following codes are mainly about cursor color mode
;;
(defun sis--reset-default-cursor-color (&rest _)
    "Reset default cursor color to nil."
    (setq sis-default-cursor-color nil))

(defun sis--set-cursor-color-advice (color)
  "Advice for FN of `set-cursor-color' with COLOR.

The advice is needed, because other packages may set cursor color in their own
way."
  (pcase sis--current
    ('english
     (list sis-default-cursor-color))
    ('other
     (list sis-other-cursor-color))
    (_
     color)))

(defun sis--update-cursor-color()
  "Update cursor color according to input source."
  ;; save original cursor color
  (unless sis-default-cursor-color
    (setq sis-default-cursor-color
          (or (when (display-graphic-p)
                (or (cdr (assq 'cursor-color default-frame-alist))
                    (face-background 'cursor)))
              "white")))
  ;; for GUI
  (when (display-graphic-p)
    ;;
    ;;actually which color passed to the function does not matter,
    ;; the advice will take care of it.
    (set-cursor-color sis-default-cursor-color))

  ;; for TUI
  (unless (display-graphic-p)
    (pcase sis--current
      ('english
       (send-string-to-terminal
        (format "\e]12;%s\a" sis-default-cursor-color)))
      ('other
       (send-string-to-terminal
        (format "\e]12;%s\a" sis-other-cursor-color))))))

;;;###autoload
(define-minor-mode sis-global-cursor-color-mode
  "Automaticly change cursor color according to input source."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-global-cursor-color-mode

    ;; auto refresh input source
    (unless (eq sis-external-ism 'native)
      (sis--try-enable-auto-refresh-mode))
    (add-hook 'enable-theme-functions #'sis--reset-default-cursor-color)
    (add-hook 'disable-theme-functions #'sis--reset-default-cursor-color)
    (advice-add 'set-cursor-color :filter-args #'sis--set-cursor-color-advice)
    (add-hook 'sis-change-hook #'sis--update-cursor-color))
   (; turn off the mode
    (not sis-global-cursor-color-mode)
    (sis--try-disable-auto-refresh-mode)
    (remove-hook 'enable-theme-functions #'sis--reset-default-cursor-color)
    (remove-hook 'disable-theme-functions #'sis--reset-default-cursor-color)
    (advice-remove 'set-cursor-color #'sis--set-cursor-color-advice)
    (remove-hook 'sis-change-hook #'sis--update-cursor-color))))

;;
;; Following codes are mainly about respect mode
;;

(defvar sis--prefix-override-map-alist nil
  "Map alist for override.")

(defvar sis--prefix-handle-stage 'normal
  "Processing state of the prefix key.

Possible values: \\='normal, \\='prefix, \\='sequence.")

(defvar sis--buffer-before-prefix nil
  "Current buffer before prefix.")

(defvar sis--buffer-before-command nil
  "Current buffer before prefix.")

(defvar sis--real-this-command nil
  "Real this command. Some commands overwrite it.")

(defvar sis--respect-post-cmd-timer nil
  "Timer to run after returning to command loop.")

(defvar sis--respect-go-english nil
  "Go english.")

(defvar sis--respect-force-restore nil
  "Force restore after command finishes.")

(defvar sis--prefix-override-order -1000
  "Order of the prefix override in `emulation-mode-map-alists'.")

(defun sis--respect-go-english-advice (&rest _)
  "Advice for `sis-respect-go-english-triggers'."
  (sis--save-to-buffer)
  (when sis-log-mode
    (message "go-english-advice: %s@%s, %s@locked"
             sis--for-buffer (current-buffer)
             sis--for-buffer-locked))
  (setq sis--for-buffer-locked t)
  (sis--set-english)
  (setq sis--respect-go-english t))

(defun sis--respect-restore-advice (fn &rest args)
  "Advice for FN in `sis-respect-restore-triggers' with ARGS args."
  (unwind-protect (apply fn args)
    (when sis-log-mode
      (message "restore-advice: %s@%s, %s@locked"
               sis--for-buffer (current-buffer)
               sis--for-buffer-locked))
    (setq sis--respect-go-english nil)
    (setq sis--respect-force-restore t)))

(defvar sis--prefix-override-map-enable nil
  "Enabe the override keymap.")

;;;###autoload
(defun sis-prefix-override-buffer-disable ()
  "Disable prefix override in current buffer."
  (interactive)
  (make-local-variable
   'sis--prefix-override-map-enable)
  (setq sis--prefix-override-map-enable nil))

;;;###autoload
(defun sis-prefix-override-buffer-enable ()
  "Disable prefix override in current buffer."
  (interactive)
  (when (local-variable-p 'sis--prefix-override-map-enable)
    (kill-local-variable 'sis--prefix-override-map-enable)))

(defun sis--prefix-override-recap-do ()
  "Recap prefix key override."
  (add-to-ordered-list
   'emulation-mode-map-alists
   'sis--prefix-override-map-alist
   sis--prefix-override-order))

(defun sis--prefix-override-recap-advice (fn &rest args)
  "Advice for FN of `prefix-override-recap-triggers' with ARGS."
  (unwind-protect (apply fn args)
    (sis--prefix-override-recap-do)))

(defun sis--prefix-override-handler (arg)
  "Prefix key handler with ARG."
  (interactive "P")
  ;; Restore the prefix arg
  (setq prefix-arg arg)
  (prefix-command-preserve-state)
  ;; Push the key back on the event queue
  (setq unread-command-events
        (append (mapcar (lambda (e) (cons t e))
                        (listify-key-sequence (this-command-keys)))
                unread-command-events)))

(defun sis--respect-focus-change-advice ()
  "Advice for `after-focus-change-function'."
  (if (frame-focus-state)
      (sis--respect-focus-in-handler)
    (sis--respect-focus-out-handler)))

(defun sis--respect-focus-out-handler ()
  "Handler for `focus-out-hook'."

  ;; `mouse-drag-region' causes lots of noise.
  (unless (eq this-command 'mouse-drag-region)
    ;; can't use `sis--save-to-buffer' directly
    ;; because OS may has already changed input source
    ;; when other windows get focus.
    ;; so, don't get the current OS input source
    (setq sis--for-buffer-locked t)
    (sis--set-english))

  (when sis-log-mode
    (message "Handle save hook, save [%s] to [%s]."
             sis--for-buffer (current-buffer))))

(defun sis--respect-focus-in-handler ()
  "Handler for `focus-in-hook'."
  (when sis-log-mode
    (message "Handle restore hook, restore [%s] from [%s] ."
             sis--for-buffer (current-buffer)))
  (sis--restore-from-buffer))

(defun sis--respect-pre-command-handler ()
  "Handler for `pre-command-hook' to preserve input source."
  (setq sis--buffer-before-command (current-buffer))
  (setq sis--real-this-command this-command)
  (when sis-log-mode
    (message "pre@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             sis--prefix-handle-stage
             (this-command-keys)
             sis--real-this-command
             (current-buffer)
             sis--prefix-override-map-enable))

  (pcase sis--prefix-handle-stage
    (; current is normal stage
     'normal
     (cond
      (; not prefix key
       (not (eq sis--real-this-command #'sis--prefix-override-handler))
       t)

      (; for prefix key
       (eq sis--real-this-command #'sis--prefix-override-handler)

       ;; go to pre@[prefix] directly
       (when sis-log-mode
         (message
          "[%s] is a prefix key, short circuit to prefix phase."
          (this-command-keys)))
       (setq sis--prefix-handle-stage 'prefix)
       (sis--respect-pre-command-handler))))
    (; current is prefix stage
     'prefix
     (setq sis--prefix-override-map-enable nil)
     (setq sis--buffer-before-prefix (current-buffer))
     (sis--save-to-buffer)
     (setq sis--for-buffer-locked t)
     (sis--set-english)
     (when sis-log-mode
       (message "Input source: [%s] (saved) => [%s]."
                sis--for-buffer sis-english-source)))
    (; current is sequence stage
     'sequence t)))

(defvar sis-prefix-override-buffer-disable-predicates
  (list 'minibufferp
        (;; read only buffer
         lambda ()
         buffer-read-only)
        (;; magit
         lambda ()
         (sis--string-match-p "^magit.*:" (buffer-name)))
        (;; special buffer
         lambda ()
         (let ((normalized-buffer-name
                (downcase (string-trim (buffer-name)))))
           (and (sis--string-match-p "^\*" normalized-buffer-name)
                (not (sis--string-match-p "^\*new\*" normalized-buffer-name))
                (not (sis--string-match-p "^\*dashboard\*"
                                          normalized-buffer-name))
                (not (sis--string-match-p "^\*scratch\*"
                                          normalized-buffer-name))))))
  "Predicates on buffers to disable prefix overriding.")

(defsubst sis--prefix-override-buffer-disable-p ()
  "Final predicate on disabling prefix override in BUFFER."
  (let ((value nil))
    (dolist (p sis-prefix-override-buffer-disable-predicates)
      (setq value (or value (funcall p))))
    value))

(defun sis--respect-post-cmd-timer-fn ()
  "Function for `sis--respect-post-cmd-timer'."
  (when sis-log-mode
    (message "timer@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             sis--prefix-handle-stage
             (this-command-keys)
             sis--real-this-command
             (current-buffer)
             sis--prefix-override-map-enable))

  ;; determine input source
  (cond
   (; go english, nothing need to do
    sis--respect-go-english
    t)
   (; transient buffer shows
    (and (boundp 'transient--showp) transient--showp)
    (setq sis--for-buffer-locked t)
    (sis--set-english))
   (; restore
    (or sis--respect-force-restore
        (not (eq sis--buffer-before-command (current-buffer))))
    ;; entering minibuffer is handled separately.
    ;; some functions like `exit-minibuffer' won't trigger post-command-hook
    (unless (minibufferp)
      (when sis-log-mode
        (message "restore: [%s]@[%s]" sis--for-buffer (current-buffer)))
      (sis--restore-from-buffer)
      (setq sis--respect-force-restore nil))))

  ;; disable prefix override for current buffer
  (when (and (not (local-variable-p 'sis--prefix-override-map-enable))
             (sis--prefix-override-buffer-disable-p))
    (sis-prefix-override-buffer-disable))

  ;; re-enable if prefix override is disabled temporarily
  (unless (local-variable-p 'sis--prefix-override-map-enable)
    (setq sis--prefix-override-map-enable t))

  (setq sis--prefix-handle-stage 'normal)
  (setq sis--respect-post-cmd-timer nil))

(defsubst sis--to-normal-stage()
  "Transite to normal stage."
  ;; for some command, after the command end,
  ;; the command loop may change the current buffer,
  ;; so delay the real processing.
  (unless sis--respect-post-cmd-timer
    (setq sis--respect-post-cmd-timer
          (run-with-timer 0 nil #'sis--respect-post-cmd-timer-fn))))

(defun sis--respect-post-command-handler ()
  "Handler for `post-command-hook' to preserve input source."
  ;; (setq this-command sis--real-this-command)
  (when sis-log-mode
    (message "post@[%s]: [%s]@key [%s]@cmd [%s]@buf [%s]@override."
             sis--prefix-handle-stage
             (this-command-keys)
             sis--real-this-command
             (current-buffer)
             sis--prefix-override-map-enable))
  (pcase sis--prefix-handle-stage
    (; current is prefix stage
     'prefix
     (setq sis--prefix-handle-stage 'sequence))
    (; current is sequence stage
     'sequence
     (cond
      (; still in progress
       (minibufferp)
       (setq sis--prefix-handle-stage 'sequence))
      (; key sequence is canceled
       (not sis--real-this-command)
       (when sis-log-mode (message "Key sequence canceled."))
       (setq sis--respect-force-restore t)
       (sis--to-normal-stage))

      (; end key sequence
       t
       (when sis-log-mode (message "Key sequence ended."))
       (setq sis--respect-force-restore t)
       (sis--to-normal-stage))))
    (; current is normal stage
     'normal
     (sis--to-normal-stage))))

(defun sis--minibuffer-setup-handler ()
  "Handler for `minibuffer-setup-hook'."
  (when sis-log-mode
    (message "enter minibuffer: [%s]@current [%s]@last [%s]@command"
             (current-buffer)
             sis--buffer-before-command
             this-command))

  (let ((res nil))
    (dolist (trigger sis-respect-minibuffer-triggers)
      (let ((cmd (car trigger))
            (detector (cdr trigger)))
        (if (and (not res) (eq this-command cmd))
            (setq res (funcall detector)))))
    (sis--set (or res 'english))))

(defun sis--minibuffer-exit-handler ()
  "Handler for `minibuffer-exit-hook'."
  (when sis-log-mode (message "exit minibuffer: [%s]@command" this-command))
  (setq sis--respect-force-restore t)
  (unless sis--respect-post-cmd-timer
    (setq sis--respect-post-cmd-timer
          (run-with-timer 0 nil #'sis--respect-post-cmd-timer-fn))))

(defun sis--respect-evil ()
  "Respect evil."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'sis-set-english)
    ;; evil's advice cause a lot of trouble
    ;; let sis to manage input method
    (advice-add 'evil-activate-input-method :override
                #'sis--do-nothing-advice)
    (advice-add 'evil-deactivate-input-method :override
                #'sis--do-nothing-advice)
    (advice-add 'ad-Advice-toggle-input-method :override
                #'sis--original-advice)
    (when sis-respect-evil-normal-escape
      (define-key evil-normal-state-map
                  (kbd "<escape>") #'sis-set-english))))

;;;###autoload
(define-minor-mode sis-global-respect-mode
  "Respect buffer/mode by proper input source.

- Respect start: start this mode with specific input source.
- Respect ~evil~: switch to English when leaving ~evil~ ~insert~ mode.
- Respect prefix key: switch to English for \\[Control-c] / \\[Control-x] /
  \\[Control-h].
- Respect buffer: restore buffer input source when it regain focus."
  :global t
  :init-value nil
  (cond
   (; turn on the mode
    sis-global-respect-mode
    (sis--ensure-ism
     ;; /respect mode/ depends on /auto refresh mode/
     (unless (eq sis-external-ism 'native)
       (sis--try-enable-auto-refresh-mode))
     ;; set english when mode enabled
     (when sis-respect-start (sis--set sis-respect-start))

     ;; respect evil
     (sis--respect-evil)
     ;; in case `evil' is loaded after `sis'
     (add-hook 'after-init-hook #'sis--respect-evil)

     (when sis-respect-prefix-and-buffer
       ;; preserve buffer input source
       (add-hook 'pre-command-hook #'sis--respect-pre-command-handler)
       (add-hook 'post-command-hook #'sis--respect-post-command-handler)
       (add-hook 'minibuffer-setup-hook #'sis--minibuffer-setup-handler)
       (add-hook 'minibuffer-exit-hook #'sis--minibuffer-exit-handler)

       (advice-add 'after-focus-change-function :after
                   #'sis--respect-focus-change-advice)

       (dolist (trigger sis-respect-go-english-triggers)
         (advice-add trigger :before #'sis--respect-go-english-advice))

       (dolist (trigger sis-respect-restore-triggers)
         ;; Don't use :filter-return, advice may not run when trigger has error.
         (advice-add trigger :around #'sis--respect-restore-advice))

       ;; set english when prefix key pressed
       (setq sis--prefix-override-map-alist
             `((sis--prefix-override-map-enable
                .
                ,(let ((keymap (make-sparse-keymap)))
                   (dolist (prefix sis-prefix-override-keys)
                     (define-key keymap
                                 (kbd prefix) #'sis--prefix-override-handler))
                   keymap))))

       (setq sis--prefix-override-map-enable t)
       (sis--prefix-override-recap-do)
       (dolist (trigger sis-prefix-override-recap-triggers)
         (advice-add trigger :around
                     #'sis--prefix-override-recap-advice)))))
   (; turn off the mode
    (not sis-global-respect-mode)
    (sis--try-disable-auto-refresh-mode)
    ;; for evil
    (when (featurep 'evil)
      (remove-hook 'evil-insert-state-exit-hook #'sis-set-english)
      (advice-remove 'evil-activate-input-method #'sis--do-nothing-advice)
      (advice-remove 'evil-deactivate-input-method #'sis--do-nothing-advice)
      (advice-remove 'ad-Advice-toggle-input-method #'sis--original-advice)
      (when sis-respect-evil-normal-escape
        (define-key evil-normal-state-map (kbd "<escape>") nil)))

    ;; preserve buffer input source
    (remove-hook 'pre-command-hook #'sis--respect-pre-command-handler)
    (remove-hook 'post-command-hook #'sis--respect-post-command-handler)
    (remove-hook 'minibuffer-setup-hook #'sis--minibuffer-setup-handler)
    (remove-hook 'minibuffer-exit-hook #'sis--minibuffer-exit-handler)

    ;; for preserving buffer input source
    (advice-remove 'after-focus-change-function
                   #'sis--respect-focus-change-advice)

    (dolist (trigger sis-respect-go-english-triggers)
      (advice-remove trigger #'sis--respect-go-english-advice))

    (dolist (trigger sis-respect-restore-triggers)
      (advice-remove trigger #'sis--respect-restore-advice))

    ;; for prefix key
    (setq emulation-mode-map-alists
          (delq 'sis--prefix-override-map-alist
                emulation-mode-map-alists))
    (setq sis--prefix-override-map-enable nil)
    (dolist (trigger sis-prefix-override-recap-triggers)
      (advice-remove trigger #'sis--prefix-override-recap-advice)))))

(defun sis--try-disable-auto-refresh-mode ()
  "Try to disable auto refresh mode."
  (when (or (not sis-auto-refresh-seconds)
            (and (not sis-global-cursor-color-mode)
                 (not sis-global-respect-mode)))
    (sis-auto-refresh-mode -1)))

;;
;; Following codes are mainly about context-mode
;;

(defsubst sis--english-p (str)
  "Predicate on STR has English characters."
  (sis--string-match-p sis-english-pattern str))

(defsubst sis--not-english-p (str)
  "Predicate on STR is has no English characters."
  (not (sis--string-match-p sis-english-pattern str)))

(defsubst sis--other-p (str)
  "Predicate on STR has /other/ language characters."
  (sis--string-match-p sis-other-pattern str))

(defsubst sis--not-other-p (str)
  "Predicate on STR has no /other/ language characters."
  (not (sis--string-match-p sis-other-pattern str)))

(cl-defstruct sis-back-detect ; result of backward detect
  to ; point after first non-blank char in the same line
  char ; first non-blank char at the same line (just before position `to')
  cross-line-to ; point after first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines before the current position
  )

(defun sis--back-detect-chars ()
  "Detect char backward by two step.

  Step 1: backward skip blank in the current line.
  Step 2: backward skip blank across lines."
  (save-excursion
    (skip-chars-backward sis-blank-pattern)
    (let ((to (point))
          (char (char-before (point))))
      (skip-chars-backward (concat sis-blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-before (point))))
        (make-sis-back-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(cl-defstruct sis-fore-detect ; result of forward detect
  to ; point before first non-blank char in the same line
  char ; first non-blank char at the same line (just after position `to')
  cross-line-to ; point before first non-blank char cross lines
  cross-line-char ; first non-blank char cross lines after the current position
  )

(defun sis--fore-detect-chars ()
  "Detect char forward.

  Forward skip blank in the current line."
  (save-excursion
    (skip-chars-forward sis-blank-pattern)
    (let ((to (point))
          (char (char-after (point))))
      (skip-chars-forward (concat sis-blank-pattern "[:cntrl:]"))
      (let ((cross-line-char (char-after (point))))
        (make-sis-fore-detect :to to
                              :char (when char (string char))
                              :cross-line-to (point)
                              :cross-line-char (when cross-line-char
                                                 (string cross-line-char)))))))

(defun sis--context-other-p (back-detect fore-detect &optional position)
  "Predicate for context of other language.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (cross-line-back-to (sis-back-detect-cross-line-to back-detect))
         (cross-line-back-char (sis-back-detect-cross-line-char back-detect))

         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [other]^
      (and (= back-to (or position (point))) (sis--other-p back-char))
      t)
     (; ^[other]
      (and (= fore-to (or position (point))) (sis--other-p fore-char))
      t)
     (; [other lang][blank or not][^][blank or not][not english]
      (and (sis--other-p back-char) (sis--not-english-p fore-char))
      t)
     (; [not english][blank or not][^][blank or not][other lang]
      (and (sis--not-english-p back-char) (sis--other-p fore-char))
      t)
     (; [other lang: to the previous line][blank][^]
      (and (or sis-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (sis--other-p cross-line-back-char))
      t))))

(defun sis--context-english-p (back-detect fore-detect &optional position)
  "Predicate for context of English.

`back-detect' BACK-DETECT and `fore-detect' FORE-DETECT are required.
If POSITION is not provided, then default to be the current position."
  (let* ((back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect))
         (cross-line-back-to (sis-back-detect-cross-line-to back-detect))
         (cross-line-back-char (sis-back-detect-cross-line-char back-detect))

         (fore-to (sis-fore-detect-to fore-detect))
         (fore-char (sis-fore-detect-char fore-detect)))
    (cond
     (; [english]^
      (and (= back-to (or position (point))) (sis--english-p back-char))
      t)
     (; ^[english]
      (and (= fore-to (or position (point))) (sis--english-p fore-char))
      t)
     (; [english][blank or not][^][blank or not][not other]
      (and (sis--english-p back-char) (sis--not-other-p fore-char))
      t)
     (; [not other][blank or not][^][blank or not][english]
      (and (sis--not-other-p back-char) (sis--english-p fore-char))
      t)
     (; [english: to the previous line][blank][^]
      (and (or sis-context-aggressive-line
               (> cross-line-back-to (line-beginning-position 0)))
           (< cross-line-back-to (line-beginning-position))
           (sis--english-p cross-line-back-char))
      t))))

(defun sis--context-line ()
  "Line context."
  (let ((line (thing-at-point 'line t)))
    (cond
     (; has /other/ lang char
      (sis--other-p line)
      'other)
     (; has no /other/ lang char
      (sis--english-p line)
      'english))))

(defun sis--context-guess ()
  "Guest the lang context for the current point."
  (let* ((back-detect (sis--back-detect-chars))
         (fore-detect (sis--fore-detect-chars))
         (context nil))

    (when sis-context-detectors
      (dolist (detector sis-context-detectors)
        (setq context (or context (funcall detector back-detect fore-detect)))))

    context))

;;;###autoload
(define-minor-mode sis-context-mode
  "Switch input source smartly according to context."
  :global nil
  :init-value nil
  (cond
   (; turn on the mode
    sis-context-mode
    (sis--ensure-ism
     (dolist (hook sis-context-hooks)
       (add-hook hook #'sis-context nil t))

     ;; adviced for all, but only take effect when sis-context-mode is enabled
     (unless sis--context-triggers-adviced
       (setq sis--context-triggers-adviced t)
       (dolist (trigger sis-context-triggers)
         (let* ((trigger-fn (nth 0 trigger))
                (pre-detector (nth 1 trigger))
                (post-detector (nth 2 trigger))
                (advice-name (format "sis--context-trigger-advice-%s"
                                     (symbol-name (eval trigger-fn)))))
           ;; dynamically create the advice
           (defalias (intern advice-name)
             `(lambda (fn &rest args)
                (if sis-context-mode
                    (let ((pre-context (and (functionp ,pre-detector)
                                            (funcall ,pre-detector)))
                          (res (apply fn args))
                          (post-context (and (functionp ,post-detector)
                                             (funcall ,post-detector))))
                      (sis--set (or pre-context post-context))
                      res)
                  (apply fn args))))
           ;; Add special property to the advice, so it can be easily removed
           (put (intern advice-name) 'sis--context-trigger-advice t)
           (advice-add (eval trigger-fn) :around (intern advice-name)))))))
   (; turn off the mode
    (not sis-context-mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (dolist (hook sis-context-hooks)
          (remove-hook hook #'sis-context nil))))
    (dolist (trigger sis-context-triggers)
      (let ((trigger-fn (eval (nth 0 trigger))))
        ;; delete advices with property of 'sis--context-trigger-advice
        (advice-mapc (lambda (advice _)
                       (when (get advice 'sis--context-trigger-advice)
                         (advice-remove trigger-fn advice)
                         (unintern advice nil)))
                     trigger-fn)))
    (setq sis--context-triggers-adviced nil))))

;;;###autoload
(define-globalized-minor-mode
  sis-global-context-mode
  sis-context-mode
  sis-context-mode)

;;;###autoload
(defun sis-context ()
  "Follow the context to switch input source."
  (let ((context (sis--context-guess)))
    (when context
      (sis--set context))))

;;
;; Following codes are mainly about the inline region overlay
;;

(defvar sis--inline-overlay nil
  "The active inline overlay.")
(make-variable-buffer-local 'sis--inline-overlay)

(defvar sis--inline-lang nil
  "Language of the active inline overlay.")
(make-variable-buffer-local 'sis--inline-lang)

(defvar sis--inline-first-space-point nil
  "First effective space point to check overlay activation.")
(make-variable-buffer-local 'sis--inline-first-space-point)

(defsubst sis--inline-overlay-start ()
  "Start position of the inline overlay."
  (when sis--inline-overlay
    (overlay-start sis--inline-overlay)))

(defsubst sis--inline-overlay-end ()
  "End position of the inline overlay."
  (when sis--inline-overlay
    (overlay-end sis--inline-overlay)))

;;;###autoload
(define-minor-mode sis-inline-mode
  "English overlay mode for mixed language editing."
  :init-value nil
  (cond
   (; turn on the mode
    sis-inline-mode
    (sis--ensure-ism
     (add-hook 'post-self-insert-hook #'sis--inline-check-to-activate nil t)))
   (; turn off the mode
    (not sis-inline-mode)
    (remove-hook 'post-self-insert-hook #'sis--inline-check-to-activate t))))

;;;###autoload
(define-globalized-minor-mode
  sis-global-inline-mode
  sis-inline-mode
  sis-inline-mode)

(defsubst sis--evil-not-insert-state-p ()
  "In Evil but not at insert state."
  (and (featurep 'evil)
       (or (evil-normal-state-p)
           (evil-visual-state-p)
           (evil-motion-state-p)
           (evil-operator-state-p))))

(defsubst sis--inline-effect-space-inserted-p ()
  "An effective space is inserted."
  (and sis-inline-mode
       (not (overlayp sis--inline-overlay))
       (not (button-at (point)))
       (not (sis--evil-not-insert-state-p))
       ;; around char is <spc> <DBC spc>
       (memq (preceding-char) (list ?\s 12288))))

(defun sis--inline-check-to-activate()
  "Check whether to activate the inline region overlay.

Check the context to determine whether the overlay should be activated or not,
if the answer is yes, then activate the /inline region/, set the
input source to English."
  (let ((effective-space (sis--inline-effect-space-inserted-p)))
    (cond
     (;if not effective space inserted, reset times to 0
      (not effective-space)
      (setq sis--inline-first-space-point nil))
     (;if effective space inserted
      effective-space
      (let* ((back-detect (sis--back-detect-chars))
             (fore-detect (sis--fore-detect-chars)))

        (unless sis--inline-first-space-point
          (setq sis--inline-first-space-point (point)))

        (cond
         (;inline english region
          (and sis-inline-with-english
               (equal sis--for-buffer 'other))
          (sis--inline-activate 'english (1- (point))))

         (;inline other lang region
          (and sis-inline-with-other
               (= (1+ sis--inline-first-space-point) (point))
               (sis--context-english-p back-detect fore-detect (- (point) 2))
               (equal sis--for-buffer 'english))
          (sis--inline-activate 'other (- (point) 2)))))))))

(defun sis--inline-activate (lang start)
  "Activate the inline region overlay from START.

LANG: the inline region language.
START: start position of the inline region."
  (interactive)
  (sis--ensure-ism
   (setq sis--inline-lang lang)
   (when (overlayp sis--inline-overlay)
     (delete-overlay sis--inline-overlay))

   (setq sis--inline-overlay (make-overlay start (point) nil t t ))
   (overlay-put sis--inline-overlay 'face 'sis-inline-face)
   (overlay-put sis--inline-overlay 'keymap
                (let ((keymap (make-sparse-keymap)))
                  (define-key keymap (kbd "RET")
                              #'sis--inline-ret-check-to-deactivate)
                  (define-key keymap (kbd "<return>")
                              #'sis--inline-ret-check-to-deactivate)
                  keymap))
   (add-hook 'post-command-hook #'sis--inline-fly-check-deactivate nil t)
   (sis--set sis--inline-lang))

  (pcase sis--inline-lang
    ('other (run-hooks 'sis-inline-other-activated-hook))
    ('english (run-hooks 'sis-inline-english-activated-hook))))

(defun sis--inline-fly-check-deactivate ()
  "Check whether to deactivate the inline region overlay."
  (interactive)
  (when (and sis-inline-mode
             (overlayp sis--inline-overlay))

    (when sis-inline-not-max-point
      ;; When cursor is at point-max,
      ;; autocomplete may display with a huge inline overlay background.
      (when (= (point) (point-max))
        (save-excursion (insert-char ?\n))))

    ;; In case some package automatically insert \n before EOF,
    ;; then kick \n out of the the overlay
    (when (and (= (char-before (sis--inline-overlay-end))
                  ?\n)
               (< (sis--inline-overlay-start)
                  (sis--inline-overlay-end)))
      (move-overlay sis--inline-overlay
                    (sis--inline-overlay-start)
                    (1- (sis--inline-overlay-end))))

    ;; select input source
    (let* ((back-detect (sis--back-detect-chars))
           (back-to (sis-back-detect-to back-detect)))
      (when (or
             ;; zero length overlay
             (= (sis--inline-overlay-start)
                (sis--inline-overlay-end))
             ;; out of range
             (or(< (point) (sis--inline-overlay-start))
                (> (point) (sis--inline-overlay-end)))
             ;; " inline  ^"
             ;; but not "           ^"
             (and (= (point) (sis--inline-overlay-end))
                  (> back-to (sis--inline-overlay-start))
                  (= (+ (if sis-inline-single-space-close 1 2)
                        back-to) (point))))
        (sis--inline-deactivate)))))

(defun sis--inline-ret-check-to-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  (when (and sis-inline-mode (overlayp sis--inline-overlay))
    ;; company
    (if (and (featurep 'company)
             (company--active-p))
        (company-complete-selection)
      (sis--inline-deactivate))))

(defun sis--inline-deactivate ()
  "Deactivate the inline region overlay."
  (interactive)
  ;; clean up
  (remove-hook 'post-command-hook #'sis--inline-fly-check-deactivate t)

  ;; select input source
  (let* ((back-detect (sis--back-detect-chars))
         (back-to (sis-back-detect-to back-detect))
         (back-char (sis-back-detect-char back-detect)))
    (cond
     (;if in evil but not insert state
      (sis--evil-not-insert-state-p)
      (sis-set-english))
     (;if cursor is not at the end of the overlay
      (and sis-context-mode
           (/= (point) (sis--inline-overlay-end)))
      (sis-context))
     (; inline english region
      (eq sis--inline-lang 'english)
      (sis-set-other))
     ;; [other lang][blank inline overlay]^
     ;; [overlay with trailing blank]^
     ;; (when (or (and (= back-to (sis--inline-overlay-start))
     ;;               (sis--other-p back-char))
     ;;          (and (> back-to (sis--inline-overlay-start))
     ;;               (< back-to (sis--inline-overlay-end))
     ;;               (< back-to (point))))
     ;;  (sis-set-other)))

     (; inline other lang region
      (eq sis--inline-lang 'other)
      (sis-set-english)))
    ;; [not-other][blank inline overlay]^
    ;; [overlay with trailing blank]^
    ;; (when (or (and (= back-to (sis--inline-overlay-start))
    ;;                (sis--not-other-p back-char))
    ;;           (and (> back-to (sis--inline-overlay-start))
    ;;                (< back-to (sis--inline-overlay-end))
    ;;                (< back-to (point))))
    ;;   (sis-set-english))))

    ;; only tighten for none-blank inline region
    (when (and (<= (point) (sis--inline-overlay-end))
               (> back-to (sis--inline-overlay-start)))

      (save-excursion
        (goto-char (sis--inline-overlay-end))
        (let* ((tighten-back-detect (sis--back-detect-chars))
               (tighten-back-to (sis-back-detect-to tighten-back-detect)))
          (when (and (<= tighten-back-to (sis--inline-overlay-end))
                     (> tighten-back-to (sis--inline-overlay-start)))
            (cond
             (; delete 0 space
              (eq sis-inline-tighten-tail-rule 0)
              t)
             (; delete 1 space
              (eq sis-inline-tighten-tail-rule 1)
              (delete-char -1))
             (; always ensure 0 space
              (eq sis-inline-tighten-tail-rule 'zero)
              (delete-region (point) tighten-back-to))
             (; always ensure 1 space
              (eq sis-inline-tighten-tail-rule 'one)
              (delete-region (point) tighten-back-to)
              (insert-char ?\s))
             (;; handled by custom function
              (functionp sis-inline-tighten-tail-rule)
              (funcall sis-inline-tighten-tail-rule tighten-back-to))))))

      ;; move point because of insertion of text adjacent to the saved point
      (when (eq sis-inline-tighten-tail-rule 'one)
        (forward-char))

      (save-excursion
        (goto-char (sis--inline-overlay-start))
        (let* ((tighten-fore-detect (sis--fore-detect-chars))
               (tighten-fore-to (sis-fore-detect-to tighten-fore-detect)))
          (when (> tighten-fore-to (sis--inline-overlay-start))
            (cond
             (; delete 0 space
              (eq sis-inline-tighten-head-rule 0)
              t)
             (; delete 1 space
              (eq sis-inline-tighten-head-rule 1)
              (delete-char 1))
             (; always ensure 0 space
              (eq sis-inline-tighten-head-rule 'zero)
              (delete-region (point) tighten-fore-to))
             (; always ensure 1 space
              (eq sis-inline-tighten-head-rule 'one)
              (delete-region (point) tighten-fore-to)
              (insert-char ?\s))
             (; handled by custom function
              (functionp sis-inline-tighten-head-rule)
              (funcall sis-inline-tighten-head-rule tighten-fore-to))))))))
  (delete-overlay sis--inline-overlay)
  (setq sis--inline-overlay nil)
  (pcase sis--inline-lang
    ('other (run-hooks 'sis-inline-other-deactivated-hook))
    ('english (run-hooks 'sis-inline-english-deactivated-hook))))

(provide 'sis)
;; Local Variables:
;; coding: utf-8
;; End:
;;; sis.el ends here
