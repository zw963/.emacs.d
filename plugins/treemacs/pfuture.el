;; -*- lexical-binding: t; -*-

;;; pfuture.el --- a simple wrapper around asynchronous processes -*- lexical-binding: t -*-

;; Copyright (C) 2020 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Homepage: https://github.com/Alexander-Miller/pfuture
;; Package-Requires: ((emacs "25.2"))
;; Version: 1.10.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'inline)

(defvar pfuture--dummy-buffer nil
  "Dummy buffer for stderr pipes.")

(define-inline pfuture--delete-process (process)
  "Delete PROCESS with redisplay inhibited."
  (inline-letevals (process)
    (inline-quote
     (let ((inhibit-redisplay t))
       (delete-process ,process)))))

(defun pfuture--sentinel (process _)
  "Delete the stderr pipe process of PROCESS."
  (unless (process-live-p process)
    (let* ((stderr-process (process-get process 'stderr-process)))
      ;; Set stderr-process to nil so that await-to-finish does not delete
      ;; the buffer again.
      (process-put process 'stderr-process nil)
      ;; delete-process may trigger other sentinels. If there are many pfutures,
      ;; this will overflow the stack.
      (run-with-idle-timer 0 nil #'pfuture--delete-process stderr-process))
    ;; Make sure the stdout buffer is deleted even if the future
    ;; is never awaited
    (unless (process-get process 'result)
      (let* ((buffer (process-get process 'buffer))
             (result (with-current-buffer buffer (buffer-string))))
        (kill-buffer buffer)
        (process-put process 'result result)))))

;;;###autoload
(defun pfuture-new (&rest cmd)
  "Create a new future process for command CMD.
Any arguments after the command are interpreted as arguments to the command.
This will return a process object with additional \\='stderr and \\='stdout
properties, which can be read via \(process-get process \\='stdout\) and
\(process-get process \\='stderr\) or alternatively with
\(pfuture-result process\) or \(pfuture-stderr process\).

Note that CMD must be a *sequence* of strings, meaning
this is wrong: (pfuture-new \"git status\")
this is right: (pfuture-new \"git\" \"status\")"
  (let ((stderr (make-pipe-process
                 :name " Process Future stderr"
                 ;; Use a dummy buffer for the stderr process. make-pipe-process creates a
                 ;; buffer unless one is specified, even when :filter is specified and the
                 ;; buffer is not used at all.
                 :buffer (or pfuture--dummy-buffer
                             (setq pfuture--dummy-buffer (get-buffer-create " *pfuture stderr dummy*")))
                 :noquery t
                 :filter #'pfuture--append-stderr)))
    ;; Make sure that the same buffer is not shared between processes.
    ;; This is not a race condition, since the pipe is not yet connected and
    ;; cannot receive input.
    (set-process-buffer stderr nil)
    (condition-case err
        (let* ((name (format " Pfuture-Buffer %s" cmd))
               (pfuture-buffer
                (let (buffer-list-update-hook)
                  (generate-new-buffer name)))
               (process
                (make-process
                 :name "Process Future"
                 :stderr stderr
                 :sentinel #'pfuture--sentinel
                 :filter #'pfuture--append-output-to-buffer
                 :command cmd
                 :noquery t))
               ;; Make the processes share their plist so that 'stderr is easily accessible.
               (plist (list 'buffer pfuture-buffer 'stdout "" 'stderr "" 'stderr-process stderr)))
          (set-process-plist process plist)
          (set-process-plist stderr plist)
          process)
      (error
       (pfuture--delete-process stderr)
       (signal (car err) (cdr err))))))

(defmacro pfuture--decompose-fn-form (fn &rest args)
  "Expands into the correct call form for FN and ARGS.
FN may either be a (sharp) quoted function, and unquoted function or an sexp."
  (declare (indent 1))
  (pcase fn
    (`(function ,fn)
     `(,fn ,@args))
    (`(quote ,fn)
     `(,fn ,@args))
    ((or `(,_ . ,_) `(,_))
     fn)
    ((pred null)
     (ignore fn))
    (fn
     `(funcall ,fn ,@args))))

(cl-defmacro pfuture-callback
    (command &key
             on-success
             on-error
             on-status-change
             directory
             name
             connection-type
             buffer
             filter)
  "Pfuture variant that supports a callback-based workflow.
Internally based on `make-process'.  Requires lexical scope.

The first - and only required - argument is COMMAND.  It is an (unquoted) list
of the command and the arguments for the process that should be started.  A
vector is likewise acceptable - the difference is purely cosmetic (this does not
apply when command is passed as a variable, in this case it must be a list).

The rest of the argument list is made up of the following keyword arguments:

ON-SUCCESS is the code that will run once the process has finished with an exit
code of 0. In its context, these variables are bound: `process': The process
object, as passed to the sentinel callback function.  `status': The string exit
status, as passed to the sentinel callback function.  `pfuture-buffer': The
buffer where the output of the process is collected, including both stdin and
stdout.  You can use `pfuture-callback-output' to quickly grab the buffer's
content.

ON-SUCCESS may take one of 3 forms: an unquoted sexp, a quoted function or an
unquoted function.  In the former two cases the passed fuction will be called
with `process', `status' and `buffer' as its arguments.

ON-ERROR is the inverse to ON-SUCCESS; it will only run if the process has
finished with a non-zero exit code.  Otherwise the same conditions apply as for
ON-SUCCESS.

ON-STATUS-CHANGE will run on every status change, even if the process remains
running.  It is meant for debugging and has access to the same variables as
ON-SUCCESS and ON-ERROR, including the (potentially incomplete) process output
buffer.  Otherwise the same conditions as for ON-SUCCESS and ON-ERROR apply.

DIRECTORY is the value given to `default-directory' for the context of the
process.  If not given it will fall back the current value of
`default-directory'.

NAME will be passed to the :name property of `make-process'.  If not given it
will fall back to \"Pfuture Callback [$COMMAND]\".

CONNECTION-TYPE will be passed to the :connection-process property of
`make-process'. If not given it will fall back to \\='pipe.

BUFFER is the buffer that will be used by the process to collect its output,
quickly collectible with `pfuture-output-from-buffer'.
Providing a buffer outside of specific use-cases is not necessary, as by default
pfuture will assign every launched command its own unique buffer and kill it
after ON-SUCCESS or ON-ERROR have finished running. However, no such cleanup
will take place if a custom buffer is provided.

FILTER is a process filter-function (quoted function reference) that can be used
to overwrite pfuture's own filter. By default pfuture uses its filter function
to collect the launched process' output in its buffer, thus when providing a
custom filter output needs to be gathered another way. Note that the process'
buffer is stored in its `buffer' property and is therefore accessible via
\(process-get process \\='buffer\)."
  (declare (indent 1))
  (let* ((command (if (vectorp command)
                      `(quote ,(cl-map 'list #'identity command))
                    command))
         (connection-type (or connection-type (quote 'pipe)))
         (directory (or directory default-directory)))
    (unless (or on-success on-error)
      (setq on-success '(function ignore)))
    `(let* ((default-directory ,directory)
            (name (or ,name (format " Pfuture-Callback %s" ,command)))
            ;; pfuture's buffers are internal implementation details
            ;; nobody should care if a new one is created
            (pfuture-buffer (or ,buffer (let (buffer-list-update-hook) (generate-new-buffer name))))
            (process
             (make-process
              :name name
              :command ,command
              :connection-type ,connection-type
              :filter ,(or filter '(function pfuture--append-output-to-buffer))
              :sentinel (lambda (process status)
                          (ignore status)
                          ,@(when on-status-change
                              `((pfuture--decompose-fn-form ,on-status-change
                                  process status pfuture-buffer)))
                          (unless (process-live-p process)
                            (if (= 0 (process-exit-status process))
                                (pfuture--decompose-fn-form ,on-success
                                  process status pfuture-buffer)
                              (pfuture--decompose-fn-form ,on-error
                                process status pfuture-buffer))
                            ,(unless buffer
                               `(kill-buffer (process-get process 'buffer))))))))
       (process-put process 'buffer pfuture-buffer)
       process)))

(defmacro pfuture-callback-output ()
  "Retrieve the output from the pfuture-buffer variable in the current scope.
Meant to be used with `pfuture-callback'."
  `(pfuture-output-from-buffer pfuture-buffer))

(cl-defun pfuture-await (process &key (timeout 1) (just-this-one t))
  "Block until PROCESS has produced output and return it.

Will accept the following optional keyword arguments:

TIMEOUT: The timeout in seconds to wait for the process.  May be a float to
specify fractional number of seconds.  In case of a timeout nil will be
returned.

JUST-THIS-ONE: When t only read from the process of FUTURE and no other.  For
details see documentation of `accept-process-output'."
  (let (inhibit-quit)
    (accept-process-output
     process timeout nil just-this-one))
  (pfuture-result process))

(define-inline pfuture-result (process)
  "Return the output of a pfuture PROCESS.
If the PROCESS is still alive only the output collected so far will be returned.
To get the full output use either `pfuture-await' or `pfuture-await-to-finish'."
  (declare (side-effect-free t))
  (inline-letevals (process)
    (inline-quote
     (let* ((result (process-get ,process 'result)))
       (cond
        (result result)
        ((process-live-p ,process)
         (let ((buffer (process-get ,process 'buffer)))
           (with-current-buffer buffer (buffer-string))))
        (t
         (let* ((buffer (process-get ,process 'buffer))
                (output (with-current-buffer buffer
                          (buffer-string))))
           (process-put ,process 'result output)
           (kill-buffer buffer)
           output)))))))

(define-inline pfuture-stderr (process)
  "Return the error output of a pfuture PROCESS."
  (declare (side-effect-free t))
  (inline-letevals (process)
    (inline-quote
     (process-get ,process 'stderr))))

(defun pfuture-await-to-finish (process)
  "Keep reading the output of PROCESS until it is done.
Same as `pfuture-await', but will keep reading (and blocking) so long as the
process is *alive*.

If the process never quits this method will block forever.  Use with caution!"
  ;; If the sentinel hasn't run, disable it. We are going to delete
  ;; the stderr process here.
  (set-process-sentinel process nil)
  (let (inhibit-quit)
    (while (accept-process-output process)))
  (let* ((plist (process-plist process))
         (stderr-process (plist-get plist 'stderr-process)))
    (when stderr-process
      (pfuture--delete-process stderr-process))
    (pfuture-result process)))

(defun pfuture--append-output-to-buffer (process msg)
  "Append PROCESS' MSG to its output buffer."
  (with-current-buffer (process-get process 'buffer)
    (goto-char (point-max))
    (insert msg)))

(defun pfuture--append-stderr (process msg)
  "Append PROCESS' MSG to the already saved stderr output."
  (let* ((process-plist (process-plist process))
         (previous-output (plist-get process-plist 'stderr)))
    (plist-put process-plist 'stderr
               (if (zerop (string-bytes previous-output))
                   msg
                 (concat previous-output msg)))))

(define-inline pfuture-output-from-buffer (buffer)
  "Return the process output collected in BUFFER."
  (declare (side-effect-free t))
  (inline-letevals (buffer)
    (inline-quote
     (with-current-buffer ,buffer
       (buffer-string)))))

(provide 'pfuture)

;;; pfuture.el ends here
