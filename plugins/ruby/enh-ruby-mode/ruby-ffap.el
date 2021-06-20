;;; ruby-ffap.el --- emacs lisp mode like FFAP(find file at point) for ruby.

;; Author: Billy.Zheng
;; Version: 0.1.0
;; Created: 2018-11-11
;; Keywords: files, hypermedia, matching, mouse, convenience
;; Package-Requires: ((emacs "24.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Add emacs-lisp-mode like FFAP(find file at point) feature to
;; `ruby-mode' and `enh-ruby-mode'

;; This is original from https://blog.chmouel.com/2008/05/04/ffap-and-ruby-in-emacs/
;; which not worked for now, i do some hack make it worked, better.

;;; Code:

(require 'rvm)

(defvar ruby-ffap-program-name "bundle exec ruby")

(defun ruby-ffap-is-require-p ()
  "Decide current point is inside a require expression."
  (and
   (fourth (syntax-ppss))
   (looking-back "\\b\\(require\\|gem\\)\\b.*")
   ))

(defun ruby-ffap-gem-path(gem)
  "Find the exact GEM path in the bundler, use RVM."
  (when (ruby-ffap-is-require-p)
    (let ((rvm-verbose nil)) (rvm-activate-corresponding-ruby))
    (shell-command-to-string
     (concat
      ruby-ffap-program-name
      " -e "
      "\"ret='()'; \\$LOAD_PATH.each {|p| x=p+'/'+ARGV[0].sub('.rb', '').tr('-','/')+'.rb'; ret=File.expand_path(x) if File.exists?(x); }; printf ret\" "
      gem))))

(eval-after-load "ffap"
  '(push '(enh-ruby-mode . ruby-ffap-gem-path) ffap-alist))
(eval-after-load "ffap"
  '(push '(ruby-mode . ruby-ffap-gem-path) ffap-alist))

(provide 'ruby-ffap)

;;; ruby-ffap.el ends here
