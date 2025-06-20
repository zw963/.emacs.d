;; -*- lexical-binding: t; -*-

;;; gitignore-mode.el --- Major mode for editing .gitignore files  -*- lexical-binding:t -*-

;; Copyright (c) 2012-2013 Sebastian Wiesner
;; Copyright (C) 2012-2024 The Magit Project Contributors

;; Author: Sebastian Wiesner <lunaryorn@gmail.com>
;; Maintainer: Jonas Bernoulli <emacs.git-modes@jonas.bernoulli.dev>
;; Homepage: https://github.com/magit/git-modes
;; Keywords: convenience vc git

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing .gitignore files.

;;; Code:

(require 'compat)
(require 'conf-mode)

(defvar gitignore-mode-font-lock-keywords
  '(("^\\s<.*$"   . font-lock-comment-face)
    ("^!"         . font-lock-negation-char-face) ; Negative pattern
    ("/"          . font-lock-constant-face)      ; Directory separators
    ("[*?]"       . font-lock-keyword-face)       ; Glob patterns
    ("\\[.+?\\]"  . font-lock-keyword-face)))     ; Ranged glob patterns

;;;###autoload
(define-derived-mode gitignore-mode conf-unix-mode "Gitignore"
  "A major mode for editing .gitignore files."
  (conf-mode-initialize "#")
  ;; Disable syntactic font locking, because comments are only valid at
  ;; beginning of line.
  (setq font-lock-defaults '(gitignore-mode-font-lock-keywords t t))
  (setq-local conf-assignment-sign nil))

;;;###autoload
(dolist (pattern (list "/\\.gitignore\\'"
                       "/info/exclude\\'"
                       "/git/ignore\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'gitignore-mode)))

;;; _
(provide 'gitignore-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; gitignore-mode.el ends here
