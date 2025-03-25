;;; wgrep-pt.el --- Writable pt buffer -*- lexical-binding: t -*-

;; Copyright (C) 2010-2020,2023 Masahiro Hayashi

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>, Bailey Ling <bling@live.ca>
;; Keywords: grep edit extensions
;; Package-Requires: ((emacs "25.1") (wgrep "3.0.0"))
;; URL: http://github.com/mhayashi1120/Emacs-wgrep/raw/master/wgrep-pt.el
;; Emacs: GNU Emacs 25 or later
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; wgrep-pt allows you to edit a pt buffer and apply those changes to
;; the file buffer.

;;; Install:

;; 1. Install pt.el
;;
;;   https://github.com/bling/pt.el

;; 2. Install wgrep.el

;; 3. Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (autoload 'wgrep-pt-setup "wgrep-pt")
;;     (add-hook 'pt-search-mode-hook 'wgrep-pt-setup)

;;; Usage:

;; See wgrep.el

;;; Code:

(require 'wgrep)

;;;###autoload
(defun wgrep-pt-setup ()
  (wgrep-setup-internal))

;;;###autoload
(add-hook 'pt-search-mode-hook 'wgrep-pt-setup)

;; For `unload-feature'
(defun wgrep-pt-unload-function ()
  (remove-hook 'pt-search-mode-hook 'wgrep-pt-setup))

(provide 'wgrep-pt)

;;; wgrep-pt.el ends here
