;;; mu4e-view.el -- part of mu4e, the mu mail user agent -*- lexical-binding: t -*-

;; Copyright (C) 2021 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In this file we define mu4e-view-mode (+ helper functions), which is used for
;; viewing e-mail messages

;;; Code:
(declare-function mu4e~view-gnus "mu4e-view-gnus")
(declare-function mu4e~view-old  "mu4e-view-old")
(declare-function mu4e~headers-update-handler  "mu4e-headers")
(declare-function mu4e-headers-search  "mu4e-headers")
(declare-function mu4e-error   "mu4e-utils")

(require 'mu4e-view-common)
(require (if mu4e-view-use-old 'mu4e-view-old 'mu4e-view-gnus))

(defun mu4e-view (msg)
  "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that.

Depending on the value of `mu4e-view-use-old', either use mu4e's
internal display mode, or a (by default) display mode based on
Gnus' article-mode."

  ;; sanity checks.
  (if (and mu4e-view-use-old (featurep 'mu4e-view-gnus))
    (error "Cannot use old view when gnus-view is loaded; restart emacs")
    (if (and (not mu4e-view-use-old) (featurep 'mu4e-view-old))
        (error "Cannot use gnus-based view with old view loaded; restart emacs")))

  (mu4e~headers-update-handler msg nil nil);; update headers, if necessary.

  (if mu4e-view-use-old
      (mu4e~view-old msg)
    (mu4e~view-gnus msg)))

(defun mu4e-view-message-with-message-id (msgid)
  "View message with message-id MSGID. This (re)creates a
headers-buffer with a search for MSGID, then open a view for that
message."
  (mu4e-headers-search (concat "msgid:" msgid) nil nil t msgid t))

(provide 'mu4e-view)
;;; mu4e-view.el ends here
