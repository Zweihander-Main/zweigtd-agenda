;;; zweigtd-agenda-heading-functions-test.el --- Tests for zweigtd-agenda-heading-functions-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/zweigtd-agenda-heading-functions
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for zweigtd-agenda-heading-functions

;;; Code:

(require 'buttercup)
(require 'zweigtd-agenda-heading-functions)

(describe "zweigtd-agenda-heading-functions-set-saved-effort"
  (it "sets the saved effort variable to the provided value"
    (spy-on 'org-set-effort)
    (let ((marker (make-marker)))
      (set-marker marker (point) (current-buffer))
      (spy-on 'org-get-at-bol :and-return-value marker)
      (zweigtd-agenda-heading-functions-set-saved-effort "2:00")
      (expect 'org-set-effort :to-have-been-called)
      (expect zweigtd-agenda-heading-functions-saved-effort :to-equal "2:00"))))

(describe "zweigtd-agenda-heading-functions-edit-headline"
  (it "edits the headline of the current agenda item"
    (with-temp-buffer
      (org-mode)
      (org-agenda-list)
      (switch-to-buffer "*Org Agenda*")
      (spy-on 'org-edit-headline)
      (let ((marker (make-marker)))
        (set-marker marker (point) (current-buffer))
        (spy-on 'org-get-at-bol :and-return-value marker)
        (zweigtd-agenda-heading-functions-edit-headline)
        (expect 'org-edit-headline :to-have-been-called)
        (kill-buffer "*Org Agenda*")))))

(provide 'zweigtd-agenda-heading-functions-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; zweigtd-agenda-heading-functions-test.el ends here
