;;; zweigtd-agenda-statistics-cookies.el --- WIP-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/zweigtd-agenda-statistics-cookies
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
;;
;; WIP
;;
;;; Code:

(require 'org)
(require 'org-element)

(defgroup zweigtd-agenda-statistics-cookies nil
  "Customization for 'zweigtd-agenda-statistics-cookies' package."
  :group 'org
  :prefix "zweigtd-agenda-statistics-cookies-")

(defun zweigtd-agenda-statistics-cookies-insert-cookies (&optional type)
  "Insert statistics cookie of optional TYPE % (default) or /."
  (save-excursion
    (let ((cur-tags-string (org-make-tag-string (org-get-tags))))
      (if (not (eq cur-tags-string ""))
          (when (org-back-to-heading t)
            (re-search-forward org-tag-line-re)
            (goto-char (- (match-beginning 1) 1)))
        (end-of-line))
      (insert (concat " " (if (eq type '/) "[/]" "[%]")))
      (org-update-statistics-cookies nil))))

(defun zweigtd-agenda-statistics-cookies-find-cookies ()
  "Find statistics cookies on line and return as plist."
  (save-excursion
    (beginning-of-line)
    (let ((end-point (save-excursion (end-of-line) (point)))
          (search-point (point))
          (cookie nil))
      (while (and (not cookie) search-point)
        (setq search-point (re-search-forward "\\[" end-point t))
        (when search-point
          (forward-char -1)
          (setq cookie (cadr (org-element-statistics-cookie-parser)))
          (forward-char 1)))
      (if cookie
          (plist-put cookie :type (if (eq (string-match-p "%" (plist-get cookie :value)) nil) '/ '%))
        cookie))))

(defun zweigtd-agenda-statistics-cookies-delete-cookies ()
  "Delete statistics cookies on line."
  (let ((cookie (zweigtd-agenda-statistics-cookies-find-cookies)))
    (when cookie
      (delete-region (plist-get cookie :begin) (plist-get cookie :end))
      (save-excursion
        (end-of-line)
        (when (eq (char-before) ? )
          (delete-char -1))))))

;;;###autoload
(defun zweigtd-agenda-statistics-cookies-toggle-cookies ()
  "Toggle between [/] and [%] type statistics cookies on line."
  (interactive)
  (let ((type (plist-get (zweigtd-agenda-statistics-cookies-find-cookies) :type)))
    (zweigtd-agenda-statistics-cookies-delete-cookies)
    (cond ((eq type '%) (zweigtd-agenda-statistics-cookies-insert-cookies '/))
          ((eq type '/) (zweigtd-agenda-statistics-cookies-insert-cookies '%)))))

(provide 'zweigtd-agenda-statistics-cookies)

;; Local Variables:
;; coding: utf-8
;; End:

;;; zweigtd-agenda-statistics-cookies.el ends here
