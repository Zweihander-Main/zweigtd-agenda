;;; zweigtd-agenda-heading-functions.el --- WIP-*-lexical-binding:t-*-

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
;;
;; WIP
;;
;;; Code:

(require 'org)
(require 'org-agenda)
(require 'zweigtd-agenda-statistics-cookies)

(defgroup zweigtd-agenda-heading-functions nil
  "Customization for 'zweigtd-agenda-heading-functions' package."
  :group 'org
  :prefix "zweigtd-agenda-heading-functions-")

(defvar zweigtd-agenda-heading-functions-saved-effort "1:00"
  "Current saved effort for agenda items.")

(defun zweigtd-agenda-heading-functions--redo-all-agenda-buffers ()
  "Refresh/redo all `org-agenda' buffers."
  (interactive)
  (let ((visible-buffers
         (if (fboundp 'doom-visible-buffers)
             (doom-visible-buffers) ; Doom vers if available
           (delete-dups (mapcar #'window-buffer (window-list))))))
    (dolist (buffer visible-buffers)
      (with-current-buffer buffer
        (when (derived-mode-p 'org-agenda-mode)
          (org-agenda-redo))))))

;;;###autoload
(defun zweigtd-agenda-heading-functions-set-saved-effort (effort)
  "Set the EFFORT property for the current headline."
  (interactive
   (list (read-string
          (format "Effort [%s]: " zweigtd-agenda-heading-functions-saved-effort)
          nil
          nil
          zweigtd-agenda-heading-functions-saved-effort)))
  (setq zweigtd-agenda-heading-functions-saved-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively
         'org-set-effort
         nil
         zweigtd-agenda-heading-functions-saved-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

;;;###autoload
(defun zweigtd-agenda-heading-functions-edit-headline ()
  "Perform org-edit-headline on current agenda item."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (call-interactively #'org-edit-headline)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker)
      (beginning-of-line 1))))

;;;###autoload
(defun zweigtd-agenda-heading-functions-break-into-child (child)
  "Create CHILD heading under current heading with same props and effort."
  (interactive
   (list (read-string "Child task: " nil nil nil)))
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         cur-tags cur-line cur-priority cur-stats-cookies)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (setq cur-line (thing-at-point 'line t))
        (if (string-match org-priority-regexp cur-line)
            (setq cur-priority (match-string 2 cur-line)))
        (setq cur-tags (org-make-tag-string (org-get-tags)))
        (setq cur-stats-cookies (zweigtd-agenda-statistics-cookies-find-cookies))
        (if (eq cur-stats-cookies 'nil)
            (zweigtd-agenda-statistics-cookies-insert-cookies))
        (if (fboundp '+org/insert-item-below)
            (call-interactively #'+org/insert-item-below) ; Doom ver if available
          (call-interactively #'org-insert-item)) ; Non-Doom if not
        (call-interactively #'org-demote-subtree)
        (funcall-interactively 'org-edit-headline child)
        (funcall-interactively 'org-set-tags-to cur-tags)
        (if cur-priority
            (funcall-interactively 'org-priority (string-to-char cur-priority)))
        (org-update-parent-todo-statistics)
        (end-of-line 1))
      (beginning-of-line 1)))
  (zweigtd-agenda-heading-functions--redo-all-agenda-buffers)
  (let (txt-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (and (search-forward child nil t)
           (setq txt-at-point
                 (get-text-property (match-beginning 0) 'txt)))
      (if (get-char-property (point) 'invisible)
          (beginning-of-line 2)
        (when (string-match-p child txt-at-point)
          (call-interactively 'zweigtd-agenda-heading-functions-set-saved-effort))))))

(provide 'zweigtd-agenda-heading-functions)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; zweigtd-agenda-heading-functions.el ends here
