;;; zweigtd-agenda-process-inbox.el --- WIP-*-lexical-binding:t-*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Author: Zweihänder
;; Keywords: outlines
;; Homepage: https://github.com/Zweihander-Main/zweigtd-agenda-process-inbox
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
(require 'bookmark)
(require 'org-agenda)
(require 'hl-line)
(require 'zweigtd-agenda-heading-functions)

(defgroup zweigtd-agenda-process-inbox nil
  "Customization for 'zweigtd-agenda-process-inbox' package."
  :group 'org
  :prefix "zweigtd-agenda-process-inbox-")

(defvar zweigtd-agenda-process-inbox-category ""
  "Category to filter inbox items by.
Leave as default empty string to use all items in the agenda buffer.")

(defvar zweigtd-agenda-process-inbox-next-file nil
  "Path to file to use for next filing.")

(defvar zweigtd-agenda-process-inbox-refile-target-info nil
  "Refile target for info items -- same as used for org-refile-targets.")

;;;###autoload
(defun zweigtd-agenda-process-inbox-single-item ()
  "Process a single item in the agenda."
  (interactive)
  (org-with-wide-buffer
   (let ((answer nil)
         (continue nil)
         (type "todo")
         (read-answer-short t))
     (while (not continue)
       (setq answer
             (read-answer (concat "Item options: "
                                  "[v]iew/"
                                  "[e]dit/"
                                  "[t]odo/"
                                  "[d]one/"
                                  "[a]note/"
                                  "[l]ink/"
                                  "[k]ill/"
                                  "[n]ext/"
                                  "[i]nfo/"
                                  "[r]ear/"
                                  "[RET]:Continue ")
                          '(("view" ?v "View in minibuffer")
                            ("edit" ?e "Edit the headline of the item")
                            ("todo" ?t "Change TODO state of item")
                            ("done" ?d "Mark done and archive")
                            ("note" ?a "Add a note to the item")
                            ("link" ?l "Open link and mark done")
                            ("kill" ?k "Kill current line")
                            ("next" ?n "Put in next file")
                            ("info" ?i "Convert to list item and refile under item")
                            ("rear" ?r "Move to end (rear) of list")
                            ("continue" ?\r "Continue processing"))))
       (cond ((string= answer "continue") (setq continue t))
             ((string= answer "view") (org-agenda-tree-to-indirect-buffer 1)  )
             ((string= answer "link")
              (let ((ret-msg ""))
                (setq ret-msg (org-agenda-open-link))
                (unless (and (stringp ret-msg )
                             (string= ret-msg "No link to open here"))
                  (setq type "link"
                        continue t))))
             ((string= answer "rear") (setq type "rear"
                                            continue t))
             ((string= answer "next") (setq type "next"
                                            continue t))
             ((string= answer "done") (setq type "done"
                                            continue t))
             ((string= answer "info") (setq type "info"
                                            continue t))
             ((string= answer "kill") (setq type "kill"
                                            continue t))
             ((string= answer "edit")
              (call-interactively
               #'zweigtd-agenda-heading-functions-edit-headline))
             ((string= answer "todo") (org-agenda-todo))
             ((string= answer "note") (call-interactively #'org-agenda-add-note))))
     (cond ((string= type "todo")
            (progn
              (org-agenda-set-tags)
              (org-agenda-priority)
              (call-interactively #'zweigtd-agenda-heading-functions-set-saved-effort)
              (org-agenda-refile nil nil t)))
           ((string= type "kill")
            (progn
              (org-agenda-todo "KILL")
              (org-agenda-archive)))
           ((string= type "done")
            (progn
              (org-agenda-todo "DONE")
              (org-agenda-archive)))
           ((string= type "rear")
            (org-agenda-drag-line-forward (- (length org-agenda-markers) 1)))
           ((string= type "next")
            (progn
              (org-agenda-todo "NEXT")
              (org-agenda-set-tags)
              (org-agenda-priority)
              (call-interactively #'zweigtd-agenda-heading-functions-set-saved-effort)
              (org-agenda-refile
               nil
               (list (concat
                      (car (last
                            (split-string zweigtd-agenda-process-inbox-next-file "/")))
                      "/") ;; should be "next.org/"
                     zweigtd-agenda-process-inbox-next-file nil nil) t)))
           ((string= type "link")
            (progn
              (org-agenda-todo "DONE")
              (org-agenda-archive)))
           ((string= type "info")
            (let ((org-refile-target-verify-function)
                  (org-refile-targets zweigtd-agenda-process-inbox-refile-target-info))
              ;; TODO: add in way to add to ideas, herf, english to add, roam, ect.
              ;; TODO: add in way to defer to bottom
              ;; TODO: Allow for schedule
              (org-agenda-refile nil nil t)
              (let* ((bookmark (plist-get org-bookmark-names-plist :last-refile))
                     (pos (bookmark-get-position bookmark))
                     (filename (bookmark-get-filename bookmark))
                     (buffer (get-file-buffer filename))
                     (inhibit-read-only t))
                (org-with-remote-undo buffer
                  (with-current-buffer buffer
                    (widen)
                    (goto-char pos)
                    (org-todo "")
                    (org-toggle-item t))))))))))

(defun zweigtd-agenda-process-inbox--bulk-process-entries ()
  "Bulk process entries in agenda."
  ;; Set temporary variable lookup -- set hl-line-face from hl-line to hl-line-active
  (when (not (null org-agenda-bulk-marked-entries))
    (let ((entries (reverse org-agenda-bulk-marked-entries))
          (processed 0)
          (skipped 0))
      (dolist (e entries)
        (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
          (if (not pos)
              (progn (message "Skipping removed entry at %s" e)
                     (cl-incf skipped))
            (goto-char pos)
            (hl-line-highlight)
            (highlight-lines-matching-regexp
             (string-trim (thing-at-point 'line t)) 'highlight)
            (let (org-loop-over-headlines-in-active-region)
              (funcall 'zweigtd-agenda-process-inbox-single-item))
            ;; `post-command-hook' is not run yet.  We make sure any
            ;; pending log note is processed.
            (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                      (memq 'org-add-log-note post-command-hook))
              (org-add-log-note))
            (cl-incf processed))))
      (org-agenda-redo t)
      (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
      (message "Acted on %d entries%s%s"
               processed
               (if (= skipped 0)
                   ""
                 (format ", skipped %d (disappeared before their turn)"
                         skipped))
               (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun zweigtd-agenda-process-inbox--bulk-mark-regexp-category (regexp)
  "Mark entries whose category match REGEXP for future agenda bulk action."
  (let ((entries-marked 0)
        category-at-point)
    (save-excursion
      (goto-char (point-min))
      (goto-char (next-single-property-change (point) 'org-hd-marker))
      (while (and (re-search-forward regexp nil t)
                  (setq category-at-point
                        (get-text-property (match-beginning 0) 'org-category)))
        (if (get-char-property (point) 'invisible)
            (beginning-of-line 2)
          (when (string-match-p regexp category-at-point)
            (setq entries-marked (1+ entries-marked))
            (call-interactively 'org-agenda-bulk-mark)))))
    (when (= entries-marked 0)
      (message "No entry matching this regexp."))))

;;;###autoload
(defun zweigtd-agenda-process-inbox-all-items ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (zweigtd-agenda-process-inbox--bulk-mark-regexp-category
   zweigtd-agenda-process-inbox-category)
  (zweigtd-agenda-process-inbox--bulk-process-entries))

;;;###autoload
(defun zweigtd-agenda-process-inbox-open-and-archive-all-links ()
  "Bulk mark links."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (goto-char (next-single-property-change (point) 'org-hd-marker))
    (let ((ret-msg "")
          (continue t))
      (while continue
        (setq ret-msg (org-agenda-open-link))
        (unless (and (stringp ret-msg )(string= ret-msg "No link to open here"))
          (progn
            (org-agenda-todo "DONE")
            (org-agenda-archive) ; TODO DRY with process-inbox-item
            (org-agenda-previous-line))) ;; Line was deleted and on next item
        (unless (org-agenda-next-line)
          (setq continue nil))))))

(provide 'zweigtd-agenda-process-inbox)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:

;;; zweigtd-agenda-process-inbox.el ends here
