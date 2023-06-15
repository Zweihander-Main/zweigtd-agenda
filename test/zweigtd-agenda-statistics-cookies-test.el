;;; zweigtd-agenda-statistics-cookies-test.el --- Tests for zweigtd-agenda-statistics-cookies-*-lexical-binding:t-*-

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

;; Tests for zweigtd-agenda-statistics-cookies

;;; Code:

(require 'buttercup)
(require 'zweigtd-agenda-statistics-cookies)

(defvar zweigtd-agenda-statistics-cookies-test-temp-buffer nil)

(describe "zweigtd-agenda-statistics-cookies"
  (before-each
    (setq zweigtd-agenda-statistics-cookies-test-temp-buffer
          (generate-new-buffer "temp-buffer"))
    (with-current-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer
      (org-mode)
      (insert "* Heading 1")
      (goto-char (point-min))
      (zweigtd-agenda-statistics-cookies-insert-cookies)))

  (after-each
    (kill-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer))

  (it "should insert [%] cookie at the end of the line"
    (with-current-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [100%]")))

  (it "should find the statistics cookie on the line and return it as a plist"
    (with-current-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer
      (let ((cookie (zweigtd-agenda-statistics-cookies-find-cookies)))
        (expect cookie :to-equal
                '(:begin 13 :end 19 :value "[100%]" :post-blank 0 :type %)))))

  (it "should toggle between [/] and [%] type statistics cookies on the line"
    (with-current-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer
      (zweigtd-agenda-statistics-cookies-toggle-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [0/0]")
      (zweigtd-agenda-statistics-cookies-toggle-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1 [100%]")))

  (it "should delete the statistics cookie on the line"
    (with-current-buffer zweigtd-agenda-statistics-cookies-test-temp-buffer
      (zweigtd-agenda-statistics-cookies-delete-cookies)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-equal "* Heading 1"))))


(provide 'zweigtd-agenda-statistics-cookies-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; zweigtd-agenda-statistics-cookies-test.el ends here
