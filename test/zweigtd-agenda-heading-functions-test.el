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

(describe "zweigtd-agenda-heading-functions"
  (it "isn't erroring out"
    (expect t :to-be t)))

(provide 'zweigtd-agenda-heading-functions-test)

;; Local Variables:
;; coding: utf-8
;; End:

;;; zweigtd-agenda-heading-functions-test.el ends here
