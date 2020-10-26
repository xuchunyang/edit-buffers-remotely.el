;;; edit-buffers-remotely-tests.el --- Tests         -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for edit-buffers-remotely.el

;;; Code:

(require 'edit-buffers-remotely)
(require 'ert)

(ert-deftest edit-buffers-remotely-buffer-list ()
  (should (listp (edit-buffers-remotely-buffer-list))))

(ert-deftest edit-buffers-remotely-server-start ()
  (edit-buffers-remotely-server-start)
  (should edit-buffers-remotely-server)
  (edit-buffers-remotely-server-stop)
  (should-not edit-buffers-remotely-server))

(provide 'edit-buffers-remotely-tests)
;;; edit-buffers-remotely-tests.el ends here
