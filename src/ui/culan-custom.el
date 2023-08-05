;;; culan-custom.el --- Čulan custom variables. -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;; Copyright (C) 2023  Lämppi Lütti <lamppilutti@gmail.com>
;;
;; This file is part or Čulan.
;;
;; Čulan is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Čulan is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details
;;
;; You should have received a copy of the GNU General Public License
;; along with Čulan.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:



(require 'custom)
(require 'xdg)



(defgroup culan nil
  "Abstract object management system."
  :prefix "culan-"
  :group 'applications)

(defcustom culan-default-storage-directory
  (expand-file-name "emacs/culan/" (xdg-data-home))
  "Default directory where čulan store data."
  :type  '(directory)
  :group 'culan)



(provide 'culan-custom)

;;; culan-custom.el ends here
