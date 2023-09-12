;;; culan-files.el --- -*- lexical-binding: t; -*-

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
;; Čulan public API for working with files.

;;; Code:



(require 'culan-api)



(defconst cfiles--file-directory "files")



(defun cfiles--generate-unique-object-id ()
  (let* ((current-time-list t))
    (format "file-%X-%X-%X-%X-(%X)"
            (random (expt 2 32))
            (random (expt 2 32))
            (random (expt 2 32))
            (random (expt 2 32))
            (nth 3 (current-time)))))



(defun cfiles-add (directory files)
  (setq directory (expand-file-name directory))
  (let* ((files-directory (file-name-concat directory cfiles--file-directory))
         (object-ids      nil))
    (unless (file-exists-p files-directory) (make-directory files-directory))
    (dolist (file files)
      (when-let* ((t* (not (file-directory-p file)))
                  (object-id (cfiles--generate-unique-object-id)))
        (copy-file file (file-name-concat files-directory object-id))
        (push object-id object-ids)))
    (apply #'capi-set directory
           (mapcar (lambda (object-id) (cons object-id "{}"))
                   object-ids))))

(defun cfiles-get (directory ids)
  (setq directory (expand-file-name directory))
  (mapcar (lambda (id) (cons id (file-name-concat directory id))) ids))

(defun cfiles-delete (directory ids)
  (setq directory (expand-file-name directory))
  (let* ((files-directory (file-name-concat directory cfiles--file-directory)))
    (mapc (lambda (id) (delete-file (file-name-concat files-directory id)))
          ids)
    (capi-delete directory ids)))



(provide 'culan-files)

;; Local Variables:
;; read-symbol-shorthands: (("cfiles-" . "culan-files-")
;;                          ("capi-"   . "culan-api-"))
;; End:

;;; culan-files.el ends here.
