;;; culan-api.el --- Čulan public operations over objects. -*- lexical-binding: t; -*-

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



(require 'sqlite)



(defconst capi--flag-file-name ".culan")

(defconst capi--db-directory "database")

(defconst capi--db-file-name "db.sqlite3")



(defun capi--read-objects (stmt)
  (let* ((result nil))
    (while (sqlite-more-p stmt)
      (let* ((row  (sqlite-next stmt))
             (id   (nth 0 row))
             (data (nth 1 row)))
        (push (cons id data) result)))
    (sqlite-finalize stmt)
    result))

(defun capi--repeat-concat (pattern times separator)
  (let* ((result pattern))
    (dotimes (_ (1- times))
      (setq result (concat result separator pattern)))
    result))



(defun capi-init-directory (directory &optional force)
  (when (or force
            (not (file-exists-p directory))
            (and (file-directory-p directory)
                 (directory-empty-p directory)))
    (make-directory directory t)
    (make-empty-file capi--flag-file-name)))

(defun capi-get-db (directory)
  (let* ((query      "CREATE TABLE IF NOT EXISTS objects
                      (id TEXT UNIQUE NOT NULL, data TEXT NOT NULL);")
         (db-file    (expand-file-name capi--db-file-name directory))
         (connection (sqlite-open db-file)))
    (sqlite-execute connection query)
    connection))

(defun capi-set (db objects)
  (let* ((query (format "INSERT OR REPLACE INTO objects(id, data) VALUES %s;"
                        (capi--repeat-concat "(?, ?)" (length objects) ","))))
    (sqlite-execute db query (flatten-list objects))
    objects))

(defun capi-get (db ids)
  (let* ((query (format "SELECT id, data FROM objects WHERE id IN (%s);"
                        (capi--repeat-concat "?" (length ids) ","))))
    (capi--read-objects (sqlite-select db query ids 'set))))

(defun capi-get-all (db)
  (let* ((query "SELECT id, data FROM objects;"))
    (capi--read-objects (sqlite-select db query nil 'set))))

(defun capi-delete (db ids)
  (let* ((query (format "DELETE FROM objects WHERE id IN (%s);"
                        (capi--repeat-concat "?" (seq-length ids) ","))))
    (sqlite-execute db query ids)))



(provide 'culan-api)

;; Local Variables:
;; read-symbol-shorthands: (("capi-" . "culan-api-"))
;; End:

;;; culan-api.el ends here.
