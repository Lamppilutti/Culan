;;; culan-db.el --- Čulan database operations. -*- lexical-binding: t; -*-

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
;; This file is private API.

;;; Code:



(require 'sqlite)



(defconst cdb--db-file-name "db.sqlite3")



(defun cdb--read-objects (stmt)
  (let* ((result nil))
    (while (sqlite-more-p stmt)
      (let* ((row  (sqlite-next stmt))
             (id   (nth 0 row))
             (data (nth 1 row)))
        (push (cons id data) result)))
    (sqlite-finalize stmt)
    result))

(defun cdb--repeat-concat (pattern times separator)
  (let* ((result pattern))
    (dotimes (_ (1- times))
      (setq result (concat result separator pattern)))
    result))



(defun cdb-get-db (directory)
  (let* ((query      "CREATE TABLE IF NOT EXISTS objects
                      (id TEXT UNIQUE NOT NULL, data TEXT NOT NULL);")
         (db-file    (expand-file-name cdb--db-file-name directory))
         (connection (sqlite-open db-file)))
    (sqlite-execute connection query)
    connection))

(defun cdb-set (db objects)
  (let* ((query (format "INSERT OR REPLACE INTO objects(id, data) VALUES %s;"
                        (cdb--repeat-concat "(?, ?)" (length objects) ","))))
    (sqlite-execute db query (flatten-list objects))
    objects))

(defun cdb-get (db ids)
  (let* ((query (format "SELECT id, data FROM objects WHERE id IN (%s);"
                        (cdb--repeat-concat "?" (length ids) ","))))
    (cdb--read-objects (sqlite-select db query ids 'set))))

(defun cdb-get-all (db ids)
  (let* ((query "SELECT id, data FROM objects;"))
    (cdb--read-objects (sqlite-select db query ids 'set))))

(defun cdb-delete (db ids)
  (let* ((query (format "DELETE FROM objects WHERE id IN (%s);"
                        (cdb--repeat-concat "?" (seq-length ids) ","))))
    (sqlite-execute db query ids)))



(provide 'culan-db)

;; Local Variables:
;; read-symbol-shorthands: (("cdb-" . "culan-db-"))
;; End:

;;; culan-db.el ends here.
