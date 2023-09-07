;;; culan-editor.el --- -*- lexical-binding: t; -*-

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
;; Major mode for editing object's metadata.

;;; Code:



(require 'derived)
(require 'json)
(require 'pp)



(defvar-local ce--directory nil)

(defvar-local ce--object-id nil)



(defun ce--json->plist (json)
  (let* ((json-object-type 'plist)
         (json-key-type    'keyword)
         (json-false       :false))
    (json-read-from-string json)))

(defun ce--object->json (object)
  (let* ((json-object-type 'plist)
         (json-key-type    'keyword)
         (json-false       :false))
    (json-encode object)))

(defun ce--save-object ()
  (let* ((raw-object (buffer-substring-no-properties (point-min) (point-max)))
         (object     (ce--object->json (car (read-from-string raw-object)))))
    (capi-set ce--directory (list (cons ce--object-id object)))
    (set-buffer-modified-p nil)
    t))

(define-derived-mode culan-editor-mode lisp-data-mode "Čulan[Editor]"
  :docstring "Major mode for editing object metadata."
  :interactive  nil
  :syntax-table nil
  :abbrev-table nil
  :group 'culan
  (add-hook 'write-contents-functions #'ce--save-object))



(defun ce-open-in-editor (directory object-id)
  (let* ((object (car (capi-get directory (list object-id))))
         (buffer (get-buffer object-id)))
    (when (null buffer)
      (with-current-buffer (generate-new-buffer object-id)
        (culan-editor-mode)
        (setq-local ce--directory directory
                    ce--object-id object-id)
        (if-let* ((plist (ce--json->plist (cdr object))))
            (pp-emacs-lisp-code plist)
          (insert "()"))
        (set-buffer-modified-p nil)
        (setq buffer (current-buffer))))
    (pop-to-buffer buffer)))



(provide 'culan-editor)

;; Local Variables:
;; read-symbol-shorthands: (("ce-"   . "culan-editor-")
;;                          ("capi-" . "culan-api-"))
;; End:

;;; culan-editor.el ends here.
