;;; culan-ui.el --- -*- lexical-binding: t; -*-

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
;; User interface for iteracting with Čulan.

;;; Code:



(eval-when-compile
  (require 'keymap)
  (require 'subr-x))

(require 'uniquify)

(require 'culan-api)
(require 'culan-custom)



(defvar-local cui--directory nil)



(defun cui--directory-initialization-dialog (directory)
  (unless (capi-initialized-directory-p directory)
    (named-let dialog
        ((initializep
          (yes-or-no-p
           (format "%s is not initialized.  Initialize it?" directory)))
         (forcep (capi-acceptable-directory-p directory))
         (abort   nil))
      (cond
       ((and initializep forcep) t)
       ((and initializep (not forcep) (not abort))
        (dialog initializep
                (yes-or-no-p
                 (format "Initializing %s directory can violate directory\
integrity.  Continue?" directory))
                t))
       (t (user-error "Abort"))))))

(defun cui--find-buffer (directory)
  (let* ((buffers (buffer-list))
         (result  nil))
    (while-let ((buffer   (pop buffers))
                (continue t))
      (when (and (equal 'culan-mode (buffer-local-value 'major-mode buffer))
                 (equal directory (buffer-local-value 'cui--directory buffer)))
        (setq result   buffer
              continue nil)))
    result))

(defun cui--init-buffer (directory)
  (let* ((buffer (generate-new-buffer "Čulan")))
    (with-current-buffer buffer
      (rename-buffer
       (format "*%s*" (or (uniquify-rationalize-file-buffer-names
                           "Čulan" directory buffer)
                          "Čulan")))
      (culan-mode)
      (setq cui--directory directory))
    buffer))

(defun cui--insert-header ()
  (insert (format "Current directory: %s \n" cui--directory)))



(defvar-keymap culan-mode-map
  :doc "Keymap for `culan-mode'."
  :suppress 'nodigits
  "l" #'culan-ui-list-objects)

(define-derived-mode culan-mode special-mode "Čulan"
  :docstring "Major mode for object list manipulation."
  :interactive  nil
  :syntax-table nil
  :abbrev-table nil
  :group 'culan
  (buffer-disable-undo))



(defun culan-ui ()
  (declare (interactive-only t))
  (interactive)
  (let* ((directory (if current-prefix-arg
                        (read-directory-name "Select Čulan directory: ")
                      culan-default-storage-directory)))
    (when (cui--directory-initialization-dialog directory)
      (capi-initialize-directory directory))
    (pop-to-buffer
     (or (cui--find-buffer directory)
         (cui--init-buffer directory)))))

(defun culan-ui-list-objects ()
  (declare (interactive-only t)
           (modes culan-mode))
  (interactive)
  (let* ((inhibit-read-only t)
         (entities (capi-get-all (capi-get-db cui--directory))))
    (erase-buffer)
    (cui--insert-header)
    (mapc (lambda (object)
            (insert (car object) "\n"))
          entities)))



(provide 'culan-ui)

;; Local Variables:
;; read-symbol-shorthands: (("cui-"  . "culan-ui-")
;;                          ("capi-" . "culan-api-"))
;; End:

;;; culan-ui.el ends here.
