;;; multifiles.el --- View and edit parts of multiple files in one buffer

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: multiple files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Open a new buffer, insert the contents of another file like this:
;;
;;     M-: (mf/insert-mirror "~/.emacs.d/init.el" 30 39)
;;
;; which would insert the lines 30-39 in the file `~/.emacs.d/init.el`.
;;
;; You can then repeat that to get multiple files in the buffer. You can also
;; edit the block, and the changes will be mirrored instantly in the other file.
;;
;; *Be careful!* Right now the mirror only works one way. Changes in the original files
;; are not currently mirrored into the multi-file. So any changes might be lost if you're
;; switching back and forth.
;;
;; This should be fixable, but this is all I have time for tonight.

;;; Code:

(defun mf/add-region-to-multifile (beg end)
  (interactive "r")
  (let ((file (buffer-file-name))
        (first-line (line-number-at-pos beg))
        (last-line (1+ (line-number-at-pos end))))
    (switch-to-buffer "*multifile*")
    (emacs-lisp-mode)
    (mf/insert-mirror file first-line last-line)))

(defun mf/insert-mirror (file first-line last-line)
  (let ((contents (mf--file-contents-between-lines)))
    (mf--insert-header)
    (let ((beg (point)))
      (insert contents)
      (mf--add-mirror-overlay beg (point)))))

(defun mf--file-contents-between-lines ()
  (save-window-excursion
    (let (beg end)
      (find-file file)
      (save-excursion
        (goto-line first-line)
        (setq beg (point))
        (goto-line last-line)
        (setq end (point))
        (buffer-substring beg end)))))

(defun mf--insert-header ()
  (deactivate-mark)
  (if (search-forward "--+[ " nil t)
      (forward-line -1)
    (end-of-buffer))
  (newline)
  (comment-dwim nil)
  (insert (format "--+[ %s --- lines %d-%d:" file first-line last-line))
  (newline 2))

(defvar mf--mirror-indicator "| ")
(add-text-properties
 0 1
 `(face (:foreground ,(format "#%02x%02x%02x" 128 128 128)
                     :background ,(format "#%02x%02x%02x" 128 128 128)))
 mf--mirror-indicator)

(defun mf--add-mirror-overlay (beg end)
  (let ((o (make-overlay beg end nil nil nil)))
    (overlay-put o 'type 'mf-mirror)
    (overlay-put o 'line-prefix mf--mirror-indicator)
    (overlay-put o 'file file)
    (overlay-put o 'first-line first-line)
    (overlay-put o 'last-line last-line)
    (overlay-put o 'modification-hooks '(on-mirror-modification))
    (overlay-put o 'insert-in-front-hooks '(on-mirror-modification))
    (overlay-put o 'insert-behind-hooks '(on-mirror-modification))))

(defun on-mirror-modification (o after? beg end &optional length)
  (when after?
    (let ((beg (overlay-start o))
          (end (overlay-end o)))
      (when (not (null beg))
        (unless (memq this-command '(undo undo-tree))
          (mf--update-header o))
        (mf--update-original-file o)
        (mf--update-mirror-overlay o)
        ))))

(defmacro comment (&rest ignore))

(defun mf--update-original-file (o)
  (let ((file (overlay-get o 'file))
        (first-line (overlay-get o 'first-line))
        (last-line (overlay-get o 'last-line))
        (contents (buffer-substring (overlay-start o) (overlay-end o))))
    (save-window-excursion
      (find-file file)
      (save-excursion
        (goto-line first-line)
        (setq beg (point))
        (goto-line last-line)
        (setq end (point))
        (delete-region beg end)
        (insert contents)))))

(defun mf--find-last-line (o)
  (let ((first-line (line-number-at-pos (overlay-start o)))
        (last-line (line-number-at-pos (overlay-end o))))
    (+ (overlay-get o 'first-line)
       (- last-line first-line))))

(defun mf--update-mirror-overlay (o)
  (overlay-put o 'last-line (mf--find-last-line o)))

(defun mf--update-header (o)
  (save-excursion
    (goto-char (overlay-start o))
    (forward-line -2)
    (end-of-line)
    (forward-char -1)
    (while (looking-back "[0-9]")
      (delete-char -1))
    (insert (format "%d" (mf--find-last-line o)))))

(provide 'multifiles)
;;; multifiles.el ends here
