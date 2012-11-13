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

(defun mf/mirror-region-in-multifile (beg end &optional multifile-buffer)
  (interactive "r")
  (deactivate-mark)
  (let ((buffer (current-buffer)))
    (switch-to-buffer-other-window (or "*multifile*" multifile-buffer))
    (mf--add-mirror buffer beg end)
    (switch-to-buffer-other-window buffer)))

(defun mf--add-mirror (buffer beg end)
  (let (contents original-overlay mirror-overlay)
    (with-current-buffer buffer
      (setq contents (buffer-substring beg end))
      (setq original-overlay (create-original-overlay beg end)))
    (mf--insert-contents)
    (setq mirror-overlay (create-mirror-overlay beg end))
    (overlay-put mirror-overlay 'twin original-overlay)
    (overlay-put original-overlay 'twin mirror-overlay)))

(defun mf--insert-contents ()
  (end-of-buffer)
  (newline)
  (setq beg (point))
  (insert contents)
  (setq end (point))
  (newline 2))

(defun create-original-overlay (beg end)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'type 'mf-original)
    (overlay-put o 'modification-hooks '(mf--on-modification))
    (overlay-put o 'insert-in-front-hooks '(mf--on-modification))
    (overlay-put o 'insert-behind-hooks '(mf--on-modification))
    o))

(defun create-mirror-overlay (beg end)
  (let ((o (make-overlay beg end nil nil t)))
    (overlay-put o 'type 'mf-mirror)
    (overlay-put o 'line-prefix mf--mirror-indicator)
    (overlay-put o 'modification-hooks '(mf--on-modification))
    (overlay-put o 'insert-in-front-hooks '(mf--on-modification))
    (overlay-put o 'insert-behind-hooks '(mf--on-modification))
    o))

(defvar mf--overlay-size nil)

(defun mf--on-modification (o after? beg end &optional delete-length)
  (when (not after?)
    (when (and (<= beg (overlay-start o))
               (>= end (overlay-end o)))
      (delete-overlay (overlay-get o 'twin))
      (delete-overlay o)))
  (when after?
    (mf--update-twin o)))

(defun mf--update-twin (o)
  (let* ((beg (overlay-start o))
         (end (overlay-end o))
         (contents (buffer-substring beg end))
         (twin (overlay-get o 'twin))
         (buffer (overlay-buffer twin))
         (beg (overlay-start twin))
         (end (overlay-end twin)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char beg)
        (insert contents)
        (delete-char (- end beg))
        ))))

(defvar mf--mirror-indicator "| ")
(add-text-properties
 0 1
 `(face (:foreground ,(format "#%02x%02x%02x" 128 128 128)
                     :background ,(format "#%02x%02x%02x" 128 128 128)))
 mf--mirror-indicator)

(provide 'multifiles)

;;; multifiles.el ends here
