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

;; Bind a key to `mf/mirror-region-in-multifile`, let's say `C-!`. Now
;; mark a part of the buffer and press it. A new \*multifile\* buffer pops
;; up. Mark some other part of another file, and press `C-!` again. This
;; is added to the \*multifile\*.

;; You can now edit the \*multifile\* buffer, and watch the original files change.
;; Or you can edit the original files and watch the \*multifile\* buffer change.

;; **Warning** This API and functionality is highly volatile.

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
    (mf---insert-contents)
    (setq mirror-overlay (create-mirror-overlay beg end))
    (overlay-put mirror-overlay 'twin original-overlay)
    (overlay-put original-overlay 'twin mirror-overlay)))

(defun mf---insert-contents ()
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
    (when (mf---removed-entire-overlay)
      (mf--remove-mirror o)))

  (when (and after? (not (null (overlay-start o))))
    (mf--update-twin o)))

(defun mf---removed-entire-overlay ()
  (and (<= beg (overlay-start o))
       (>= end (overlay-end o))))

(defun mf--remove-mirror (o)
  (let* ((twin (overlay-get o 'twin))
         (original (if (mf--is-original o) o twin))
         (mirror (if (mf--is-original o) twin o))
         (mirror-beg (overlay-start mirror))
         (mirror-end (overlay-end mirror)))
    (with-current-buffer (overlay-buffer mirror)
      (save-excursion
        (delete-overlay mirror)
        (delete-region mirror-beg mirror-end)
        (goto-char mirror-beg)
        (delete-blank-lines)))
    (delete-overlay original)))

(defun mf--is-original (o)
  (equal 'mf-original (overlay-get o 'type)))

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
