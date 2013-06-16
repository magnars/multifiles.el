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

(require 'dash)

(defun mf/mirror-region-in-multifile (beg end &optional multifile-buffer)
  "Mirror the region between BEG and END into *multifile* buffer.

If called with \\[universal-argument], prompt the user for the buffer where the
region is added.

In both cases, the region is added to the end of the buffer.
Editing this region will automatically update the original buffer
as well.  Editing the region in the original file will also
update the mirror buffers."
  (interactive (list (region-beginning) (region-end)
                     (when current-prefix-arg
                       (read-buffer "Mirror into buffer: " "*multifile*"))))
  (deactivate-mark)
  (let ((buffer (current-buffer))
        (mode major-mode))
    (switch-to-buffer-other-window (or multifile-buffer "*multifile*"))
    (funcall mode)
    (multifiles-minor-mode 1)
    (mf--add-mirror buffer beg end)
    (switch-to-buffer-other-window buffer)))

(defun mf/remove-mirror-region-in-multifile (&optional pos)
  "Remove the twin overlays under the cursor.  Can be called
either in original or mirror buffer.

If optional argument POS is provided, remove the twin overlays
found at that position."
  (interactive)
  (setq pos (or pos (point)))
  (let ((ovs (--filter
              (memq (overlay-get it 'type) '(mf-original mf-mirror))
              (overlays-at pos))))
    (-each ovs 'mf--remove-mirror)))

(defun mf/transpose-mirror-regions (&optional arg)
  "Exchange the mirror region under point with the one following it.

If ARG is provided, transpose ARG following regions, or preceding
if negative."
  (interactive "p")
  (let* ((n (abs arg))
         (dir (/ arg n))
         (cur (mf--current-mirror-overlay))
         (offset (and cur (- (point) (overlay-start cur)))))
    (when cur
      (while (< 0 n)
        (let* ((next (mf--next-overlay dir))
               (cs (overlay-start cur))
               (ns (and next (overlay-start next))))
          (if (not next)
              (setq n -1)
            (if (> dir 0)
                (progn
                  (save-excursion (goto-char (1- ns)) (insert "\n"))
                  (mf--move-overlay cur ns)
                  (mf--move-overlay next cs)
                  (save-excursion (goto-char (overlay-end cur)) (delete-char 1)))
              (save-excursion (goto-char (1- cs)) (insert "\n"))
              (mf--move-overlay next cs)
              (mf--move-overlay cur ns)
              (save-excursion (goto-char (overlay-end next)) (delete-char 1)))
            (goto-char (+ (overlay-start cur) offset))
            (setq n (1- n))))))))

(defun mf/jump-to-twin (o)
  "Jump to the twin buffer."
  (interactive (list
                (cond
                 ((mf--current-mirror-overlay))
                 ((let ((orig (mf--current-original-overlay)))
                    (when orig
                      (if (> (length orig) 1)
                          (let ((buf (completing-read
                                      "Jump to mirror in buffer: "
                                      (--map (buffer-name (overlay-buffer (overlay-get it 'twin))) orig)
                                      nil t)))
                            (--first (equal (buffer-name (overlay-buffer (overlay-get it 'twin))) buf) orig))
                        (car orig))))))))
  (when o
    (let ((twin (overlay-get o 'twin))
          (offset (- (point) (overlay-start o))))
      (switch-to-buffer-other-window (overlay-buffer twin))
      (goto-char (+ (overlay-start twin) offset)))))

(defvar multifiles-minor-mode-map (make-sparse-keymap)
  "Keymap for multifiles minor mode.")

(define-key multifiles-minor-mode-map [remap save-buffer] 'mf/save-original-buffers)

(defun mf/save-original-buffer ()
  "Save the original buffer of the mirror region under point."
  (interactive)
  (let ((cur (mf--current-mirror-overlay)))
    (when cur
      (with-current-buffer (overlay-buffer (overlay-get cur 'twin))
        (when buffer-file-name
          (save-buffer))))))

(defun mf/save-original-buffers ()
  "Save the original buffers of all mirror regions in current
buffer and also current buffer, if it has some associated file.

The mirror overlays are temporarily removed before saving the
current buffer."
  (interactive)
  (when (yes-or-no-p "Are you sure you want to save all original files?")
    (--each (mf--original-buffers)
      (with-current-buffer it
        (when buffer-file-name
          (save-buffer)))))
  (when buffer-file-name
    (let ((mirrors (->> (overlays-in (point-min) (point-max))
                     (--filter (eq 'mf-mirror (overlay-get it 'type)))
                     (mf--sort-overlays)
                     (--map (list
                             it
                             (overlay-start it)
                             (overlay-end it)
                             (buffer-substring (overlay-start it) (1+ (overlay-end it)))))))
          (p (point)))
      (--each (reverse mirrors)
        (move-overlay (nth 0 it) 1 2)
        (delete-region (nth 1 it) (1+ (nth 2 it))))
      (save-buffer)
      (--each mirrors
        (goto-char (nth 1 it))
        (insert (nth 3 it))
        (move-overlay (nth 0 it) (nth 1 it) (nth 2 it)))
      (goto-char p))))

(defun mf--original-buffers ()
  (->> (overlays-in (point-min) (point-max))
    (--filter (equal 'mf-mirror (overlay-get it 'type)))
    (--map (overlay-buffer (overlay-get it 'twin)))
    (-distinct)))

(define-minor-mode multifiles-minor-mode
  "A minor mode for the *multifile* buffer."
  nil "" multifiles-minor-mode-map)

(defun mf--add-mirror (buffer beg end)
  (let (contents original-overlay mirror-overlay)
    (mf--add-hook-if-necessary)
    (with-current-buffer buffer
      (mf--add-hook-if-necessary)
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

(defun mf--any-overlays-in-buffer ()
  (--any? (memq (overlay-get it 'type) '(mf-original mf-mirror))
          (overlays-in (point-min) (point-max))))

(defun mf--add-hook-if-necessary ()
  (unless (mf--any-overlays-in-buffer)
    (add-hook 'post-command-hook 'mf--update-twins nil t)))

(defun mf--remove-hook-if-necessary ()
  (unless (mf--any-overlays-in-buffer)
    (remove-hook 'post-command-hook 'mf--update-twins t)))

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

(defun mf--current-mirror-overlay (&optional pos)
  "Return the mirror overlay at POS.  Defaults to (point)."
  (setq pos (or pos (point)))
  (car (--filter (eq (overlay-get it 'type) 'mf-mirror) (overlays-at pos))))

(defun mf--current-original-overlay (&optional pos)
  "Return list of original overlays at POS.  Defaults to (point)."
  (setq pos (or pos (point)))
  (--filter (eq (overlay-get it 'type) 'mf-original) (overlays-at pos)))

(defvar mf--changed-overlays nil)
(make-variable-buffer-local 'mf--changed-overlays)

(defun mf--on-modification (o after? beg end &optional delete-length)
  (when (not after?)
    (when (mf---removed-entire-overlay)
      (mf--remove-mirror o)))

  (when (and after? (not (null (overlay-start o))))
    (add-to-list 'mf--changed-overlays o)))

(defun mf---removed-entire-overlay ()
  (and (<= beg (overlay-start o))
       (>= end (overlay-end o))))

(defun mf--update-twins ()
  (when mf--changed-overlays
    (-each mf--changed-overlays 'mf--update-twin)
    (setq mf--changed-overlays nil)))

(defun mf--remove-mirror (o)
  (let* ((twin (overlay-get o 'twin))
         (original (if (mf--is-original o) o twin))
         (mirror (if (mf--is-original o) twin o))
         (mirror-beg (overlay-start mirror))
         (mirror-end (overlay-end mirror))
         (cb (current-buffer)))
    (with-current-buffer (overlay-buffer mirror)
      (save-excursion
        (delete-overlay mirror)
        (unless (equal cb (current-buffer)) (delete-region mirror-beg mirror-end))
        (goto-char mirror-beg)
        (delete-blank-lines)
        (mf--remove-hook-if-necessary)))
    (delete-overlay original)
    (mf--remove-hook-if-necessary)))

(defun mf--is-original (o)
  (equal 'mf-original (overlay-get o 'type)))

(defun mf--update-twin (o)
  (when (overlay-start o)
    (let* ((beg (overlay-start o))
           (end (overlay-end o)))
      (when (and (<= beg (point)) (<= (point) end))
        ;; this is an unfortunate hack to preserve the
        ;; point. `save-excursion' doesn't work if the point is in an
        ;; area that is removed during the editing. Therefore, we
        ;; first insert a part before the point, then remove the
        ;; original, then insert the rest. Feels like an emacs bug...
        (let ((contents-b (buffer-substring beg (point)))
              (contents-e (buffer-substring (point) end))
              (twin (overlay-get o 'twin))
              (cb (current-buffer)))
          (mf--insert-text-into-twin o contents-b contents-e)
          ;; if this buffer is a mirror buffer, get all the original
          ;; overlays in the original buffer and mirror their twins as
          ;; well. This is sort of "3-way merge" mirror -> orig -> all
          ;; other mirrors
          (when (eq (overlay-get o 'type) 'mf-mirror)
            (let ((mirrors (with-current-buffer (overlay-buffer twin)
                             (mf--current-original-overlay (1+ (overlay-start twin))))))
              (--each mirrors (mf--insert-text-into-twin it contents-b contents-e cb)))))))))

(defun mf--insert-text-into-twin (o contents-b contents-e &optional cb)
  (let* ((twin (overlay-get o 'twin))
         (buffer (overlay-buffer twin))
         (beg (overlay-start twin))
         (end (overlay-end twin)))
    (with-current-buffer buffer
      (when (not (equal cb (current-buffer)))
        (if (and (<= beg (point)) (<= (point) end))
            (progn
              (goto-char beg)
              (insert contents-b)
              (delete-char (- end beg))
              (save-excursion (insert contents-e)))
          (save-excursion
            (goto-char beg)
            (insert contents-b contents-e)
            (delete-char (- end beg))))))))

(defvar mf--mirror-indicator "| ")
(add-text-properties
 0 1
 `(face (:foreground ,(format "#%02x%02x%02x" 128 128 128)
                     :background ,(format "#%02x%02x%02x" 128 128 128)))
 mf--mirror-indicator)


;;; navigation
(defun mf--sort-overlays (list)
  "Sort LIST of overlays by `overlay-start'."
  (sort list (lambda (x y) (< (overlay-start x) (overlay-start y)))))

(defun mf--move-overlay (overlay pos)
  "Move the OVERLAY and the content of the region inside it to POS."
  (let* ((s (overlay-start overlay))
         (e (overlay-end overlay))
         (len (- e s))
         (content (buffer-substring s e)))
    (save-excursion
      (goto-char pos)
      (insert content)
      (move-overlay overlay pos (+ pos len)))
    (if (< pos s)
        (delete-region (+ s len) (+ e len))
      (delete-region s e))))

(defun mf--next-overlay (&optional arg)
  "Find the next mirror overlay after point."
  (setq arg (or arg 1))
  (if (< 0 arg)
      (->> (overlays-in (point-min) (point-max))
        (--filter (eq 'mf-mirror (overlay-get it 'type)))
        (mf--sort-overlays)
        (--drop-while (<= (overlay-start it) (point)))
        (-drop (1- arg))
        (car))
    (mf--previous-overlay (- arg))))

(defun mf--previous-overlay (&optional arg)
  "Find the previous mirror overlay before point."
  (setq arg (or arg 1))
  (if (< 0 arg)
      (->> (overlays-in (point-min) (point-max))
        (--filter (eq 'mf-mirror (overlay-get it 'type)))
        (mf--sort-overlays)
        (nreverse)
        (--drop-while (>= (overlay-end it) (point)))
        (-drop (1- arg))
        (car))
    (mf--next-overlay (- arg))))

(provide 'multifiles)

;;; multifiles.el ends here
