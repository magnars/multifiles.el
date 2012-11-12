(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq multifiles-root-path project-directory)
  (setq multifiles-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path multifiles-root-path)
(add-to-list 'load-path multifiles-util-path)
(add-to-list 'load-path (expand-file-name "espuds" multifiles-util-path))

(require 'multiple-cursors)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*multifiles-test*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (delete-selection-mode 0)
 (subword-mode 0)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
