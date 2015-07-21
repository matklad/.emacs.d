;;; init.el ---


(defvar local-dir user-emacs-directory
  "The root dir of the Emacs configuration.")

(defun local-file-name (file-name)
  (let* ((file-path (expand-file-name file-name local-dir))
         (parent-dir (file-name-directory file-path)))
    (unless (or (not parent-dir)
                (file-exists-p parent-dir))
      (make-directory parent-dir))
    file-path))


(defun load-local (file-name)
  (load (local-file-name file-name)))

(setq gc-cons-threshold 50000000)
(setq shell-file-name "bash")

(setq load-prefer-newer t)
(load-local "core/packages.el")
(load-local "rc/ui.el")
(load-local "rc/editor.el")
(load-local "rc/bindings.el")
(load-local "rc/coding.el")

;;; init.el ends here
