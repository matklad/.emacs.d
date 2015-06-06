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
(setq shell-file-name "/usr/bin/bash")

(setq load-prefer-newer t)
(load-local "core/packages.el")
(load-local "rc/ui.el")
(load-local "rc/editor.el")
(load-local "rc/bindings.el")
(load-local "rc/coding.el")

;;; init.el ends here
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((whitespace-style face empty trailing lines-tail))))
 '(undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/cache/undo/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
