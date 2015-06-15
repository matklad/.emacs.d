(use-package dash
  :ensure t)


(defun smart-open-line ()
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent)
  (recenter))


(defun smart-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(defun isearch-delete-something ()
  "Delete non-matching text or the last character."
  ;; Mostly copied from `isearch-del-char' and Drew's answer on the page above
  (interactive)
  (if (= 0 (length isearch-string))
      (ding)
    (setq isearch-string
          (substring isearch-string
                     0
                     (or (isearch-fail-pos) (1- (length isearch-string)))))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string "")))
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))


(defun rename-buffer-and-file ()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Doesn't mess with special buffers."
  (interactive)
  (-each
   (->> (buffer-list)
     (-filter #'buffer-file-name)
     (--remove (eql (current-buffer) it)))
   #'kill-buffer))

(defun switch-theme (name)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name (custom-available-themes))))))
  (load-theme name t))


(global-set-key (kbd "M-o")     'smart-open-line)
(global-set-key (kbd "C-a")     'smart-move-beginning-of-line)
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "M-SPC")   'cycle-spacing)
(global-set-key (kbd "C-c r")   'rename-buffer-and-file)
(global-set-key (kbd "C-c k")   'kill-other-buffers)
(global-set-key (kbd "<f11>")   'toggle-frame-maximized)

(define-key isearch-mode-map (kbd "<backspace>")
  #'isearch-delete-something)
