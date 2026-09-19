;;; -*- lexical-binding: t; -*-

(defun dagens-file ()
  (concat
   (file-name-as-directory "~/sync/dagens")
   (format-time-string "%Y-%m-%d")
   ".txt"))

(defun center-text ()
  (if (eq major-mode 'dagens-mode)
      (progn
        (setq-local left-fringe-width
                    (max 0
                         ;; Pixel per column * columns
                         (* (/
                             (window-pixel-width (get-buffer-window))
                             (window-total-width))
                            ;; We have width - 80 of dead space.
                            ;; Divided by two sides.
                            (/ (- (window-total-width) 80) 2))))
        (set-window-buffer (get-buffer-window) (current-buffer))
        )))

(defun dagens ()
  (interactive)
  (find-file (dagens-file))
  (delete-other-windows)
  (dagens-mode)
  (add-hook 'window-state-change-hook 'center-text)
  )

(add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)


(define-derived-mode dagens-mode text-mode "dagens"
  "Major mode for writing about dagens."
  (load-fresh-theme 'doom-plain)
  (auto-fill-mode 1)
  (set-face-background 'fringe (face-background 'default))
  ;;(setq left-fringe-width 100)
  (set-window-buffer (get-buffer-window) (current-buffer)))


(provide 'dagens-mode)
