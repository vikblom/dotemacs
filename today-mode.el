;;; -*- lexical-binding: t; -*-

(defun today-file ()
  (concat
   user-emacs-directory
   (file-name-as-directory "today")
   ;;(file-name-as-directory "~/.today")
   (format-time-string "%Y-%m-%d")
   ".txt"))

;; (defun today ()
;;   (interactive)
;;   (find-file (today-file))
;;   (delete-other-windows)
;;   (today-mode))

(define-derived-mode today-mode text-mode "today"
  "Major mode for writing about today."
  (load-fresh-theme 'flatui)
  (auto-fill-mode 1)
  ;;(set-window-margins (get-buffer-window) 10)
  (set-face-background 'fringe (face-background 'default))
  (setq left-fringe-width 100)
  (set-window-buffer (get-buffer-window) (current-buffer)))


(provide 'today-mode)
