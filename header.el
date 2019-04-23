(defun sl/make-header ()
  ""
  (let* ((sl/full-header (abbreviate-file-name buffer-file-name))
         (sl/header (file-name-directory sl/full-header))
         (sl/drop-str "..."))
    (if (> (length sl/full-header)
           (window-body-width))
        (if (> (length sl/header)
               (window-body-width))
            (progn
              (concat sl/drop-str (substring sl/header
                                             (+ (- (length sl/header)
                                                   (window-body-width))
                                                (length sl/drop-str))
                                             (length sl/header))))
          (concat sl/header))
      (concat sl/header (file-name-nondirectory buffer-file-name)))))

(defun sl/display-header ()
  (setq header-line-format
        '("" ;; invocation-name
          (:eval (if (buffer-file-name)
                     (sl/make-header))))))

(add-hook 'buffer-list-update-hook
          'sl/display-header)
