;; PACKAGING
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Custom keyword that conditionally disables
(add-to-list 'use-package-keywords ':onlyif)
(defun use-package-normalize/:onlyif (_name _keyword _arg) (car _arg))
(defun use-package-handler/:onlyif (name _keyword _arg rest state)
  (use-package-process-keywords name (if (eval _arg) rest) state))
