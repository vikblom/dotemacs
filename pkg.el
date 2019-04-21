;; PACKAGING
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(if (equal user-login-name "A276720")
    (setq url-proxy-services
          '(("http" . "A276720@cloudpxgot1.srv.volvo.com:8080")
            ("https"    . "A276720@cloudpxgot1.srv.volvo.com:8080")
            ("ftp"      . "A276720@cloudpxgot1.srv.volvo.com:8080")
            ("no_proxy" . "^.*example.com"))))

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
