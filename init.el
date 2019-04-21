;; (setq inhibit-startup-screen t
;;       inhibit-startup-message t)

(prefer-coding-system 'utf-8)

;; Set up package managing
(package-initialize)
(load "~/.emacs.d/pkg.el")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Deps
(require 'seq)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)


;; Fonts
(require 'iso-transl)
(global-font-lock-mode t)

(defun font-exists? (font) (find-font (font-spec :name font)))
(set-frame-font
 (seq-find 'font-exists? '("Roboto Mono-10"
                           "Inconsolata-11"
                           "DejaVu Sans Mono-10")))

(setq font-lock-maximum-decoration t)

;; backup to folder by copying
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)

(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin/"))
(setq exec-path (append exec-path '("~/.local/bin/")))


;; REMOTE
(put 'dired-find-alternate-file 'disabled nil)

;; BEHAVIOUR
(progn
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (delete-selection-mode t)
  (global-auto-revert-mode t)
  (setq auto-revert-remote-files t)
  (setq split-width-threshold 140)
  (setq require-final-newline t))

;; INDENTATION
(setq-default indent-tabs-mode nil
              transient-mark-mode t
              tab-width 4)

;; KEYBINDS
(progn (global-set-key [f6] 'toggle-truncate-lines)

       (global-set-key (kbd "C-s") 'isearch-forward-regexp)
       (global-set-key (kbd "C-r") 'isearch-backward-regexp)
       (global-set-key (kbd "C-M-s") 'isearch-forward)
       (global-set-key (kbd "C-M-r") 'isearch-backward)

       (global-set-key (kbd "C-z") 'pop-to-mark-command)


       (global-set-key (kbd "C-<next>") (lambda ()
                                          (interactive)
                                          (other-window 1)))
       (global-set-key (kbd "C-<prior>") (lambda ()
                                           (interactive)
                                           (other-window -1)))
       (global-set-key (kbd "C-å") 'previous-multiframe-window)
       (global-set-key (kbd "C-ä") 'next-multiframe-window)
       (global-set-key (kbd "M-å") 'previous-buffer)
       (global-set-key (kbd "M-ä") 'next-buffer)

       (global-set-key (kbd "<C-tab>") 'hippie-expand)
       (global-set-key (kbd "C-TAB") 'hippie-expand)

       (global-unset-key (kbd "C-<end>"))
       (global-unset-key (kbd "M-<home>"))
       (global-unset-key (kbd "C-x C-SPC"))
       (global-unset-key (kbd "<menuq>"))

       (global-set-key (kbd "M-z") 'zap-up-to-char))


;; (defun match-paren (arg)
;;   "Go to the matching paren if on a paren; otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))
;; (global-set-key "%" 'match-paren)

(defun init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; SCROLLING
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1
      scroll-margin 5)

;; APPEARANCE
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(add-hook 'prog-mode-hook 'linum-mode)
(column-number-mode 1)
(line-number-mode 1)
(global-hl-line-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

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


;; Theme setup
(defun load-fresh-theme (theme)
  "Disables all active themes and loads a new theme."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                  (mapcar 'symbol-name
                                          (custom-available-themes))))))
  (mapcar 'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun find-theme (theme)
  "Finds source .el of a theme by name. Nil if not on path."
  (locate-file
   (concat (symbol-name theme) "-theme.el")
   (custom-theme--load-path)))

(let ((theme (seq-find 'find-theme
                       '(dracula wombat noctilux gruber-darker))))
  (cond ((not theme)) ;; Do nothing
        ((daemonp) (add-hook 'after-make-frame-functions
                             (lambda (frame)
                               (select-frame frame)
                               (load-theme theme t))))
        ((window-system) (load-theme theme t))))

;; Global packages
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  :bind ("C-x C-r" . recentf-open-files))


(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends nil))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("<backtab>" . yas-expand)))


(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>")
  (setq ac-auto-start nil))


(use-package paredit
  :ensure t
  :config
  :hook ((emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode)
         . paredit-mode)
  :bind (:map paredit-mode-map
              ("<M-down>" . (lambda () (interactive) (beginning-of-defun -1)))
              ("<M-up>" . beginning-of-defun)
              ("M-n" . (lambda () (interactive) (beginning-of-defun -1)))
              ("M-p" . beginning-of-defun)))


(use-package comint
  :bind (:map comint-mode-map
              ("C-l C-l" . comint-clear-buffer))
  :config
  (setq
   comint-scroll-to-bottom-on-output t
   comint-scroll-to-bottom-on-input t
   comint-move-point-for-output t))


(use-package whitespace
  :ensure t
  :config
  (setq whitespace-display-mappings
        '((space-mark 32 [46])
          (newline-mark 10 [182 10])
          (tab-mark 9 [9655 9] [92 9])))
  (set-face-attribute 'whitespace-trailing
                      nil
                      :background "#ff8888"
                      :foreground "gray20")
  (set-face-attribute 'whitespace-line
                      nil
                      :background "gray20"
                      :foreground "#ff8888")
  (set-face-attribute 'whitespace-tab
                      nil
                      :foreground "#999999")
  (setq whitespace-style '(face trailing lines-tail tabs tab-mark))
  (setq whitespace-line-column 80)
  (add-hook 'prog-mode-hook 'whitespace-mode))


;; C-lang
(use-package cc-mode
  :bind (:map c-mode-map
              ("<M-up>" . c-beginning-of-defun)
              ("<M-down>" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("M-p" . c-beginning-of-defun)
              ("M-n" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("C-c RET" . (lambda () (interactive) (compile "make -C .."))))
  :config
  (electric-indent-mode -1)
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'access-label -4)
  (c-set-offset 'topmost-intro-cont 0)
  (c-toggle-auto-newline 0)

  (require 'gud)
  ;;(define-key gud-mode-map (kbd "C-SPC") 'gud-break)
  (setq gdb-show-main t)
  (setq gdb-many-windows t))

(use-package ctags-update
  :onlyif (locate-file "ctags" exec-path)
  :defer t
  :bind (:map c-mode-map ("<f5>" . ctags-update)))

(use-package clang-format
  :onlyif (locate-file "clang-format" exec-path)
  :config
  (setq clang-format-style "{BasedOnStyle: WebKit, PointerAlignment: Right}")
  :bind ("C-c f" . clang-format-buffer))


;; Scheme-lang
(use-package geiser
  :onlyif (executable-find "chicken")
  :config
  (setq geiser-active-implementations '(chicken)))
;; (use-package scheme-mode
;;   :onlyif (locate-file "chicken" exec-path)
;;   :init
;;   (setq scheme-program-name '"csi")
;;   (setq scheme-mit-dialect nil)
;;   :bind (:map scheme-mode-map
;;               ("<C-return>" . scheme-send-last-sexp)
;;               ("<C-enter>" . scheme-send-last-sexp))
;;   :mode ("\\.scm\\'" . scheme-mode))


;; Julia-lang
(use-package julia-mode
  :onlyif (executable-find "julia")
  :ensure t
  :mode ("\\.jl\\'" . julia-mode)
  :init
  (require 'julia-repl)
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  :bind (:map julia-mode-map
              ("C-c C-j" . run-julia)
              ("<C-return>" . julia-shell-run-region-or-line)
              ("<C-enter>" . julia-shell-run-region-or-line)
              ("C-c C-s" . julia-shell-save-and-go)))


;; Python-lang
(defun python-hook ()
  (setenv "PYTHONPATH" '"/home/viktor/projects/motion-mining/")
  (setenv "TF_CPP_MIN_LOG_LEVEL" "2")
  (setq python-shell-interpreter "ipython3")
  (setq python-indent 4)
  (setq python-shell-interpreter-args
        "-c \"%load_ext autoreload\" --simple-prompt -i")
  (defun refresh ()
    (interactive)
    (save-some-buffers)
    (python-shell-send-string "%autoreload"))
  (add-hook 'inferior-python-mode-hook
            (lambda () (local-set-key (kbd "<f5>") 'refresh)))
  (local-set-key (kbd "<C-return>") 'python-shell-send-region)
  (local-set-key (kbd "<C-enter>") 'python-shell-send-region)
  (local-set-key (kbd "<f5>") 'refresh)
  (local-set-key (kbd "<M-up>")
                 (lambda () (interactive)
                   (python-nav-backward-defun)
                   (recenter 10)))
  (local-set-key (kbd "<M-down>")
                 (lambda () (interactive)
                   (python-nav-forward-defun)
                   (recenter 10)))
  (setq-default py-split-windows-on-execute-function 'split-window-vertically))

(add-hook 'python-mode-hook 'python-hook)


;; R-lang
(use-package ess-site
  :disabled
  :bind (:map ess-mode-map
              ("<C-return>" .
               ess-eval-region-or-function-or-paragraph))
  :bind (:map ess-mode-map
              ("<C-enter>" .
               ess-eval-region-or-function-or-paragraph))
  :bind (:map ess-mode-map
              ("<C-S-return>" .
               ess-eval-region-or-function-or-paragraph-and-step))
  :bind (:map ess-mode-map
              ("<C-S-enter>" .
               ess-eval-region-or-function-or-paragraph-and-step))
  :bind (:map ess-mode-map
              ("C-c C-n" . ess-eval-line-and-step))
  :config
  (setq comint-scroll-to-buttom-on-output t
        comint-scroll-to-buttom-on-input t
        comint-move-point-for-output t
        ess-history-directory "~/.R/"
        ess-history-file "~/.R/history"
        ess-eval-visibly nil)
  ;;(ess-toggle-underscore t)
  :mode ("\\.R\\'" . R-mode))

;; Matlab mode
;;(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;;(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
;; (use-package matlab
;;   :defer t
;;   :config
;;   (matlab-cedet-setup)
;;   (setq matlab-indent-function t)
;;   (load-library "matlab-load")
;;   (auto-complete-mode 1)
;;   :bind (:map matlab-mode-map
;;               ("<C-return>" . matlab-shell-run-region-or-line)
;;               ("<C-enter>" . matlab-shell-run-region-or-line)
;;               ("C-c C-m" . matlab-shell)))


;; LaTeX
;; (use-package tex
;;   :defer t
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;   (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;;   (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
;;   ;;(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;;   (setq font-latex-fontify-sectioning 'color)
;;   (defun my-LaTeX-mode()
;;     (add-to-list 'TeX-view-program-list
;;                  '("Evince" "evince --page-index=%(outpage) %o"))
;;     (add-to-list 'LaTeX-verbatim-environments "lstlisting")
;;     (setq TeX-view-program-selection '((output-pdf "Evince")))
;;     (setq TeX-source-correlate-start-server t)
;;     (TeX-global-PDF-mode t)
;;     (setq TeX-parse-self t)
;;     (setq-default TeX-master nil)
;;     (setq TeX-auto-save t)
;;     (setq TeX-save-query nil)
;;     (local-set-key (kbd "<C-return>") 'TeX-command-master)
;;     (local-set-key (kbd "<C-enter>") 'TeX-command-master)
;;     (local-set-key (kbd "<C-S-return>") 'TeX-command-run-all)
;;     (local-set-key (kbd "<C-S-enter>") 'TeX-command-run-all)
;;     (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)
;;     ))
