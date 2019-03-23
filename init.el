;; Startup
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)

(prefer-coding-system 'utf-8)


;; Builtints
(require 'seq)



;; Fonts
(require 'iso-transl)
(global-font-lock-mode t)

(defun font-exists? (font) (find-font (font-spec :name font)))
(set-frame-font
 (seq-find 'font-exists? '("Roboto Mono-10"
                           "Inconsolata-11"
                           "DejaVu Sans Mono-10")))


(cond
 ((find-font (font-spec :name "Roboto Mono-10"))
  (set-frame-font "Roboto Mono-10"))
 ((find-font (font-spec :name "Inconsolata-11"))
  (set-frame-font "Inconsolata-11"))
 ((find-font (font-spec :name "DejaVu Sans Mono-10"))
  (set-frame-font "DejaVu Sans Mono-10")))

(set-face-attribute 'default nil :font "Roboto Mono-10")
(setq font-lock-maximum-decoration t)

;; backup to folder by copying
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)

(setenv "PATH" (concat (getenv "PATH") ":~/.local/bin/"))
(setq exec-path (append exec-path '("~/.local/bin/")))


;; REMOTE
(put 'dired-find-alternate-file 'disabled nil)

;; BEHAVIOUR
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(delete-selection-mode t)
(global-auto-revert-mode t)
(setq auto-revert-remote-files t)
(setq split-width-threshold 140)
(setq require-final-newline t)

;; INDENTATION
(setq-default indent-tabs-mode nil)
(setq-default transient-mark-mode t)
(setq-default tab-width 4)

;; KEYBINDS
(global-set-key [f6] 'toggle-truncate-lines)

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

(global-unset-key (kbd "C-<end>"))
(global-unset-key (kbd "M-<home>"))
(global-unset-key (kbd "C-x C-SPC"))
(global-unset-key (kbd "<menuq>"))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)


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


;; PACKAGING
;; Tell emacs where stuff is
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/packages/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
(load-theme (seq-find 'find-theme '(gruber-darker wombat)) t)

(mapcar 'print '("foo" "bar"))

;; Global packages
(use-package recentf
  :ensure t
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (global-set-key (kbd "C-x C-r") 'recentf-open-files))


(use-package magit
  :ensure t
  :config
  (setq vc-handled-backends nil)
  (global-set-key (kbd "C-x C-g") 'magit-status))


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Alternatively use Control-c + tab
  ;;(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))


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
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("<M-down>" . (lambda () (interactive) (beginning-of-defun -1)))
              ("<M-up>" . beginning-of-defun)
              ("M-n" . (lambda () (interactive) (beginning-of-defun -1)))
              ("M-p" . beginning-of-defun)))


(use-package comint
  :bind (:map comint-mode-map
              ("C-l C-l" . comint-clear-buffer)))


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
                      :foreground "#444444")
  (setq whitespace-style '(face trailing lines-tail tabs tab-mark))
  (setq whitespace-line-column 80)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(global-set-key (kbd "<C-tab>") 'hippie-expand)
(global-set-key (kbd "C-TAB") 'hippie-expand)


;; C-lang
(use-package cc-mode
  :bind (:map c-mode-map
              ("<M-up>" . c-beginning-of-defun)
              ("<M-down>" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("M-p" . c-beginning-of-defun)
              ("M-n" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("C-c RET" . (lambda () (interactive) (compile "make -C ..")))
              ("<f5>" . c-common-cleanup))
  :config
  (electric-indent-mode -1)
  (setq c-default-style "linux"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'access-label -4)
  (c-set-offset 'topmost-intro-cont 0)
  ;;(casi-mode 1)
  (c-toggle-auto-newline 0)

  (require 'gud)
  ;;(define-key gud-mode-map (kbd "C-SPC") 'gud-break)
  (setq gdb-show-main t)
  (setq gdb-many-windows t)

  (require 'ctags-update)
  (global-set-key (kbd "C-c C-t") 'ctags-update))


;; Scheme-lang
(use-package scheme-mode
  :init
  (setq scheme-program-name '"csi")
  (setq scheme-mit-dialect nil)
  :bind (:map scheme-mode-map
              ("<C-return>" . scheme-send-last-sexp)
              ("<C-enter>" . scheme-send-last-sexp))
  :mode ("\\.scm\\'" . scheme-mode))




;; Julia-lang
(use-package julia-mode
  :if (locate-file "julia" exec-path)
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



;;Haskell setup
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" "4138944fbed88c047c9973f68908b36b4153646a045648a22083bd622d1e636d" default)))
 '(frame-brackground-mode (quote dark))
 '(package-selected-packages
   (quote
    (julia-repl gruber-darker-theme ctags-update yasnippet-snippets magit yasnippet use-package ujelly-theme paredit geiser fish-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
