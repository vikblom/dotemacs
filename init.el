;;; -*- lexical-binding: t; -*-

;;; Set up package managing
;;(setq package-check-signature nil)
(package-initialize)
(load "~/.emacs.d/pkg.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Deps
(require 'seq)

(setenv "PATH" (concat (getenv "PATH") ":" "~/.local/bin/" ":" "~/local/bin/"))
(setq exec-path (append '("~/.local/bin/" "~/local/bin/") exec-path))
;;(setq load-path (append load-path '("~/.local/bin/" "~/local/bin/")))

(defun init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun write-today ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(defun double-indent ()
  (interactive)
  (replace-regexp "^\\( +\\)" "\\1\\1" nil (mark) (point)))

(defun next-line-non-empty-column (arg)
  "Find next line, on the same column, skipping those that would
end up leaving point on a space or newline character."
  (interactive "p")
  (let* ((hpos (- (point) (point-at-bol)))
         (re (format "^.\\{%s\\}[^\n ]" hpos)))
    (cond ((> arg 0)
           (forward-char 1) ; don't match current position (can only happen at column 0)
           (re-search-forward re))
          ((< arg 0)
           (forward-char -1)           ; don't match current position.
           (re-search-backward re)
           (goto-char (match-end 0))))
    ;; now point is after the match, let's go back one column.
    (forward-char -1)))

;; FONT
(require 'iso-transl)
(prefer-coding-system 'utf-8)
(global-font-lock-mode t)

(defun font-exist-p (font) (find-font (font-spec :name font)))

(defun pref-font ()
  (seq-find 'font-exist-p '("Roboto Mono-10"
                            "Inconsolata-11"
                            "DejaVu Sans Mono-10")))

(add-to-list 'default-frame-alist '(font . "Roboto Mono-10"))

(setq font-lock-maximum-decoration t)


;; BACKUP
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)

;; REMOTE
(put 'dired-find-alternate-file 'disabled nil)

;; BEHAVIOUR
(progn (add-hook 'before-save-hook 'delete-trailing-whitespace)
       (delete-selection-mode t)
       (setq x-select-enable-clipboard t)
       (global-auto-revert-mode t)
       (setq auto-revert-remote-files t)
       (setq split-width-threshold 140)
       (setq require-final-newline t)
       (setq bookmark-save-flag 1)
       (fset 'yes-or-no-p 'y-or-n-p)
       (put 'upcase-region 'disabled nil)
       (put 'downcase-region 'disabled nil))

;; INDENTATION
(setq-default indent-tabs-mode nil
              transient-mark-mode t
              tab-width 4
              truncate-lines 'nil
              truncate-partial-width-windows 'nil)

;; KEYBINDS
(progn (global-set-key [f6] 'toggle-truncate-lines)
       (global-set-key (kbd "C-c C-q") 'comment-or-uncomment-region)

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
       (global-set-key (kbd "M-[") 'previous-multiframe-window)
       (global-set-key (kbd "M-]") 'next-multiframe-window)
       ;;(global-set-key (kbd "M-[") 'previous-buffer)
       ;;(global-set-key (kbd "M-]") 'next-buffer)

       (global-set-key (kbd "<C-tab>") 'hippie-expand)
       (global-set-key (kbd "C-TAB") 'hippie-expand)

       (global-unset-key (kbd "C-<end>"))
       (global-unset-key (kbd "M-<home>"))
       (global-unset-key (kbd "C-x C-SPC"))
       (global-unset-key (kbd "<menuq>"))

       (global-set-key (kbd "M-z") 'zap-up-to-char)
       (global-set-key (kbd "C-x C-b") 'ibuffer))


;; SCROLLING
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1
      scroll-margin 5)

;; APPEARANCE
(progn (setq frame-resize-pixelwise t)
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (menu-bar-mode -1)
       (add-hook 'prog-mode-hook 'linum-mode)
       (column-number-mode 1)
       (line-number-mode 1)
       (global-hl-line-mode 1)
       (show-paren-mode 1)
       (setq show-paren-delay 0))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))


;; Stack Overflow header for each open buffer
(load "~/.emacs.d/header.el")


;; Theme setup
(defun load-fresh-theme (theme)
  "Disables all active themes and loads a new theme."
  (interactive (list (intern
                      (completing-read "Load custom theme: "
                                       (mapcar 'symbol-name
                                               (custom-available-themes))))))
  (mapcar 'disable-theme custom-enabled-themes)
  (load-theme theme t))

(use-package srcery-theme
  :ensure t
  :config
  (setq srcery-black "#050505"))


(defun find-theme (theme)
  "Finds source .el of a theme by name. Nil if not on path."
  (locate-file
   (concat (symbol-name theme) "-theme.el")
   (custom-theme--load-path)))

(defun pref-theme ()
  (seq-find 'find-theme '(srcery
                          dracula
                          noctilux
                          wombat
                          gruber-darker)))

(cond ((not (pref-theme))) ;; Do nothing
      ((daemonp) (add-hook 'after-make-frame-functions
                           (lambda (frame)
                             (select-frame frame)
                             (load-theme (pref-theme) t))))
      ((window-system) (load-theme (pref-theme) t)))

;; Global packages
(use-package org
  :config
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-agenda-files (list "~/org")
        org-cycle-separator-lines 1
        org-startup-folded nil
        org-log-done nil))


(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (run-with-timer 0 (* 5 60) 'recentf-save-list)
  ;;(add-hook 'server-done-hook 'recentf-save-list)
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
(use-package yasnippet-snippets)


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
          scheme-mode
          geiser-mode
          geiser-repl-mode)
         . paredit-mode)
  :bind (:map paredit-mode-map
              ("<M-down>" . (lambda () (interactive) (beginning-of-defun -1)))
              ("<M-up>" . beginning-of-defun)
              ("M-n" . (lambda () (interactive) (beginning-of-defun -1)))
              ("M-p" . beginning-of-defun)))

(use-package lispy
  :defer t)

(use-package comint
  :bind (:map comint-mode-map
              ("C-l C-l" . comint-clear-buffer)
              ("C-p" . comint-previous-input)
              ("C-n" . comint-next-input))
  :config
  (setq comint-scroll-to-bottom-on-output t
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
  (setq whitespace-style '(face trailing))
  (setq whitespace-line-column 80)
  (add-hook 'prog-mode-hook 'whitespace-mode))


(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings 'super)
  (setq windmove-wrap-around t)
  :bind (("s-h" . windmove-left)
         ("s-j" . windmove-down)
         ("s-k" . windmove-up)
         ("s-l" . windmove-right)))


;; HELM
;; https://tuhdo.github.io/helm-intro.html
(use-package helm
  :bind (:map global-map
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-find)
         ("C-c C-j" . helm-semantic-or-imenu))
  :bind (:map helm-map
              ("C-j" . helm-ff-RET))
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'below
        helm-M-x-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t))


(use-package helm-ls-git
  :bind ("C-c C-p" . helm-browse-project))


;; Today mode
(load "~/.emacs.d/today-mode.el")

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
  :onlyif (executable-find "ctags")
  :bind (:map c-mode-map ("<f5>" . ctags-update)))


(defun create-etags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((default-directory dir-name))
    (eshell-command
     (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name))
    (visit-tags-table (concat dir-name "TAGS"))))

(use-package clang-format
  :onlyif (executable-find "clang-format")
  :defer t
  :config
  (setq clang-format-style "{BasedOnStyle: WebKit, PointerAlignment: Right}")
  ;:bind (:map c-mode-map ("C-c f" . clang-format-buffer))
  )


;; Scheme-lang
(use-package geiser
  :onlyif (executable-find "csi")
  :config
  (setq geiser-active-implementations '(chicken)
        geiser-chicken-compile-geiser-p nil))


;; (use-package ac-geiser
;;   :onlyif (executable-find "chicken")
;;   :hook geiser-mode)


;; GO-lang
(use-package go-mode
  :config
  (setenv "GOPATH" "~/kod/go/")
  (setenv "GOBIN" "~/kod/go/bin/")
  (setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOBIN")))
  (setq exec-path (append exec-path (list (getenv "GOBIN"))))
  (with-eval-after-load 'go-mode
    (require 'go-autocomplete))
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-j" . nil)))

;; Julia-lang
;; (use-package julia-mode
;;   :onlyif (executable-find "julia")
;;   :config
;;   (setq auto-mode-alist (delq (assoc "\\.jl\\'" auto-mode-alist) auto-mode-alist))
;;   :mode ("\\.jl\\'" . julia-mode))

;; (use-package julia-repl
;;   :onlyif (executable-find "julia")
;;   :config
;;   (add-hook 'julia-mode-hook 'julia-repl-mode)
;;   :bind (:map julia-mode-map
;;               ("C-c C-j" . julia-repl)
;;               ("<C-return>" . julia-repl-send-region-or-line)
;;               ("<C-enter>" . julia-repl-send-region-or-line)))



;; Python-lang
(defun python-hook ()
  (setq python-shell-interpreter "ipython3"
        python-indent 4)
  (setq python-shell-interpreter-args "-c \"%load_ext autoreload\" --simple-prompt -i")
  ;;(setq python-shell-interpreter-args "-i")
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
  (setq-default py-split-windows-on-execute-function 'split-window-vertically)
  (define-key python-mode-map (kbd "C-c C-p") nil))

(add-hook 'python-mode-hook 'python-hook)


;; R-lang
;; (use-package ess-site
;;   :disabled
;;   :config
;;   (setq comint-scroll-to-buttom-on-output t
;;         comint-scroll-to-buttom-on-input t
;;         comint-move-point-for-output t
;;         ess-history-directory "~/.R/"
;;         ess-history-file "~/.R/history"
;;         ess-eval-visibly nil)
;;   :bind (:map ess-mode-map
;;               ("<C-return>" . ess-eval-region-or-function-or-paragraph)
;;               ("<C-enter>" . ess-eval-region-or-function-or-paragraph)
;;               ("C-c C-n" . ess-eval-line-and-step))
;;   :mode ("\\.R\\'" . R-mode))

(setq snake-initial-x 3
      snake-width 15)

;; Matlab mode
;;(autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
;;(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(use-package matlab
  :defer t
  :config
  ;(matlab-cedet-setup)
  (setq matlab-indent-function-body t)
  (auto-complete-mode 1)
  :bind (:map matlab-mode-map
              ("<C-return>" . matlab-shell-run-region-or-line)
              ("<C-enter>" . matlab-shell-run-region-or-line)
              ("C-c C-m" . matlab-shell)
              ("<M-down>" . matlab-end-of-defun)
              ("<M-up>" . matlab-beginning-of-defun)
              ("M-n" . matlab-end-of-defun)
              ("M-p" . matlab-beginning-of-defun)))


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
