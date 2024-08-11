;;; -*- lexical-binding: t; -*-
;;
;; Use GCC emacs
;; https://akrl.sdf.org/gccemacs.html
;; https://www.emacswiki.org/emacs/GccEmacs
;; CC=gcc-10 ./configure --with-cairo --with-native-compilation
;;
;; Recompiling elisp faster?
;; (setq comp-speed 3)
;; (native-compile-async "~/.emacs.d/elpa/" 1 t)
;; (native-compile-async "<path/to/system/elisp/files>" <n> t)
;;
;; On MacOs
;; Might work: https://github.com/emacs-mirror/emacs/commits/feature/more-fds ?
;; Compile with bumped open file limit: https://en.liujiacai.net/2022/09/03/emacs-maxopenfiles/
;; brew install libxml2 gcc libgccjit jansson autoconf texinfo imagemagick
;; set -x CPATH (xcrun --show-sdk-path)/usr/include:(xcrun --show-sdk-path)/usr/include/libxml2
;; ./configure --with-modules --with-ns --disable-ns-self-contained --with-native-compilation "CFLAGS=-DFD_SETSIZE=10240 -DDARWIN_UNLIMITED_SELECT"

(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))

(setq warning-minimum-level :error)

;;; Set up package managing
;;(setq package-check-signature nil)
(package-initialize)
(load "~/.emacs.d/pkg.el")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(defun init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun write-today ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(use-package fish-mode
  :onlyif (executable-find "fish")
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (if (not (eq system-type 'darwin))
      (setq exec-path-from-shell-name (executable-find "fish"))
    )
  (exec-path-from-shell-initialize))

(use-package direnv
  :onlyif (executable-find "direnv")
  :ensure t
  :config
  (direnv-mode)
  (setq direnv-always-show-summary nil))

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

(defun org-src-region ()
  "Wrap marked region in a org SRC block."
  (interactive)
  (let ((lang (read-from-minibuffer "language: ")))
    (save-excursion
      (goto-char (region-beginning))
      (if (not (eq (point) (line-beginning-position)))
          (newline))
      (insert (concat "#+BEGIN_SRC " lang))
      (if (not (eq (point) (line-end-position)))
          (newline)))
    (save-excursion
      (if (not (eq (point) (line-beginning-position)))
          (newline))
      (goto-char (region-end))
      (insert "#+END_SRC" ?\n)
      (if (not (eq (point) (line-end-position)))
          (newline)))))

;; (setq compilation-last-buffer nil)
(defun compile-again (pfx)
  """Run the same compile as the last time.

If there was no last time, or there is a prefix argument, this acts like
M-x compile.
"""
 (interactive "p")
 (if (and (eq pfx 1)
	  compilation-last-buffer)
     (progn
       (set-buffer compilation-last-buffer)
       (recompile)
       )
   (call-interactively 'compile)))
;; color compilation buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun uuid-create ()
  "Return a newly generated UUID. This uses a simple hashing of variable data."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Inserts a new UUID at the point."
  (interactive)
  (insert (uuid-create)))

;; FONT
(require 'iso-transl)
(prefer-coding-system 'utf-8)
;; (global-font-lock-mode t)
;; (setq font-lock-support-mode nil)

(defun font-exist-p (font) (find-font (font-spec :name font)))

(defun pref-font ()
  (if (window-system)
      (let ((font (seq-find 'font-exist-p (if (eq system-type 'darwin)
                                              '("Inconsolata-14"
                                                "Roboto Mono-12")
                                            '(
                                              "Inconsolata-9"
                                              "Roboto Mono-8"
                                              "Fira Code-8"
                                              "Meslo LG S-8"
                                              "DejaVu Sans Mono-9"
                                              )))))
        (set-face-attribute 'default nil :font font))))

(if (daemonp)
    (progn
      (add-hook 'after-make-frame-functions
                (lambda (frame) (pref-font)))
      (add-hook 'server-switch-hook #'raise-frame))
  (pref-font))

(if (eq system-type 'darwin) (server-start))


;; BACKUP
(setq backup-directory-alist `(("." . "~/.emacs.d/backups/")))
(setq backup-by-copying t)

;; REMOTE
(put 'dired-find-alternate-file 'disabled nil)

;; BEHAVIOUR
(progn (add-hook 'before-save-hook 'delete-trailing-whitespace)
       (setq compilation-scroll-output t)
       (delete-selection-mode t)
       (setq x-select-enable-clipboard t)
       (setq x-select-enable-clipboard-manager nil)
       ;; (global-auto-revert-mode t)
       ;; (setq auto-revert-remote-files t)
       (setq split-height-threshold 100) ;; Impossibly tall
       (setq split-width-threshold 160)  ;; Two 80s
       (setq require-final-newline t)
       (setq bookmark-save-flag 1)
       (fset 'yes-or-no-p 'y-or-n-p)
       (put 'upcase-region 'disabled nil)
       (put 'downcase-region 'disabled nil)
       (electric-pair-mode 1)
       ;; performance
       read-process-output-max (* 1024 1024) ;; 1mb
       gc-cons-threshold 100000000 ;; 100 mb
       )

(setq display-buffer-base-action
      '((display-buffer-reuse-window
         display-buffer-same-window)
        . ((reusable-frames . t))))

;; Avoid resizing.
(customize-set-variable 'even-window-sizes nil)

;; INDENTATION
(setq-default indent-tabs-mode nil
              transient-mark-mode t
              tab-width 4
              truncate-lines t
              truncate-partial-width-windows 'nil
              fill-column 80)

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
       (global-set-key (kbd "C-M-o") 'previous-multiframe-window)
       (global-set-key (kbd "M-o") 'next-multiframe-window)
       ;;(global-set-key (kbd "M-[") 'previous-buffer)
       ;;(global-set-key (kbd "M-]") 'next-buffer)

       (global-unset-key (kbd "C-<end>"))
       (global-unset-key (kbd "M-<home>"))
       (global-unset-key (kbd "C-x C-SPC"))
       (global-unset-key (kbd "<menuq>"))

       (global-set-key (kbd "M-z") 'zap-up-to-char)
       (global-set-key (kbd "C-x C-b") 'ibuffer)

       (global-set-key (kbd "C-c k") 'compile-again))


;; SCROLLING
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      pixel-scroll-precision-mode t
      scroll-step 1
      scroll-margin 5)

;; APPEARANCE
(setq fancy-splash-image "~/.emacs.d/misc/emacs.png")

(progn (setq frame-resize-pixelwise t)
       (tool-bar-mode -1)
       (scroll-bar-mode -1)
       (menu-bar-mode -1)
       (setq use-dialog-box nil)
       (column-number-mode 1)
       (line-number-mode 1)
       (global-hl-line-mode 1)
       (show-paren-mode 1)
       (setq show-paren-delay 0
             show-paren-context-when-offscreen 'overlay))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

;; Treesitter
;; Installing grammars:
;; (setq treesit-language-source-alist
;;    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(if (treesit-available-p)
    (setq major-mode-remap-alist
          '((go-mode . go-ts-mode)
            (bash-mode . bash-ts-mode)
            (json-mode . json-ts-mode)
            ;; (markdown-mode . markdown-ts.mode) Doesn't exist?
            (yaml-mode . yaml-ts-mode))))
(setopt treesit-font-lock-level 4)

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

(use-package ample-theme
  :ensure t)

(use-package doom-themes
  :ensure t)

(defun find-theme (theme)
  "Finds source .el of a theme by name. Nil if not on path."
  (locate-file
   (concat (symbol-name theme) "-theme.el")
   (custom-theme--load-path)))

(defun pref-theme ()
  (seq-find 'find-theme '(
                          doom-sourcerer ; dark with blue hints
                          doom-palenight ; faded mid blue
                          doom-henna
                          doom-wilmersdorf ; gentoo chill
                          doom-one
                          doom-1337 ; dark brighter text
                          doom-badger
                          doom-nord
                          ample-flat ; brownish muted colors
                          doom-gruvbox
                          doom-tomorrow-night ; blueish muted colors
                          doom-opera ; grey
                          doom-ayu-mirage ; navy contrast yellows
                          doom-plain-dark
                          srcery
                          dracula ; gentoo no chill
                          noctilux
                          wombat
                          gruber-darker))
   )

(cond ((not (pref-theme))) ;; Do nothing
      ((daemonp) (add-hook 'after-make-frame-functions
                           (lambda (frame)
                             (select-frame frame)
                             (load-theme 'doom-sourcerer t))))
      (t (load-theme (pref-theme) t)))

;; (if (eq (pref-theme) 'doom-1337)
;;     (set-face-attribute 'highlight
;;                         nil
;;                         :background "gray40"))

;; (custom-theme-set-faces 'doom-1337
;;                         '(highlight ((t (:background "gray40"))))
;;                         )

;; Global packages

(setq winner-mode 't)

(use-package sql)
(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))

;; http://notesyoujustmightwanttosave.blogspot.com/2011/12/org-speed-keys.html
(use-package org
  :config
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq ;;org-use-speed-commands 't
   org-agenda-files (list "~/org")
   org-blank-before-new-entry '((heading . t) (plain-list-item . auto))
   org-cycle-separator-lines 1
   org-startup-folded 'folded
   org-startup-indented 't
   org-log-done nil
   ;; Babel
   org-confirm-babel-evaluate nil
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   )
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (sql . t))))

(use-package pbcopy
  :onlyif (eq system-type 'darwin)
  :init
  (turn-on-pbcopy))

(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  (setq recentf-max-menu-items 25)
  (run-with-timer 0 (* 5 60) 'recentf-save-list)
  ;;(add-hook 'server-done-hook 'recentf-save-list)
  :bind ("C-x C-r" . recentf-open-files))


(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-set-leader 'normal (kbd "<SPC>"))
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo)
  ;; Emacs in terminal cannot tell <tab> from TAB (which is the same as C-i).
  (define-key evil-motion-state-map (kbd "TAB") nil)
  ;; Collides with xref-find-definition
  (define-key evil-normal-state-map (kbd "M-.") nil))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package magit
  :ensure t
  :config
  (evil-global-set-key 'normal (kbd "<leader> g g") 'magit-status)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (setq vc-handled-backends nil
        magit-log-section-commit-count 20
        magit-prefer-push-default 't
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally)
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (commit . show) (unpushed . show) (status . show)))
  ;; (custom-set-faces
  ;;  '(ediff-current-diff-A ((t (:inherit 'magit-diff-removed))))
  ;;  '(ediff-current-diff-B ((t (:inherit 'magit-diff-added))))
  ;;  '(ediff-current-diff-C ((t (:inherit 'magit-diff-none))))

  ;;  '(ediff-fine-diff-A ((t (:inherit 'magit-diff-removed-highlight))))
  ;;  '(ediff-fine-diff-B ((t (:inherit 'magit-diff-added-highlight))))
  ;;  '(ediff-fine-diff-C ((t (:inherit 'magit-diff-none))))
  ;;  )

  ;; (setq face-new-frame-defaults
  ;;       (cl-delete-if (lambda (face) (string-prefix-p "ediff" (symbol-name (car face))))
  ;;                     face-new-frame-defaults))
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; Stop the company suggestion from hanging around and
  ;; interfering with the placeholders.
  (add-hook 'yas-before-expand-snippet-hook 'company-cancel)
  :bind (:map yas-minor-mode-map
              ("<tab>" . nil)
              ("TAB" . nil)
              ("<backtab>" . yas-expand)))
(use-package yasnippet-snippets
  :ensure t)

;; (use-package auto-complete
;;   :ensure t
;;   :config
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;   (ac-config-default)
;;   (ac-set-trigger-key "TAB")
;;   (ac-set-trigger-key "<tab>")
;;   (setq ac-auto-start nil))

(use-package paredit
  :ensure t
  :config
  :hook ((emacs-lisp-mode
          ;; eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          geiser-mode
          geiser-repl-mode)
         . paredit-mode)
  :bind (:map paredit-mode-map
              ;; ("<M-down>" . (lambda () (interactive) (beginning-of-defun -1)))
              ;; ("<M-up>" . beginning-of-defun)
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


;; (use-package windmove
;;   :ensure t
;;   :config
;;   (windmove-default-keybindings 'super)
;;   (setq windmove-wrap-around t)
;;   :bind (("s-h" . windmove-left)
;;          ("s-j" . windmove-down)
;;          ("s-k" . windmove-up)
;;          ("s-l" . windmove-right)))


;; HELM
;; https://tuhdo.github.io/helm-intro.html
(use-package helm
  :bind (:map global-map
         ("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-find)
         ("M-i" . helm-semantic-or-imenu))
  :bind (:map helm-map
              ("C-j" . helm-ff-RET))
  :config
  (helm-mode 1)
  (setq helm-split-window-default-side 'below
        helm-split-window-inside-p t
        helm-M-x-fuzzy-match t
        helm-split-window-inside-p t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t)
  ;; https://github.com/emacs-helm/helm/issues/648
  (setq ffap-machine-p-known 'reject)
  (defun helm-skip-dots (old-func &rest args)
    "Skip . and .. initially in helm-find-files.  First call OLD-FUNC with ARGS."
    (apply old-func args)
    (let ((sel (helm-get-selection)))
      (if (and (stringp sel) (string-match "/\\.$" sel))
          (helm-next-line 2)))
    (let ((sel (helm-get-selection))) ; if we reached .. move back
      (if (and (stringp sel) (string-match "/\\.\\.$" sel))
          (helm-previous-line 1))))
  (advice-add #'helm-preselect :around #'helm-skip-dots)
  (advice-add #'helm-ff-move-to-first-real-candidate :around #'helm-skip-dots))


(use-package helm-ls-git
  :bind ("<leader> b p" . helm-browse-project))

(use-package company
  :ensure t
  :bind (:map company-mode-map
              ("C-TAB" . company-complete)
              ("<C-tab>" . company-complete))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2))

;; Integrates with LSP
;; Leading bind C-c ! ...
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-keymap-prefix
        (define-key flycheck-mode-map (kbd "C-c f") flycheck-command-map))
  (add-hook 'js-json-mode-hook 'flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "<leader> l")
  :config
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;;(lsp-register-custom-settings '(("gopls.semanticTokens" t t)))
  ;; (lsp-register-custom-settings '(("gopls.usePlaceholders" t t)))
  (lsp-register-custom-settings '(("gopls.analyses.composites" t f)))
  (setq
   lsp-eldoc-enable-hover t
   lsp-modeline-diagnostics-enable nil
   lsp-signature-auto-activate nil
   lsp-signature-render-documentation nil

   ;; Semantic tokens can slow down rendering.
   ;; NOTE: Required gopls setting.
   ;; lsp-semantic-tokens-enable t
   ;; lsp-semantic-tokens-honor-refresh-requests t

   ;;lsp-diagnostic-package :none
   ;;lsp-enable-on-type-formatting nil
   ;; lsp-log-io t
   lsp-signature-render-documentation nil
   lsp-lens-enable nil
   lsp-headerline-breadcrumb-enable t
   lsp-headerline-breadcrumb-icons-enable nil
   lsp-headerline-breadcrumb-enable-diagnostics nil
   lsp-file-watch-threshold 1000 ;; enough for go stdlib
   ;; performance
   read-process-output-max (* 1024 1024) ;; 1mb
   gc-cons-threshold 100000000 ;; 100mb
   )
  :hook ((go-ts-mode . lsp-deferred)
         (clojure-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         ;;(c-mode . lsp-deferred)
         ;;(c++-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t)))
         )
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 25)
  :config
  (setq lsp-ui-doc-show-with-mouse nil)
  :bind (:map lsp-ui-doc-mode-map ("C-c i" . lsp-ui-doc-focus-frame)))

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package projectile
  ;; projectile + ripgrep
  ;; https://github.com/bbatsov/projectile/issues/1777
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-commander)
  (setq projectile-globally-ignored-directories
        '(".idea"
          ".vscode"
          ".ensime_cache"
          ".eunit"
          ".git"
          ".hg"
          ".fslckout"
          "_FOSSIL_"
          ".bzr"
          "_darcs"
          ".tox"
          ".svn"
          ".stack-work"
          ".ccls-cache"
          ".cache"
          ".clangd"
          "env"
          "/nix/store"))
  :bind (:map projectile-mode-map
              ("<leader> p" . projectile-command-map))
  ;; :config
  ;; (evil-define-key 'normal projectile-mode-map (kbd "<leader> p") 'projectile-command-map)
  )

(use-package helm-projectile
  :ensure t)

(use-package perspective
  :ensure t
  ;; :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "<leader> o"))  ; pick your own prefix key here
  ;; (evil-define-key 'normal projectile-mode-map (kbd "<leader> p") 'projectile-command-map)
  :init
  (persp-mode))

(use-package yaml-ts-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively nil))

;; Today mode
(load "~/.emacs.d/today-mode.el")

;; Nix-lang
(use-package nix-mode
  :ensure t
  :onlyif (executable-find "nix"))

;; C-lang
(use-package cc-mode
  :bind (:map c-mode-map
              ;; ("<M-up>" . c-beginning-of-defun)
              ;; ("<M-down>" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("M-p" . c-beginning-of-defun)
              ("M-n" . (lambda () (interactive) (c-beginning-of-defun -1)))
              ("C-c RET" . (lambda () (interactive) (compile "make -C .."))))
  :config
  ;; (setq electric-indent-mode nil)
  (setq c-default-style "linux"
        c-basic-offset 4
        c-toggle-auto-newline 1)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'access-label -4)
  (c-set-offset 'topmost-intro-cont 0)

  ;; (require 'gud)
  ;;(define-key gud-mode-map (kbd "C-SPC") 'gud-break)
  ;; (setq gdb-show-main t)
  ;; (setq gdb-many-windows t)
  )


(use-package ctags-update
  :onlyif (executable-find "ctags")
  :bind (:map c-mode-map ("<f5>" . ctags-update)))


;; C++-lang
;; (use-package modern-cpp-font-lock
;;   :ensure t)
;; (use-package cmake-mode
;;   :onlyif (executable-find "cmake")
;;   :config
;;   (setq cmake-tab-width 4))

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

;; GO-lang
;; go get golang.org/x/tools/cmd/...
;; go get golang.org/x/tools/gopls@latest
;; ?go get github.com/rogpeppe/godef
(use-package go-mode
  :onlyif (executable-find "go")
  :ensure t)
(use-package go-ts-mode
  :onlyif (executable-find "go")
  :ensure t
  ;;(with-eval-after-load 'go-mode (require 'go-autocomplete))
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  :custom (lsp-go-gopls-server-args '("-logfile=/tmp/gopls-client.log"
                                      ;; "-verbose"
                                      ;; "-rpc.trace"
                                      ;; "-remote=unix;/var/folders/g5/x31g1yjj74b39_c1kbg4vwzw0000gp/T/gopls-daemon.viktor"
                                      "-remote.debug=localhost:8008"
                                      "-remote.logfile=/tmp/gopls-daemon.log"))
  :config
  (setq go-ts-mode-indent-offset 4) ;; ???
  (defun golang-clean-buffer ()
    (interactive)
    (progn
      (save-some-buffers)
      (lsp-organize-imports)
      (lsp-format-buffer)
      (save-buffer)))
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq-local compile-command "go test")
              ;;(setq-local compilation-read-command nil)
              ;; Copy the fill paragraph n friends setup from go-mode.
              (setq-local paragraph-start
                          (concat "[[:space:]]*\\(?:"
                                  comment-start-skip
                                  "\\|\\*/?[[:space:]]*\\|\\)$"))
              (setq-local paragraph-separate paragraph-start)
              (setq-local fill-paragraph-function #'go-fill-paragraph)
              (setq-local fill-forward-paragraph-function #'go--fill-forward-paragraph)
              (setq-local adaptive-fill-function #'go--find-fill-prefix)
              (setq-local adaptive-fill-first-line-regexp "")
              (setq-local comment-line-break-function #'go--comment-indent-new-line)
              ))
  :bind (:map go-ts-mode-map
              ("<leader> l l" . golang-clean-buffer)
              ("<leader> l c" . compile)
              ;; ("C-c k" . compile-again)
              ;; ("<M-down>" . (lambda () (interactive) (beginning-of-defun -1)))
              ;; ("<M-up>" . beginning-of-defun)
              ("M-n" . (lambda () (interactive)
                         (treesit-search-forward-goto
                          (treesit-node-at (point))
                          "^\\(import\\|function\\|method\\)_declaration$" 'start)))
              ("M-p" . (lambda () (interactive)
                         (treesit-search-forward-goto
                          (treesit-node-at (point))
                          "^\\(import\\|function\\|method\\)_declaration$" 'start 'backward)))))

(defun linebreak-struct ()
  (interactive)
  (save-excursion
    (let* ((p (treesit-search-forward (treesit-node-at (point)) "^expression_list$"))
           (children (treesit--children-covering-range-recurse p (treesit-node-start p) (treesit-node-end p) 0))
           ;; Nodes what need adjusting.
           (nodes (seq-filter (lambda (c) (member (treesit-node-type c) '("," "{" "}"))) children))
           ;; Positions to adjust.
           (positions (mapcar 'treesit-node-end nodes)))
      ;; Insert characters back to front so that pending positions
      ;; are not invalidated.
      (progn
        (message "node %s" p)
        (message "childres %s" children)
        (message "nodes %s" nodes)
        (dolist (p (sort positions '>))
          (goto-char p)
          (pcase (char-before)
            ;; For } we want to achieve ",\n}".
            (?\} (progn (backward-char)
                        (insert-char ?\,)
                        (newline-and-indent)))
            ;; For , and { add a newline after.
            ((or ?\{ ?\,) (newline-and-indent))
            (x (message "unknown item %s" x))))))))

(defun lsp-go-tags ()
  "Set tags for LSP Go"
  (interactive)
  ;; FIXME: Loop through the variable and replace earlier -tags.
  (let ((tags (read-from-minibuffer "-tags=")))
    (setq lsp-go-build-flags (vector (s-join "" (list "-tags=" tags))))))


;; Rust
;; rustup component add rust-src
;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs
;; git clone https://github.com/rust-lang/rust-analyzer.git && cd rust-analyzer
;; cargo xtask install --server
(use-package rust-mode
  :onlyif (executable-find "rustc")
  :ensure t
  :bind (:map rust-mode-map
              ;; ("C-c k" . compile-again)
              ("M-n" . (lambda () (interactive) (beginning-of-defun -1)))
              ("M-p" . beginning-of-defun)))

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
  (setq python-shell-interpreter "python3"
        python-indent 4)
  ;;(setq python-shell-interpreter-args "-c \"%load_ext autoreload\" --simple-prompt -")
  (setq python-shell-interpreter-args "-i")
  (defun refresh ()
    (interactive)
    (save-some-buffers)
    (python-shell-send-string "%autoreload"))
  (add-hook 'inferior-python-mode-hook
            (lambda () (local-set-key (kbd "<f5>") 'refresh)))
  (local-set-key (kbd "<C-return>") 'python-shell-send-region)
  (local-set-key (kbd "<C-enter>") 'python-shell-send-region)
  (local-set-key (kbd "<f5>") 'refresh)
  ;; (local-set-key (kbd "<M-up>")
  ;;                (lambda () (interactive)
  ;;                  (python-nav-backward-defun)
  ;;                  (recenter 10)))
  ;; (local-set-key (kbd "<M-down>")
  ;;                (lambda () (interactive)
  ;;                  (python-nav-forward-defun)
  ;;                  (recenter 10)))
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
;;(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(use-package matlab
  :defer t
  :config
  ;(matlab-cedet-setup)
  (setq matlab-indent-function-body t)
  :bind (:map matlab-mode-map
              ("<C-return>" . matlab-shell-run-region-or-line)
              ("<C-enter>" . matlab-shell-run-region-or-line)
              ("C-c C-m" . matlab-shell)
              ;; ("<M-down>" . matlab-end-of-defun)
              ;; ("<M-up>" . matlab-beginning-of-defun)
              ("M-n" . matlab-end-of-defun)
              ("M-p" . matlab-beginning-of-defun)))

;; JSON
(setq js-indent-level 2)

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
