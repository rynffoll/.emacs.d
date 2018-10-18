(setq debug-on-error t)
(setq debug-on-quit t)

(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq debug-on-error nil)
            (setq debug-on-quit nil)))

(require 'package)
(setq package-archives
      `(,@package-archives
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(setq package-enable-at-startup nil
      package--initialized t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)
(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

(use-package quelpa
  :defer t
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance")
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package paradox
  :defer 1
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token") 
  :config
  (paradox-enable))

(use-package general
  :init
  (defconst my-leader "SPC")
  (defconst my-local-leader "SPC m")
  (defconst my-non-normal-leader "M-m")
  (defconst my-non-normal-local-leader "M-m m")
  :config
  (general-define-key
   :states '(normal visual insert emacs motion)
   :keymaps 'override
   :prefix my-leader
   :non-normal-prefix my-non-normal-leader
   "" '(nil :which-key "Leader")
   "." 'counsel-find-file

   "o" '(:ignore t :which-key "Open")
   "od" 'docker
   "ol" 'link-hint-open-link
   "oL" 'counsel-find-library
   "op" 'package-list-packages

   "b" '(:ignore t :which-key "Buffers")
   "bB" 'ibuffer
   "bN" 'evil-buffer-new
   "bb" 'ivy-switch-buffer
   "bk" 'kill-this-buffer
   "b]" 'evil-next-buffer
   "b[" 'evil-prev-buffer
   "bR" 'crux-rename-buffer-and-file
   "bD" 'crux-delete-buffer-and-file
   "bp" 'counsel-projectile

   "f" '(:ignore t :which-key "Files")
   "fd" 'counsel-dired-jump
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "fR" 'crux-rename-file-and-buffer
   "fD" 'crux-delete-file-and-buffer
   "fp" 'projectile-find-file
   "ft" 'treemacs

   "e" '(:ignore t :which-key "Emacs")
   "ed" 'iqa-find-user-init-directory
   "ee" 'iqa-find-user-init-file
   "er" 'iqa-reload-user-init-file

   "g" '(:ignore t :which-key "Git")
   "gg" 'magit-status
   "gt" 'git-timemachine
   "gl" 'magit-list-repositories

   "p" '(:ignore t :which-key "Projectile")
   "pb" 'counsel-projectile
   "pp" 'projectile-switch-project
   "pf" 'projectile-find-file
   "pt" 'treemacs-projectile

   "/" '(:ignore t :which-key "Search")
   "//" 'swiper
   "/i" 'imenu
   "/p" 'counsel-projectile-rg

   "h" '(:ignore t :which-key "Help")
   "h." 'helpful-at-point
   "hC" 'helpful-command
   "hF" 'counsel-describe-face
   "hT" 'google-translate-at-point-reverse
   "hc" 'helpful-callable
   "hf" 'helpful-function
   "hk" 'helpful-key
   "hm" 'helpful-macro
   "ht" 'google-translate-at-point
   "hv" 'helpful-variable

   "t" '(:ignore t :which-key "Toggle")
   "to" 'olivetti-mode
   "tT" 'counsel-load-theme
   "tr" 'rainbow-mode

   "q" '(:ignore t :which-key "Quit")
   "qq" 'kill-emacs
   "qr" 'restart-emacs)
  (general-define-key
   :states '(normal visual insert emacs motion)
   :keymaps 'override
   :prefix my-local-leader
   :non-normal-prefix my-non-normal-local-leader
   "" '(nil :which-key "Local")))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-emacs-state-cursor '(box (face-foreground 'warning)))
  (evil-mode-line-format nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil)
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-magit
  :after magit evil
  :custom
  (evil-magit-want-horizontal-movement t)
  (evil-magit-use-z-for-folds t))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-org
  :after org evil
  :hook
  (org-mode . evil-org-mode)
  :custom
  (evil-org-special-o/O '(item table-row))
  (evil-org-key-theme '(todo textobjects insert navigation heading)))

(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (initial-major-mode 'text-mode)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil)
  (create-lockfiles nil)
  (debug-on-quit nil)
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  (scroll-step 1)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (ring-bell-function 'ignore)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (make-backup-files nil)
  (enable-local-variables :all)
  (enable-local-eval t))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode 1))

(use-package ns-win
  :ensure nil
  :custom
  (mac-command-modifier 'meta))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package simple
  :ensure nil
  :custom
  (backward-delete-char-untabify-method 'untabify)
  :config
  (column-number-mode 1))

(use-package cus-edit
  :ensure nil
  :custom
  ;; alternatively, one can use `(make-temp-file "emacs-custom")'
  (custom-file null-device "Don't store customizations"))

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1))

(use-package ibuffer
  :ensure nil
  :general
  ([remap list-buffers] 'ibuffer))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

(use-package tramp
  :defer t
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package helpful
  :defer t
  :commands
  helpful-at-point
  helpful-command
  helpful-callable
  helpful-function
  helpful-key
  helpful-macro
  helpful-variable)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode +1))

(use-package restart-emacs
  :defer t
  :commands restart-emacs)

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer")
  (with-eval-after-load 'evil
    ;; cyrillic tweaks
    (define-key evil-normal-state-map (kbd "C-х") #'evil-force-normal-state)
    (define-key evil-insert-state-map (kbd "C-х") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "C-х") #'evil-exit-visual-state)))

(use-package iqa
  :defer t
  :commands
  iqa-find-user-init-directory
  iqa-find-user-init-file
  iqa-reload-user-init-file
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org")))

(use-package exec-path-from-shell
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))

(use-package undo-tree
  :defer t
  :custom
  (undo-tree-auto-save-history t)
  ;; undo-in-region is known to cause undo history corruption, which can
  ;; be very destructive! Disabling it deters the error, but does not fix
  ;; it entirely!
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "/.cache/undo-tree"))))
  :config
  (global-undo-tree-mode t))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

(use-package epa
  :ensure nil
  :defer t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-hide-details-hide-symlink-targets nil)
  :hook
  (dired-mode . dired-hide-details-mode))

;; (use-package dired-hide-dotfiles
;;   :general
;;   (:keymaps 'dired-mode-map :states '(normal emacs)
;; 	    "." 'dired-hide-dotfiles-mode)
;;   :hook
;;   (dired-mode . dired-hide-dotfiles-mode))

(use-package async
  :after dired
  :config
  (dired-async-mode t))

(use-package eshell
  :ensure nil)

(use-package em-smart
  :ensure nil
  :config
  (eshell-smart-initialize))

(use-package esh-autosuggest
  :hook
  (eshell-mode . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :hook
  (eshell-mode . eshell-fringe-status-mode))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t (:inherit mode-line-inactive :box nil :underline nil :overline nil))))
  (org-tag ((t (:inherit shadow))))
  :config
  (set-face-attribute 'default nil :font "Fira Mono 14"))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package tooltip
  :ensure nil
  :config
  (tooltip-mode -1))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package frame
  :ensure nil
  :config
  (blink-cursor-mode -1)
  (when window-system
    (setq frame-parameters '((left . 0.5) (top . 0.5)
			     (width . 0.7) (height . 0.9)))
    (dolist (fp frame-parameters)
      (add-to-list 'default-frame-alist fp))))

(use-package fringe
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
	;; '(nil nil) ;; no continuation indicators
	'(nil right-curly-arrow) ;; right indicator only
	;; '(left-curly-arrow nil) ;; left indicator only
	;; '(left-curly-arrow right-curly-arrow) ;; default
	))

(use-package feebleline
  :disabled
  :custom
  (feebleline-show-git-branch t)
  :config
  (feebleline-mode 1))

(use-package minions
  :disabled
  :custom
  (minions-mode-line-lighter "[+]")
  :config
  (minions-mode))

(use-package moody
  :disabled
  :custom
  (x-underline-at-descent-line t)
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package powerline
  :defer t
  :custom
  (powerline-default-separator nil))

(use-package spaceline
  :defer t
  :custom
  (spaceline-highlight-face-func 'spaceline-highlight-face-evil-state))

(use-package spaceline-segments
  :ensure nil
  :defer t
  :custom
  (spaceline-minor-modes-p nil)
  (spaceline-hud-p nil)
  (spaceline-purpose-p nil)
  (spaceline-buffer-position-p nil)
  (spaceline-buffer-modified-p nil)
  (spaceline-buffer-encoding-abbrev-p nil)
  (spaceline-buffer-size-p nil)
  (spaceline-input-method-p t)
  (spaceline-org-clock-p t)
  (spaceline-org-pomodoro-p t))

(use-package spaceline-config
  :ensure nil
  :preface
  (defun spaceline-custom-theme (&rest additional-segments)
    "My custom spaceline theme."
    (apply 'spaceline--theme
           '((((persp-name
                workspace-number
                window-number) :separator "|"))
             :fallback evil-state
             :face highlight-face
             :priority 100)
           '((buffer-modified buffer-size buffer-id remote-host)
             :priority 98)
           additional-segments))
  :config
  (spaceline-custom-theme))

(use-package hide-mode-line
  :hook
  (treemacs-mode . hide-mode-line-mode))

(use-package solarized-theme
  :custom
  (solarized-use-variable-pitch nil)
  (solarized-scale-outline-headlines nil)
  (solarized-scale-org-headlines nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-light t))

(use-package solaire-mode
  :disabled t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup-hook . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))

(use-package ivy
  :defer 2
  :general
  ([remap switch-to-buffer] 'ivy-switch-buffer)
  (:keymaps 'ivy-mode-map
	    "C-j" 'ivy-next-line
	    "C-k" 'ivy-previous-line)
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil "Don't use ^ as initial input")
  (ivy-format-function 'ivy-format-function-line "highlight til EOL")
  (ivy-use-virtual-buffers nil "don't show recent files in switch-buffer")
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode +1))

(use-package hydra
  :defer 2)

(use-package ivy-hydra
  :after ivy hydra
  :defer 2)

(use-package ivy-rich
  :after ivy
  :defer 2
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :defer 2
  :commands counsel-describe-face
  :general
  ([remap apropos]                  'counsel-apropos)
  ([remap bookmark-jump]            'counsel-bookmark)
  ([remap describe-face]            'counsel-describe-face)
  ([remap describe-function]        'counsel-describe-function)
  ([remap describe-variable]        'counsel-describe-variable)
  ([remap execute-extended-command] 'counsel-M-x)
  ([remap find-file]                'counsel-find-file)
  ([remap find-library]             'counsel-find-library)

  ([remap info-lookup-symbol]       'counsel-info-lookup-symbol)
  ([remap imenu]                    'counsel-imenu)
  ([remap recentf-open-files]       'counsel-recentf)
  ([remap org-capture]              'counsel-org-capture)
  ([remap swiper]                   'counsel-grep-or-swiper)
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable))

(use-package flx
  :defer 2
  :custom
  (ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
			   (counsel-grep . ivy--regex-plus)
			   (swiper . ivy--regex-plus)
			   (t . ivy--regex-fuzzy))))

(use-package counsel-projectile
  :defer 2
  :after projectile
  :general
  ([remap projectile-find-file]        'counsel-projectile-find-file)
  ([remap projectile-find-dir]         'counsel-projectile-find-dir)
  ([remap projectile-switch-to-buffer] 'counsel-projectile-switch-to-buffer)
  ([remap projectile-grep]             'counsel-projectile-grep)
  ([remap projectile-ag]               'counsel-projectile-ag)
  ([remap projectile-switch-project]   'counsel-projectile-switch-project))

(use-package highlight-numbers
  :defer t
  :hook
  ((prog-mode conf-mode) . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :defer t
  :hook
  ((prog-mode conf-mode) . highlight-escape-sequences-mode))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook css-mode-hook)

(use-package smartparens
  :defer t
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-from-inside t)
  (sp-cancel-autoskip-on-backward-movement nil)
  :config
  (use-package smartparens-config :ensure nil)

  ;; smartparens breaks evil-mode's replace state
  (with-eval-after-load 'evil
    (add-hook 'evil-replace-state-entry-hook #'turn-off-smartparens-mode)
    (add-hook 'evil-replace-state-exit-hook  #'turn-on-smartparens-mode))

  (smartparens-global-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package company
  :hook
  (after-init . global-company-mode)
  :general
  ("C-@" 'company-complete)
  :custom
  (company-minimum-prefix-length 1)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-minimum-width 30)
  (company-tooltip-margin 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil))

(use-package company-shell
  :after company
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package company-flx
  :after company
  :config
  (company-flx-mode +1))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package treemacs
  :defer t
  :commands treemacs
  :custom
  (treemacs-collapse-dirs (if (executable-find "python") 3 0))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-git-mode 'deferred))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile
  :commands treemacs-projectile)

(use-package flycheck
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  (use-package fringe-helper)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    "........"
    "..XX..XX"
    ".XX..XX."
    "XX..XX.."
    ".XX..XX."
    "..XX..XX"
    "........"))

(use-package dumb-jump
  :defer t
  :custom
  (dumb-jump-selector 'ivy))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking)

(use-package clojure-snippets
  :defer t)

(use-package cider
  :defer t)

(use-package clj-refactor
  :after clojure-mode
  :defer t
  :hook
  (clojure-mode . clj-refactor-mode))

(use-package projectile
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode t))

(use-package magit
  :defer t
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  ("~/Projects" . 1))))

(use-package magit-todos
  :after magit
  :defer t
  :commands magit-status
  :config
  (magit-todos-mode))

(use-package git-timemachine
  :defer t
  :commands git-timemachine)

(use-package git-gutter-fringe
  :disabled
  :config
  (use-package fringe-helper)
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX.....")
  (global-git-gutter-mode t))

(use-package gitignore-mode
  :mode ("^.gitignore$" . gitignore-mode))

(use-package diff-hl
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  ((prog-mode conf-mode org-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))

(use-package org
  :ensure org-plus-contrib
  :defer 1
  :custom
  (org-startup-indented t)
  (org-tags-column 0)
  ;; (org-ellipsis "…")
  ;; (org-ellipsis " ▼ ")
  (org-ellipsis "  ")
  (org-pretty-entities t)

  (org-use-speed-commands t)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-directory "~/Dropbox/Org")
  (org-agenda-files `(,(concat org-directory "/todo.org")))
  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s")))

(use-package org-bullets
  :after org
  :defer 1
  :custom
  ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
  ;; ► • ★ ▸
  (org-bullets-bullet-list '("◆"))
  :hook
  (org-mode . org-bullets-mode))

(use-package toc-org
  :after org
  :hook
  (org-mode . toc-org-enable))

(use-package docker
  :commands docker
  :defer t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'docker-container-mode 'emacs)
    (evil-set-initial-state 'docker-image-mode 'emacs)
    (evil-set-initial-state 'docker-network-mode 'emacs)
    (evil-set-initial-state 'docker-volume-mode 'emacs)
    (evil-set-initial-state 'docker-machine-mode 'emacs)))

(use-package docker-tramp)

(use-package dockerfile-mode)

(use-package yaml-mode
  :mode "Procfile\\'"
  :hook (yaml-mode . ansible))

(use-package ansible
  :commands ansible::auto-decrypt-encrypt
  :init
  (put 'ansible::vault-password-file 'safe-local-variable #'stringp)
  :hook
  (ansible . ansible::auto-decrypt-encrypt)
  :general
  (:keymaps 'ansible::key-map :states '(normal visual insert emacs motion) :prefix my-local-leader
            "d" 'ansible::decrypt-buffer
            "e" 'ansible::encrypt-buffer
            "h" 'ansible-doc)
  :custom-face
  (ansible::section-face ((t (:inherit 'font-lock-variable-name-face))))
  (ansible::task-label-face ((t (:inherit 'font-lock-doc-face)))))

(use-package ansible-doc
  :hook
  (ansible . ansible-doc-mode)
  :config
  (evil-set-initial-state 'ansible-doc-module-mode 'emacs))

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package company-ansible
  :after company
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package restclient
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :after org restclient
  :defer t
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :after company restclient
  :defer t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package password-generator
  :defer t)

(use-package google-translate
  :defer t
  :commands google-translate-at-point google-translate-at-point-reverse
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-output-destination nil)
  (google-translate-pop-up-buffer-set-focus t))

(use-package olivetti
  :defer t
  :commands olivetti-mode
  :custom (olivetti-body-width 100))

(use-package crux
  :defer t)

(use-package link-hint
  :commands link-hint-open-link)

(use-package browse-url
  :ensure nil
  :config
  (when (file-directory-p "/mnt/c/Windows/System32/cmd.exe")
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic)))))

;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (org-babel-tangle)) nil t)
;; End:
