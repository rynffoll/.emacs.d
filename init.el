(setq debug-on-error t)
(setq debug-on-quit t)

(setq load-prefer-newer t)
(setq message-log-max t) ;; we don't want to lose any startup log info

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

(require 'package)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))
(setq package-archive-priorities
      '(("melpa-stable" . 5)
        ("gnu"          . 5)
        ("melpa"        . 10)))
(package-initialize)

(setq package-enable-at-startup nil
      package--initialized t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.01)

(use-package bind-key)

(use-package quelpa
  :defer t
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance")
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :custom
  (quelpa-use-package-inhibit-loading-quelpa t "Improve startup performance"))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (use-dialog-box nil)
  (enable-recursive-minibuffers t)
  (indent-tabs-mode nil "Don't use tabs")
  (create-lockfiles nil "Stop creating .# files")
  (frame-resize-pixelwise t)
  (window-resize-pixelwise t)
  (inhibit-compacting-font-caches t)
  (scroll-step 1)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 101)
  (ring-bell-function 'ignore)
  (delete-by-moving-to-trash t)
  :hook
  (focus-out-hook . garbage-collect)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (make-backup-files nil "Stop creating backup~ files")
  (auto-save-default nil "Stop creating #autosave# files")
  (enable-local-variables :all)
  (enable-local-eval t))

(use-package autorevert
  :custom
  (auto-revert-verbose nil)
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode))

(use-package saveplace
  :ensure nil
  :config
  (save-place-mode))

(use-package super-save
  :disabled
  :config
  (super-save-mode +1))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer . (lambda ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

(use-package winum
  :demand
  :custom
  ;; (winum-auto-setup-mode-line nil "For spaceline")
  (winum-scope 'frame-local)
  :config
  (winum-mode))

(use-package shackle
  :custom
  (shackle-default-alignment 'below)
  (shackle-default-size 0.4)
  (shackle-rules '((help-mode :align below :select t)
                   (helpful-mode :align below)
                   (flycheck-error-list-mode :align below :size 0.25)
                   (cider-repl-mode :align below :size 0.3)
                   (ansible-doc-module-mode :align below)
                   ("*Pack*" :align below :size 0.2)))
  :config
  (shackle-mode 1))

(use-package eyebrowse
  :defer 1
  :custom
  (eyebrowse-new-workspace t "Clean up and display the scratch buffer")
  (eyebrowse-wrap-around t)
  :config
  (eyebrowse-mode t))

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file null-device "Don't store customizations"))

(use-package helpful
  :defer t)

(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history t)
  ;; undo-in-region is known to cause undo history corruption, which can
  ;; be very destructive! Disabling it deters the error, but does not fix
  ;; it entirely!
  (undo-tree-enable-undo-in-region nil)
  (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "/.cache/undo-tree"))))
  :config
  (global-undo-tree-mode t))

(use-package paradox
  :defer 5
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t "Don't ask github token")
  :config
  (paradox-enable))

(use-package calendar
  :ensure nil
  :custom
  (calendar-date-style 'iso)
  (calendar-week-start-day 1))

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh")
  (tramp-default-proxies-alist nil))

(use-package epa
  :ensure nil
  :defer t
  :custom
  (epa-pinentry-mode 'loopback))

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-aBhl --group-directories-first")
  (dired-auto-revert-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'always "Never prompt for recursive copies of a directory")
  (dired-recursive-deletes 'always "Never prompt for recursive deletes of a directory")
  (dired-hide-details-hide-symlink-targets nil)
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :custom
  (dired-bind-jump nil))

(use-package async
  :after dired
  :config
  (dired-async-mode t))

(use-package dired-hide-dotfiles
  :after dired
  :bind
  (:map dired-mode-map
  ("M-." . dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :defer t
  :bind
  (:map dired-mode-map
            ("TAB" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package dired-sidebar
  :defer t
  :bind
  ("C-x M-t" . dired-sidebar-toggle-sidebar)
  :custom
  ;; (dired-sidebar-theme 'none)
  (dired-sidebar-no-delete-other-windows t)
  (dired-sidebar-toggle-hidden-commands '(balance-windows
                                          delete-window
                                          delete-other-windows)))

(use-package pack
  :defer t
  :bind
  (:map dired-mode-map
        ("P" . pack-dired-dwim))
  :custom
  (pack-dired-default-extension ".zip"))

(use-package eshell
  :ensure nil
  :defer t)

(use-package em-smart
  :ensure nil
  :after eshell
  :config (eshell-smart-initialize))

(use-package esh-autosuggest
  :after eshell
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-fringe-status
  :after eshell
  :hook (eshell-mode . eshell-fringe-status-mode))

(use-package eshell-prompt-extras
  :after eshell
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :hook
  (ediff-prepare-buffer . show-all)
  (ediff-quit . winner-undo))

(use-package ivy
  :custom
  (ivy-wrap t)
  (ivy-fixed-height-minibuffer t)
  (ivy-initial-inputs-alist nil "Don't use ^ as initial input")
  (ivy-format-function 'ivy-format-function-line "highlight til EOL")
  (ivy-use-virtual-buffers nil "don't show recent files in switch-buffer")
  (ivy-virtual-abbreviate 'full)
  (ivy-on-del-error-function nil)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist '((counsel-ag . ivy--regex-plus)
                           (counsel-grep . ivy--regex-plus)
                           (swiper . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  :config
  (ivy-mode t))

(use-package counsel
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ([remap menu-bar-open]            . counsel-tmm)
   ([remap insert-char]              . counsel-unicode-char)
   ([remap isearch-forward]          . counsel-grep-or-swiper)
   ([remap describe-function]        . counsel-describe-function)
   ([remap describe-variable]        . counsel-describe-variable)
   ([remap apropos-command]          . counsel-apropos)
   :map mode-specific-map
   :prefix-map counsel-prefix-map
   :prefix "c"
   ("a" . counsel-apropos)
   ("b" . counsel-bookmark)
   ("B" . counsel-bookmarked-directory)
   ("c w" . counsel-colors-web)
   ("c e" . counsel-colors-emacs)
   ("d" . counsel-dired-jump)
   ("f" . counsel-file-jump)
   ("F" . counsel-faces)
   ("g" . counsel-org-goto)
   ("h" . counsel-command-history)
   ("H" . counsel-minibuffer-history)
   ("i" . counsel-imenu)
   ("j" . counsel-find-symbol)
   ("l" . counsel-locate)
   ("L" . counsel-find-library)
   ("m" . counsel-mark-ring)
   ("o" . counsel-outline)
   ("O" . counsel-find-file-extern)
   ("p" . counsel-package)
   ("r" . counsel-recentf)
   ("s g" . counsel-grep)
   ("s r" . counsel-rg)
   ("s s" . counsel-ag)
   ("t" . counsel-org-tag)
   ("v" . counsel-set-variable)
   ("w" . counsel-wmctrl)
   :map help-map
   ("F" . counsel-describe-face))
  :custom
  (counsel-describe-function-function 'helpful-callable)
  (counsel-describe-variable-function 'helpful-variable))

(use-package swiper)

(use-package smex)

(use-package hydra)

(use-package ivy-hydra
  :after ivy hydra)

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package counsel-projectile
  :after counsel projectile
  :bind
  ([remap projectile-find-file]        . counsel-projectile-find-file)
  ([remap projectile-find-dir]         . counsel-projectile-find-dir)
  ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
  ([remap projectile-grep]             . counsel-projectile-grep)
  ([remap projectile-ag]               . counsel-projectile-ag)
  ([remap projectile-ripgrep]          . counsel-projectile-rg)
  ([remap projectile-switch-project]   . counsel-projectile-switch-project))

(use-package ns-win
  :if (memq window-system '(mac ns))
  :ensure nil
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier 'super))

(use-package files
  :ensure nil
  :if (memq window-system '(mac ns))
  :custom
  (insert-directory-program "gls"))

(use-package browse-url
  :ensure nil
  :config
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic))))

(use-package restart-emacs
  :defer t)

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package iqa
  :custom
  (iqa-user-init-file (concat user-emacs-directory "config.org"))
  :config
  (iqa-setup-default))

(use-package shell-pop
  :defer t
  :bind ("M-`" . shell-pop)
  :custom
  (shell-pop-full-span t "Spans full width of a window")
  (shell-pop-shell-type '("eshell" "*eshell-pop*" (lambda () (eshell)))))

(use-package exec-path-from-shell
  :defer 0.1
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :defer t
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-uppercase-first nil)
  :config
  (which-key-mode +1))

(use-package ssh-config-mode
  :init
  (autoload 'ssh-config-mode "ssh-config-mode" t))

(use-package frame
  :ensure nil
  :bind
  ("C-z" . nil)
  :custom
  (default-frame-alist '((left . 0.5) (top . 0.5)
                         (width . 0.7) (height . 0.9)))
  :config
  (blink-cursor-mode -1))

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

(use-package fringe
  :ensure nil
  :init
  (setf (cdr (assq 'continuation fringe-indicator-alist))
        ;; '(nil nil) ;; no continuation indicators
        '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        ))

(use-package font-lock+
  :ensure nil
  :quelpa
  (font-lock+ :repo "emacsmirror/font-lock-plus" :fetcher github))

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default nil :font "Fira Mono 14"))

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-dired
  :defer t)

(use-package faces
  :ensure nil
  :custom-face
  (mode-line ((t :inherit mode-line :box nil :underline nil :overline nil)))
  (mode-line-inactive ((t (:inherit mode-line-inactive :box nil :underline nil :overline nil)))))

(use-package hide-mode-line
  :defer t
  :hook (dired-sidebar-mode . hide-mode-line-mode))

(use-package minions
  :config
  (minions-mode))

(use-package doom-modeline
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-minor-modes t)
  :hook
  (after-init . doom-modeline-mode))

(use-package solarized-theme
  ;; :disabled
  :custom
  (solarized-distinct-doc-face t "Emphasize docstrings")
  (solarized-use-variable-pitch nil "Don't change the font for some headings and titles")
  (solarized-emphasize-indicators nil "Use less colors for indicators such as git:gutter, flycheck and similar")
  (solarized-scale-org-headlines nil "Don't change size of org-mode headlines (but keep other size-changes)")
  ;; Avoid all font-size changes
  ;; (solarized-height-minus-1 1.0)
  ;; (solarized-height-plus-1 1.0)
  ;; (solarized-height-plus-2 1.0)
  ;; (solarized-height-plus-3 1.0)
  ;; (solarized-height-plus-4 1.0)
  :config
  (load-theme 'solarized-dark t))

(use-package spacemacs-common
  :disabled
  :ensure spacemacs-theme
  :custom
  (spacemacs-theme-org-agenda-height nil)
  (spacemacs-theme-org-bold t)
  (spacemacs-theme-org-height nil)
  (spacemacs-theme-org-highlight t)
  :config
  (load-theme 'spacemacs-light t))

(use-package doom-themes
  :disabled
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package delsel
  :ensure nil
  :bind
  (:map mode-specific-map
        ("C-g" . minibuffer-keyboard-quit))
  :config
  (delete-selection-mode 1))

(use-package simple
  :ensure nil
  :custom
  (backward-delete-char-untabify-method 'hungry)
  :config
  (column-number-mode 1))

(use-package rainbow-mode
  :defer t
  :hook css-mode)

(use-package paren
  :ensure nil
  :config
  (show-paren-mode t))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode conf-mode) . rainbow-delimiters-mode))

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
  (smartparens-global-mode t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package hl-todo
  :defer t
  :hook ((prog-mode conf-mode) . hl-todo-mode))

(use-package highlight-indent-guides
  :defer t)

(use-package highlight-numbers
  :defer t
  :hook ((prog-mode conf-mode) . highlight-numbers-mode))

(use-package editorconfig
  :defer t
  :hook ((prog-mode conf-mode) . editorconfig-mode))

(use-package display-line-numbers
  :ensure nil
  :defer t
  :custom
  (display-line-numbers-width-start 1))

(use-package company
  :defer t
  :custom
  (company-minimum-prefix-length 2)
  (company-require-match 'never)
  (company-selection-wrap-around t)
  (company-tooltip-minimum-width 30)
  (company-tooltip-margin 2)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-dabbrev-code-other-buffers t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode))

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

(use-package anzu
  :custom
  (anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1))

(use-package hideshow
  :ensure nil
  :defer t
  :hook (prog-mode . hs-minor-mode))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package company-yasnippet
  :ensure company
  :after company yasnippet
  :preface
  (defun my/add-snippets-to-company-backend (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  :custom
  (company-backends (mapcar #'my/add-snippets-to-company-backend company-backends)))

(use-package flycheck
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'right-fringe)
  :config
  (use-package fringe-helper)
  (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
    ".....X.."
    "....XX.."
    "...XXX.."
    "..XXXX.."
    "...XXX.."
    "....XX.."
    ".....X.."))

(use-package dumb-jump
  :defer t
  :preface
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  :custom
  (dumb-jump-selector 'ivy))

(use-package lisp
  :disabled
  :ensure nil
  :hook
  (after-save . check-parens))

(use-package highlight-defined
  :defer t
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :defer t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package erefactor
  :defer t)

(use-package eros
  :defer t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package clojure-mode
  :defer t)

(use-package clojure-mode-extra-font-locking
  :defer t)

(use-package clojure-snippets
  :defer t)

(use-package cider
  :defer t
  :custom
  (cider-repl-use-pretty-printing t)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-history-display-style 'one-line)
  (cider-repl-history-highlight-current-entry t)
  (cider-repl-history-highlight-inserted-item t))

(use-package clj-refactor
  :after clojure-mode
  :defer t
  :hook
  (clojure-mode . clj-refactor-mode))

(use-package eldoc
  :ensure nil
  :hook
  ((clojure-mode cider-repl-mode) . eldoc-mode))

(use-package projectile
  :bind
  (:map mode-specific-map ("p" . projectile-command-map))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

(use-package magit
  :bind
  ("C-x g" . magit)
  :custom
  (magit-completing-read-function 'ivy-completing-read "Force Ivy usage.")
  (magit-clone-default-directory "~/Projects")
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-repository-directories `((,user-emacs-directory . 0)
                                  (,magit-clone-default-directory . 1))))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(use-package forge
  :after magit)

(use-package git-timemachine
  :defer t)

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitignore-templates
  :defer t)

(use-package diff-hl
  :defer t
  :custom
  (diff-hl-draw-borders nil)
  :hook
  ((prog-mode conf-mode org-mode) . diff-hl-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_: next    _b_: base            _<_: upper/base        _C_: combine
_p_: prev    _u_: upper           _=_: upper/lower       _r_: resolve
             _l_: lower           _>_: base/lower        _k_: kill current
             _a_: all             _R_: refine
^^           _RET_: current       _E_: ediff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package org
  :ensure org-plus-contrib
  :defer t
  :preface
  (defun my/open-org-directory ()
    (interactive)
    (find-file org-directory))
  (defun my/open-org-inbox-file ()
    (interactive)
    (find-file my/org-inbox-file))
  (defun my/open-org-todo-file ()
    (interactive)
    (find-file my/org-todo-file))
  (defun my/open-org-notes-file ()
    (interactive)
    (find-file my/org-notes-file))
  :custom-face
  (org-tag ((t (:inherit shadow))))
  :custom
  (org-modules '(org-expiry))
  (org-insert-heading-respect-content t "Insert new headings after current subtree rather than inside it")

  (org-startup-indented t)
  (org-tags-column 0)
  (org-ellipsis "  ")
  (org-pretty-entities t)
  (org-use-sub-superscripts '{} "Require {} for sub/super scripts")

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-edit-src-content-indentation 0)

  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)

  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!/@)" "CANCELLED(c@/!)")))
  (org-log-into-drawer t)
  (org-expiry-inactive-timestamps t)

  (org-directory "~/Org")
  (my/org-inbox-file (concat org-directory "/inbox.org"))
  (my/org-todo-file (concat org-directory "/todo.org"))
  (my/org-notes-file (concat org-directory "/notes.org"))
  (org-agenda-files `(,my/org-inbox-file ,my/org-todo-file))
  (org-archive-location (concat org-directory "/old/archive.org" "::* From %s")))

(use-package org-bullets
  :after org
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
  :defer t
  :bind
  (:map mode-specific-map ("d" . docker)))

(use-package docker-tramp
  :defer t)

(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package yaml-mode
  :defer t
  :mode "Procfile\\'")

(use-package ansible-mode
  :ensure nil
  :quelpa (ansible-mode :fetcher github :repo "rynffoll/ansible-mode")
  :defer t
    :custom
  (ansible-mode-enable-auto-decrypt-encrypt t)
  :hook
  (yaml-mode . ansible-mode-maybe-enable))

(use-package ansible-doc
  :after ansible-mode
  :hook
  (ansible-mode . ansible-doc-mode))

(use-package jinja2-mode
  :defer t
  :mode "\\.j2\\'")

(use-package company-ansible
  :after company ansible-mode
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package restclient
  :defer t
  :mode
  ("\\.http\\'" . restclient-mode))

(use-package ob-restclient
  :after org restclient
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package password-generator
  :defer t)

(use-package google-translate
  :defer t
  :custom
  (google-translate-default-target-language "ru")
  (google-translate-default-source-language "en")
  (google-translate-output-destination nil)
  (google-translate-pop-up-buffer-set-focus t))

(use-package olivetti
  :defer t
  :custom (olivetti-body-width 100))

(use-package crux
  :defer t)

(use-package link-hint
  :defer t)

(setq debug-on-error nil)
(setq debug-on-quit nil)
