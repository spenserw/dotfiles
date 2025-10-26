;;; .emacs --- Emacs Configuration
;;; Commentary:
;; Robust Emacs configuration with proper error handling

;;; Code:

;; ====================
;; Package Management
;; ====================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Auto-install missing packages

;; Add custom elisp directory
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Start server for emacsclient
(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; ====================
;; Basic Settings
;; ====================

;; Appearance
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(setq redisplay-dont-pause t)

;; Helper function to switch themes cleanly
(defun switch-theme (theme)
  "Disable all active themes and load THEME."
  (interactive
   (list (intern (completing-read "Load custom theme: "
                                   (mapcar 'symbol-name
                                           (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; Theme
(use-package oceanic-theme
  :config
  (load-theme 'oceanic t))

(use-package twilight-bright-theme
  :defer t)

;; Show full file paths in title bar
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Auto revert files when changed on disk
(global-auto-revert-mode 1)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; ====================
;; Backup Settings
;; ====================

(let ((backup-dir (expand-file-name "~/.saves"))
      (auto-saves-dir (expand-file-name "~/.saves")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups
      kept-new-versions 5    ; Keep some new versions
      kept-old-versions 2)   ; And some old ones, too

;; ====================
;; Window Management
;; ====================

;; Open vertically by default
(setq split-width-threshold 0
      split-height-threshold nil)

;; ====================
;; Evil Mode (Vim Emulation)
;; ====================

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Vim-like keybindings
(global-set-key (kbd "C-f") 'evil-force-normal-state)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)

;; ====================
;; Development Settings
;; ====================

;; Indentation
(use-package smart-tabs-mode
  :config
  (smart-tabs-insinuate 'c 'c++))

(setq-default c-basic-offset 4
              indent-tabs-mode nil
              js-indent-level 2
              typescript-indent-level 2
              css-indent-offset 2)

;; Environment variables (for SSH agent, etc.)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))

;; ====================
;; TODO Highlighting
;; ====================

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("FIX"    . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("DONE"   . "#A020F0")
          ("DEV"    . "#16E01A")
          ("TOP"    . "#16E01A")
          ("GOTCHA" . "#FF4500")
          ("LOW"    . "#FF4500")
          ("HIGH"   . "#1E90FF")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode 1))

;; ====================
;; Projectile
;; ====================

(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (setq projectile-project-search-path '("~/code/"))
  (projectile-mode 1))

(use-package helm-projectile
  :after (helm projectile))

;; ====================
;; Helm
;; ====================

(use-package helm
  :config
  (helm-mode 1)
  (global-set-key (kbd "C-x b") 'helm-multi-files))

;; ====================
;; Tree-sitter
;; ====================

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;; ====================
;; Quelpa (for GitHub packages)
;; ====================

(use-package quelpa
  :config
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :after quelpa)

;; TSI - Tree-sitter indentation
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; ====================
;; Language Modes
;; ====================

;; TypeScript
(use-package typescript-mode
  :after tree-sitter
  :mode ("\\.tsx?\\'" . typescriptreact-mode)
  :config
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; JSON
(use-package json-mode
  :mode "\\.json\\'")

;; YAML
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; GLSL
(use-package glsl-mode
  :mode "\\.glsl\\'")

;; ANSI color in compilation buffers
(use-package ansi-color
  :ensure nil
  :config
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

;; ====================
;; LSP Mode
;; ====================

;; Performance tuning
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 4))

(use-package flycheck
  :config
  (setq flycheck-checker-error-threshold 1000))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :hook (prog-mode . company-mode))

(use-package lsp-mode
  :commands lsp
  :hook ((web-mode . lsp)
         (html-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp)
         (ruby-mode . lsp)
         (python-mode . lsp)
         (css-mode . lsp))
  :custom
  (lsp-diagnostics-provider :flycheck)
  (lsp-disabled-clients '(angular-ls))  ; Hook will disable ESLint outside bb_web
  (lsp-typescript-suggest-complete-function-calls nil)
  (lsp-typescript-preferences-quote-style "single")
  (lsp-clients-typescript-max-ts-server-memory 2048)
  :config
  ;; Custom root finder for TypeScript/JS in BigBiller3 project
  (defun my-lsp-bigbiller-root ()
    "Return bb_web as root for JS/TS files in BigBiller3 project."
    (when-let* ((file (buffer-file-name))
                (project-root (locate-dominating-file file ".git"))
                (bb-web-dir (expand-file-name "bb_web" project-root)))
      (when (and (file-directory-p bb-web-dir)
                 (string-match-p "BigBiller3" project-root)
                 (or (derived-mode-p 'typescript-mode)
                     (derived-mode-p 'js-mode)))
        bb-web-dir)))

  ;; Add custom root finder to LSP (applies to both ts-ls and eslint)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (add-to-list 'lsp-root-uri-functions #'my-lsp-bigbiller-root)))
  ;; Ignore unnecessary JavaScript/TypeScript directories
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dist\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]coverage\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.vendor\\'")

  ;; Ruby/Solargraph: dynamically find gem directory
  (let ((gem-dir (expand-file-name "~/.local/share/gem/ruby")))
    (when (file-directory-p gem-dir)
      (add-to-list 'lsp-solargraph-library-directories gem-dir))))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list)

;; ====================
;; Git Integration
;; ====================

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

;; ====================
;; Optional: Version Managers
;; ====================

;; NVM - Only load if available
(use-package nvm
  :ensure nil
  :if (file-directory-p "~/.nvm")
  :load-path "~/.emacs.d/elisp"
  :config
  (ignore-errors
    (nvm-use "16")))

;; RVM - Only load if available
(use-package rvm
  :ensure nil
  :if (file-directory-p "~/.rvm")
  :load-path "~/.emacs.d/elisp"
  :config
  (ignore-errors
    (rvm-use-default)))

;; ====================
;; Writing Mode
;; ====================

(use-package writeroom-mode
  :commands writeroom-mode)

(use-package olivetti
  :commands olivetti-mode)

(defun writing-mode ()
  "Toggle a distraction-free writing mode."
  (interactive)
  (setq-default word-wrap t)
  (setq-default left-margin-width 25
                right-margin-width 23
                mode-line-format nil)
  (set-fringe-mode 0)
  (set-window-buffer nil (current-buffer))
  (set-background-color "#fbf8ef")
  (set-foreground-color "#1c1e1f")
  (variable-pitch-mode))

(defun toggle-margins ()
  "Toggle margins in current buffer."
  (interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
      (progn
        (setq left-margin-width 0)
        (setq right-margin-width 0)
        (set-window-buffer (selected-window) (current-buffer)))
    (setq left-margin-width 25)
    (setq right-margin-width 23)
    (set-window-buffer (selected-window) (current-buffer))))

;; ====================
;; Custom Keybindings
;; ====================

(global-set-key (kbd "M-p") 'query-replace)
(global-set-key (kbd "C-c r") 'lsp-rename)
(global-set-key (kbd "C-q") 'kill-buffer-and-window)
(global-set-key (kbd "C-c d") 'lsp-goto-type-definition)
(global-set-key (kbd "C-x m") 'toggle-margins)

;; Prevent accidental exits
(global-unset-key (kbd "C-x C-c"))

;; ====================
;; Org Mode Settings
;; ====================

(use-package org
  :ensure nil
  :config
  (setq org-startup-truncated nil))

;; ====================
;; Scratch Buffer Persistence
;; ====================

(setq remember-notes-buffer-name "*scratch*"
      initial-buffer-choice (lambda ()
                              (ignore-errors
                                (kill-buffer remember-notes-buffer-name))
                              (remember-notes)))

;; ====================
;; Custom Variables
;; ====================

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5c9bd73de767fa0d0ea71ee2f3ca6fe77261d931c3d4f7cca0734e2a3282f439" "03f28a4e25d3ce7e8826b0a67441826c744cbf47077fb5bc9ddb18afe115005f" "d64a2afb4f2b196266dc25be670d44a45a94a9f8499279e3863d8a9d54711ed1" "07cb8ee4f51bde500e71e6da1311f2349d6f2e21570bcd9d0d85f5147d77c4a9" "c376e68aa20c8648d8b797cdf62e067761721e0b5d365f7957ad5a17504b1e61" "cfd51857f5e80eddece7eb5d30b9afce81f442707207e0d636250d03978a66ec" default))
 '(helm-completion-style 'helm)
 '(ispell-dictionary nil)
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(lsp-treemacs flycheck twilight-bright-theme emacsql-sqlite forge magit yasnippet-snippets undo-tree typescript-mode tree-sitter-langs tsi company lsp-ui lsp-mode helm-projectile helm cask writeroom-mode yaml-mode json-mode tree-sitter tsc quelpa-use-package quelpa exec-path-from-shell hl-todo smart-tabs-mode tabbar olivetti glsl-mode evil-commentary oceanic-theme evil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 82 :width normal :weight normal :foundry "ADBO" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod" :weight normal))))
 '(mmm-default-submode-face ((t nil)))
 '(org-document-title ((t (:weight bold :height 2.0))))
 '(org-level-1 ((t (:height 1.9 :family "ETBembo"))))
 '(org-level-2 ((t (:height 1.7 :family "ETBembo"))))
 '(org-level-3 ((t (:height 1.5 :family "ETBembo"))))
 '(org-level-4 ((t (:height 1.35 :family "ETBembo"))))
 '(org-level-5 ((t (:height 1.2 :family "ETBembo"))))
 '(variable-pitch ((t (:height 1.2 :family "DejaVu Sans Condensed")))))

(provide '.emacs)
;;; .emacs ends here
