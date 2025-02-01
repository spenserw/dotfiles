(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/elisp")

(server-start)

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
   '(lsp-treemacs flycheck twilight-bright-theme emacsql-sqlite forge magit yasnippet-snippets undo-tree typescript-mode tree-sitter-langs tsi helm-lsp company lsp-ui lsp-mode helm-projectile helm cask writeroom-mode yaml-mode json-mode tree-sitter tsc quelpa-use-package quelpa exec-path-from-shell hl-todo smart-tabs-mode ## tabbar olivetti glsl-mode evil-commentary oceanic-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1B2B34" :foreground "#D8DEE9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 82 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod" :weight normal))))
 '(mmm-default-submode-face ((t nil)))
 '(org-document-title ((t (:weight bold :height 2.0))))
 '(org-level-1 ((t (:height 1.9 :family "ETBembo"))))
 '(org-level-2 ((t (:height 1.7 :family "ETBembo"))))
 '(org-level-3 ((t (:height 1.5 :family "ETBembo"))))
 '(org-level-4 ((t (:foreground "black" :height 1.35 :family "ETBembo"))))
 '(org-level-5 ((t (:foreground "dim gray" :height 1.2 :family "ETBembo"))))
 '(variable-pitch ((t (:height 1.2 :family "DejaVu Sans Condensed")))))

;; Quelpa
(require 'quelpa-use-package)
(setq quelpa-use-package-inhibit-loading-quelpa t)

(require 'evil)
(evil-mode 1)
(evil-commentary-mode)
(global-undo-tree-mode)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(evil-set-undo-system 'undo-tree)

(smart-tabs-insinuate 'c 'c++)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; SSH Agent
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; Yasnippets
(require 'yasnippet)
(yas-global-mode 1)

;; appearance & behavior
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'oceanic t)
;; (load-theme 'twilight-bright t)
(setq redisplay-dont-pause t)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("FIX"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("DONE"  . "#A020F0")
        ("DEV"    . "#16E01A")
        ("TOP"    . "#16E01A")
        ("GOTCHA" . "#FF4500")
        ("LOW" . "#FF4500")
        ("HIGH"   . "#1E90FF")
        ("STUB"   . "#1E90FF")))
(global-hl-todo-mode 1)

;; Show full file paths
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Auto revert mode
(global-auto-revert-mode 1)

;; Preserve scratch
(setq remember-notes-buffer-name "*scratch*"
      initial-buffer-choice (lambda ()
                              (kill-buffer remember-notes-buffer-name)
                              (remember-notes)))

;; Put backup files neatly away
(let ((backup-dir "~/.saves")
      (auto-saves-dir "~/.saves"))
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
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; Open vertically by default
(setq
   split-width-threshold 0
   split-height-threshold nil)

;; Toggle margins
(defun toggle-margins ()
"Set margins in current buffer."
(interactive)
  (if (or (> left-margin-width 0) (> right-margin-width 0))
    (progn
      (setq left-margin-width 0)
      (setq right-margin-width 0)
      (set-window-buffer (selected-window) (current-buffer)))
    (setq left-margin-width 25)
    (setq right-margin-width 23)
    (set-window-buffer (selected-window) (current-buffer))))

(global-set-key (kbd "C-x m") 'toggle-margins)

;; Vim emulation settings
(global-set-key (kbd "C-f") 'evil-force-normal-state)
(global-set-key (kbd "C-u") 'scroll-down-command)
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)

(require 'org)
(defun writing-mode ()
  (interactive)
  (setq-default word-wrap t)
  (setq-default left-margin-width 25
				right-margin-width 23
				mode-line-format nil)
  (set-fringe-mode 0)
  (set-window-buffer nil (current-buffer))
  (set-background-color "#fbf8ef")
  (set-foreground-color "#1c1e1f")
  'variable-pitch-mode)

;; Convenience binds
(global-set-key (kbd "M-p") 'query-replace)
(global-set-key (kbd "C-c r") 'lsp-rename)
(global-set-key (kbd "C-q") 'kill-buffer-and-window)
(global-set-key (kbd "C-c d") 'lsp-goto-type-definition)

;; STOP CLOSING MY SESSION
(global-unset-key (kbd "C-x C-c"))

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; NVM
(require 'nvm)
(nvm-use "16")

;; RVM
(require 'rvm)
(rvm-use-default)

;; JSON
(require 'json-mode)

;; Projectile
(setq projectile-keymap-prefix (kbd "C-c p"))
(setq projectile-project-search-path '("~/code/"))
(projectile-mode)

;; Helm
(helm-mode)
(global-set-key (kbd "C-x b") 'helm-multi-files)

;; Tree sitter
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))



;; (setq debug-on-error f)
;; LSP Mode
;; (setq lsp-log-io t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 4))
(setq lsp-diagnostics-provider :flycheck)
(use-package lsp-mode
  :init
  :hook (web-mode . lsp)
  :custom
  (use-package flycheck)
  (use-package lsp-ui)
  (lsp-disabled-clients '(angular-ls))
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.vendor\\'")
  (add-to-list 'lsp-solargraph-library-directories "/home/spenserw/.local/share/gem/ruby/3.0.0/gems")
  (lsp-typescript-suggest-complete-function-calls nil)
  (lsp-typescript-preferences-quote-style "single")
  (lsp-clients-typescript-max-ts-server-memory 2048)
  (flycheck-checker-error-threshold 1000))

(add-hook 'html-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'ruby-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'css-mode-hook #'lsp)

(setq-default show-trailing-whitespace t)

;; Typescript.el
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; CSS
(setq-default css-indent-offset 2)

;; Forge
(with-eval-after-load 'magit
  (require 'forge))

(setq auth-sources '("~/.authinfo"))
