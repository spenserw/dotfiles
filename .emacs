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
 '(org-startup-truncated nil)
 '(package-selected-packages
   '(writeroom-mode yaml-mode json-mode tree-sitter-langs tree-sitter tsc jsdoc quelpa-use-package quelpa magit prettier-js exec-path-from-shell lsp-ui flycheck lsp-treemacs company lsp-mode hl-todo org-download vue-mode org-roam rg smart-tabs-mode ## yasnippet tabbar olivetti glsl-mode evil-commentary oceanic-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#1B2B34" :foreground "#D8DEE9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 92 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(font-lock-comment-face ((t (:foreground "goldenrod" :weight normal))))
 '(mmm-default-submode-face ((t nil)))
 '(org-document-title ((t (:weight bold :height 2.0))))
 '(org-level-1 ((t (:height 1.9 :family "ETBembo"))))
 '(org-level-2 ((t (:height 1.7 :family "ETBembo"))))
 '(org-level-3 ((t (:foreground "black" :height 1.5 :family "ETBembo"))))
 '(org-level-4 ((t (:foreground "black" :height 1.35 :family "ETBembo"))))
 '(org-level-5 ((t (:foreground "dim gray" :height 1.2 :family "ETBembo"))))
 '(variable-pitch ((t (:height 1.2 :family "DejaVu Sans Condensed")))))

;; Quelpa
(require 'quelpa-use-package)
(setq quelpa-use-package-inhibit-loading-quelpa t)

(require 'evil)
(evil-mode 1)
(evil-commentary-mode)

(smart-tabs-insinuate 'c 'c++)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(require 'tramp)
(setq tramp-default-method "scp")

;; Yasnippets
(require 'yasnippet)
(yas-global-mode 1)
;; Enable old org-structure-template-alist behaviour
(setq byte-compile-warnings '(cl-functions)) ;; silence warning about 'deprecated cl' in org-tempo
(require 'org-tempo)

;; Ripgrep
(require 'rg)

;; appearance & behavior
(tool-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'oceanic t)

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
		("DEV"    . "#16E01A")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")))
(global-hl-todo-mode 1)

;; Auto revert mode
(global-auto-revert-mode 1)

(setq backup-directory-alist `(("." . "~/.saves")))
(show-paren-mode 1)

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

;; Note taking commands & bindings
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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

;; org-roam
(setq org-roam-directory "~/documents/notes/org/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)
(require 'org-roam-protocol)

(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(defun org-notes-insert-link (file)
  "Insert link to note"
  (interactive "FFile:") ; `F` Interactive code signifies a file that may or may not exist.
  (insert (concat "[[" file "]" "[]]")) 
  (backward-char 2)) ; Reposition cursor to write the description.
(global-set-key (kbd "C-c i") 'org-notes-insert-link)

;; Org mode hooks
(add-hook 'org-mode-hook
  (lambda ()
    (define-key org-mode-map (kbd "C-k") nil)
	(define-key org-mode-map (kbd "C-j") nil)
	(setq org-fontify-whole-heading-line t
		  org-fontify-done-headline t
		  org-bullets-bullet-list '("\u200b")
		  org-hide-emphasis-markers t)
	(org-indent-mode t)
	(org-bullets-mode t)
	(variable-pitch-mode t)))

;; Notes - Biology
(add-to-list 'org-structure-template-alist
             '("B" . "#+TITLE:?\n* Encounters\n* Images\n* Resources\n"))

;; Convenience binds
(global-set-key (kbd "M-p") 'query-replace)
(global-set-key (kbd "C-q") 'kill-buffer-and-window)
(global-set-key (kbd "C-c f") 'org-roam-find-file)
(global-set-key (kbd "C-c r") 'org-roam-insert)

;; STOP CLOSING MY SESSION
(global-unset-key (kbd "C-x C-c"))

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; JSON
(require 'json-mode)

;; RVM
(require 'rvm)
(rvm-use-default)

;; NVM
(require 'nvm)
(nvm-use "17.7.1")

;; LSP Mode
(use-package lsp-mode
  :init
  :hook (web-mode . lsp)
  :custom
  (use-package flycheck)
  (use-package lsp-ui)
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/home/spenserw/.ts-server.log"))
  (lsp-eslint-options '((resolvePluginsRelativeTo . "/home/spenserw/.nvm/versions/node/v17.7.1/lib/node_modules")))
  (lsp-javascript-suggestion-actions-enabled 0)
  (nvm-use "17.7.1"))
(add-hook 'js-mode-hook #'lsp)
(add-hook 'ruby-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)

(use-package jsdoc
  :ensure t
  :quelpa (jsdoc
           :fetcher github
           :repo "isamert/jsdoc.el")
  :config
   (use-package tree-sitter)        ;; Required dependencies
   (use-package tree-sitter-langs)
   :hook (js-mode . tree-sitter-mode))

(global-set-key (kbd "C-c j") 'jsdoc)

;; Prettier
(require 'prettier-js)

;; Typescript.el
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
