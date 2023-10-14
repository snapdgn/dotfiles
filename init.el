(setq warning-minimum-level :emergency)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'package)
(require 'use-package)
(package-initialize)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; ido mode configs
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;; ido mode configs end
(show-paren-mode 1)
;(set-frame-font "Noto Sans Mono SemiCondensed-12")
(set-frame-font "Iosevka Nerd Font Mono-13")
;(set-frame-font "Fira Code-12")
;;(package-refresh-contents)

;; --------------------------------------------staight.el installation start
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; -----------------------------------------------staraight.el installtion end

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Install use-package support
(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package evil :demand t)
(evil-mode 1)

;; Installed Packages
(use-package rust-mode)
(use-package lsp-ui)
(use-package projectile)
(use-package lsp-ivy)
(use-package lsp-haskell)
(use-package lsp-treemacs)
(use-package undo-tree)
(use-package multi-term)
(use-package direnv
 :config
 (direnv-mode))
(use-package clang-format
  :config
  (setq clang-format-style "file"))
(use-package magit)
(add-hook 'before-save-hook 'clang-format-buffer)

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/cp/")
  (yas-global-mode 1))
;; ------------------------------------lsp-mode package
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"))
;  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; lsp-mode end---------------------------------------

;; ----------------------------------------------------- flycheck + posframe setup
(use-package flycheck)
(use-package flycheck-posframe)
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; Set the background color and other options (optional)
(with-eval-after-load 'flycheck-posframe
  (setq flycheck-posframe-warning-prefix "⚠ "))
(setq flycheck-idle-change-delay 2) ; Increase the delay between checks to 2 seconds
(setq flycheck-posframe-inhibit-functions
      (delq 'flycheck-posframe-inhibit-proced flycheck-posframe-inhibit-functions))
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; Customize the appearance of the floating window
(setq flycheck-posframe-position 'center)
(setq multi-term-program "/nix/store/rra18alcyj16wz2sm2rw5rnji7dgn7pp-system-path/bin/zsh")
;;(setq multi-term-program "/nix/store/rra18alcyj16wz2sm2rw5rnji7dgn7pp-system-path/bin/bash")
;; ----------------------------------------------------- posframe setup
(use-package flycheck-rust)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package ivy)
(use-package all-the-icons)
;; ---------------------------------------Package Setups
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory "~/.emacs.d/undo-history/")

;; ---------------------------------------Package setup ends



;; custom-keybindings
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
;(global-set-key (kbd "M-, ,") 'save-buffer)
(global-set-key (kbd "C-,") 'save-buffer)
(define-key evil-normal-state-map (kbd "g;") 'goto-last-change)

(setq lsp-rust-server 'rust-analyzer)
;; LSP SETUP --------------------

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;(with-eval-after-load 'lsp-mode
;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; -----treesitter setup
(require 'treesit)
(add-to-list
 'treesit-language-source-alist
 '(python "https://github.com/tree-sitter/tree-sitter-python.git"))

;; -----------------------------packages end-------------------------------
;; -----------------------------themes configuration-----------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'molokai t)
;(load-theme 'doom-nord-aurora t)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  ;;(Doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))
;; -----------------------------themes configuration-----------------------


;; misc

(defun my-open-multi-term-below ()
  "Open a multi-term shell in a split window at the bottom."
  (interactive)
  (split-window-below)
  (other-window 1) ; Move the cursor to the new window
  (multi-term))

(global-display-line-numbers-mode t)
;; -------------------global keymaps

(global-set-key (kbd "C-x t") 'my-open-multi-term-below)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;(bind-key "k" (lambda () (interactive) (kill-buffer (current-buffer))) ctl-x-map)
(defun kill-buffer-and-window ()
  "Kill the current buffer and its associated window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

;; custom functions ------------------------------------------

(defun kill-buffer-and-window ()
  "Kill the current buffer and its associated window with a single key confirmation."
  (interactive)
  (if (y-or-n-p "Kill this buffer and its window? ")
      (progn
        (kill-buffer (current-buffer))
        (delete-window))))

(global-set-key (kbd "C-x k") 'kill-buffer-and-window)

(defun toggle-comment-or-uncomment-region ()
  "Toggle comment or uncomment the selected region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "M-;") 'toggle-comment-or-uncomment-region)

;; custom functions end-------------------------------------------

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)


;; custom-keybindings end

(setq backup-directory-alist '((".", "~/.emacs_saves"))
