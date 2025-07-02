;; emacs performace improvement hacks, startups + lsp
;;(setq gc-cons-threshold (* 50 1000 1000))
;;(setq lsp-log-io nil)  if set to true can cause a performance hit
;;(setq read-process-output-max (* 1024 1024)) ; 1mb
;; take a second look later at below
;;(setq lsp-enable-file-watchers nil)

(setq warning-minimum-level :emergency)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
;; (toggle-frame-maximized)
(scroll-bar-mode 0)

;; #performace and loading
(global-so-long-mode 1)
;;(global-font-lock-mode 0)
(setq ac-auto-start 5) ;set autocomplete to higher threshold
(global-visual-line-mode 1)
(setq-default truncate-lines t)


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
;;(tab-bar-mode)
;; (add-hook 'after-init-hook (lambda () (set-frame-font "Inconsolata Semicondensed-12:style=Medium")))
;; (add-hook 'after-init-hook (lambda () (set-frame-font "SF Mono-11:style=Medium")))
;; (add-hook 'after-init-hook (lambda () (set-frame-font "IBM Plex Mono-12:style=Medium")))
(add-hook 'after-init-hook (lambda () (set-frame-font "Noto Sans Mono Condensed-12:style=Medium")))
;; (add-hook 'after-init-hook (lambda () (set-frame-font "FiraCode Nerd Font-12:style=Regular")))
;; (set-frame-font "Noto Sans Mono SemiCondensed-12")
;;(set-frame-font "Iosevka Nerd Font Mono-13" nil t)

;; simple font at startup
;(set-frame-font "Fira Code-12")
;;(package-refresh-contents)

(setq dired-listing-switches "-alh")
;; (use-package engrave-faces)
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "C-,") nil)))

;; org-mode
;;(require 'org)
;; unbind `C-,' key, which is used to save files (evil mode keybinding)

;; enable TAB key in org mode
(setq evil-want-C-i-jump nil)
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
;; set margin
(setq org-latex-packages-alist '(("margin=2cm" "geometry" nil)))
(setq org-latex-src-block-backend 'engraved)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(transient-mark-mode 1)

(add-hook 'c-mode-hook
  (lambda ()
      (c-set-style "linux")
      (c-toggle-auto-state)))

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

(use-package evil
  :ensure t
  :init
  ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  ;; (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

;; (use-package evil-collection
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-collection-init))
;;(evil-set-initial-state 'dired-mode 'emacs)

(repeat-mode 1)

;; Installed Packages

(use-package vterm :load-path "~/emacs-libvterm")
(add-to-list 'display-buffer-alist
     '("\*vterm\*"
       (display-buffer-in-side-window)
       (window-height . 0.25)
       (side . right)
       (slot . 0))) 

;; (tab-bar-mode 1)
(use-package rust-mode
  :ensure t
  :defer t)
(add-hook 'rust-mode-hook 'rust-ts-mode)

(use-package projectile
  :ensure t
  :defer t)

(use-package smart-compile
  :ensure t
  :defer t)
(global-set-key (kbd "C-x C-l") 'smart-compile)

(use-package lsp-ivy
  :ensure t
  :defer t)

(use-package lsp-haskell
  :ensure t
  :defer t)

(use-package lsp-treemacs
  :ensure t
  :defer t)

(use-package undo-tree
  :ensure t
  :defer t)

(use-package multi-term
  :ensure t
  :defer t)

(use-package pdf-tools
  :ensure t
  :defer t)
(use-package beacon
  :ensure t
  :defer t
  :config
  (beacon-mode 1))
;; test startup time M-x esup
(use-package esup
  :defer t
  :config
  ;; Work around a bug where esup tries to step into the byte-compiled
  ;; version of `cl-lib', and fails horribly.
  (setq esup-depth 0))

(use-package magit
  :ensure t
  :defer t)
;; (use-package evil-goggles
;;   :ensure t
;;   :config
;;   (evil-goggles-mode)
;;   (evil-goggles-use-diff-faces))

(use-package elpy
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'elpy-enable)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local company-backends '(elpy-company-backend)))))

;;(elpy-company-backend 1)

;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (elpy-enable))

(use-package lsp-pyright
  :ensure t
  :defer 5
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package format-all
  :config
  (format-all-mode t))

;(use-package dirvish)
;(dirvish-override-dired-mode)

(use-package docker-compose-mode)
(use-package ace-window)
(use-package direnv
 :config
 (direnv-mode))
(use-package clang-format
  :config
  (setq clang-format-style "file"))
(add-hook 'before-save-hook 'clang-format-buffer)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-frame-mode 1)
  (lsp-ui-sideline-enable t)
  (lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
  (lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
  (lsp-ui-flycheck-enable t)
  (lsp-ui-imenu-enable t))

;; (define-prefix-command 'my-g-keymap)
;; (global-set-key (kbd "g") 'my-g-keymap)
;; (define-key my-g-keymap (kbd "d") 'xref-find-definitions)

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "~/cp/")
  (yas-global-mode 1))

;; elpy configs
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(setq lsp-headerline-breadcrumb-enable nil)

;; ------------------------------------lsp-mode package

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-headerline-breadcrumb-mode nil)
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;;(lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (setq lsp-rust-server 'rust-analyzer)
  ;;  (setq rustic-format-on-save t)
  (rust-enable-format-on-save)
  (lsp-inlay-hints-mode)
  (lsp-lens-enable nil) ;; disable no of references found and stuff
  (lsp-inlay-hint-enable t)
  (setq lsp-eldoc-hook nil)

  (setq lsp-headerline-breadcrumb-enable nil)
  (setq eldoc-echo-area-use-multiline-p nil) ;; turn off the hints at the bottom of the screen
  (setq lsp-signature-auto-activate t)
  (setq lsp-enable-symbol-highlighting t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; go -setup

(use-package go-mode
  :ensure t)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook 'lsp-deferred)
(define-key go-mode-map (kbd "C-c f") 'lsp-find-definition)


;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


;; optionally if you want to use debugger
(use-package dap-mode
  :defer t
  :commands (dap-debug dap-hydra)
  :config
  ;; additional dap-mode configuration here
)
;; (use-package dap-language) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; lsp-mode end---------------------------------------

;; ----------------------------------------------------- flycheck + posframe setup
(use-package flycheck :ensure)
(use-package flycheck-posframe)
(global-flycheck-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; set the background color and other options (optional)
(with-eval-after-load 'flycheck-posframe
  (setq flycheck-posframe-warning-prefix "⚠ "))
(setq flycheck-idle-change-delay 2) ; increase the delay between checks to 2 seconds
(setq flycheck-posframe-inhibit-functions
      (delq 'flycheck-posframe-inhibit-proced flycheck-posframe-inhibit-functions))
;; Customize the appearance of the floating window
;;(setq flycheck-posframe-position 'bottom)
(setq multi-term-program "/nix/store/rra18alcyj16wz2sm2rw5rnji7dgn7pp-system-path/bin/zsh")
;;(setq multi-term-program "/usr/bin/zsh")
;;(setq multi-term-program "/nix/store/rra18alcyj16wz2sm2rw5rnji7dgn7pp-system-path/bin/bash")
;; ----------------------------------------------------- posframe setup
(use-package flycheck-rust)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;(use-package smart-mode-line
;  :config
;  (smart-mode-line-enable))
(use-package nix-mode
  :mode "\\.nix\\'")
(use-package buffer-move)
(use-package ivy)
(use-package all-the-icons)
;; ---------------------------------------Package Setups
(global-undo-tree-mode 1)
;(undo-tree-mode t)
(evil-set-undo-system 'undo-tree)
(setq undo-tree-enable-undo-in-region nil)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory "~/.emacs.d/undo-history/")

;; ---------------------------------------Package setup ends



;; custom-keybindings
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
;(global-set-key (kbd "M-, ,") 'save-buffer)
(global-set-key (kbd "C-,") 'save-buffer)
(define-key evil-normal-state-map (kbd "g;") 'goto-last-change)
(global-set-key (kbd "C-x f") 'indent-region)
;; 4-sized tab space
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; RUSTIC------------------------------

;; (use-package rustic
;;   :ensure
;;   :bind (:map rustic-mode-map
;;               ("M-j" . lsp-ui-imenu)
;;               ("M-?" . lsp-find-references)
;;               ("C-c C-c l" . flycheck-list-errors)
;;               ("C-c C-c a" . lsp-execute-code-action)
;;               ("C-c C-c r" . lsp-rename)
;;               ("C-c C-c q" . lsp-workspace-restart)
;;               ("C-c C-c Q" . lsp-workspace-shutdown)
;;               ("C-c C-c s" . lsp-rust-analyzer-status))
;;   :config
;;   ;; uncomment for less flashiness
;;   ;; (setq lsp-eldoc-hook nil)
;;   ;; (setq lsp-enable-symbol-highlighting nil)
;;   ;; (setq lsp-signature-auto-activate nil)

;;   ;; comment to disable rustfmt on save
;;   (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

;; (defun rk/rustic-mode-hook ()
;;   ;; so that run C-c C-c C-r works without having to confirm, but don't try to
;;   ;; save rust buffers that are not file visiting. Once
;;   ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
;;   ;; no longer be necessary.
;;   (when buffer-file-name
;;     (setq-local buffer-save-without-query t))
;;   (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; RUSTIC END--------------------------------------------------

;; LSP SETUP --------------------

;; ccls for c/c++/objective c
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "/nix/store/s2m73zqj03kp432nx7ci83qgs6aqmx98-user-environment/bin/ccls")

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
  ;; (load-theme 'doom-vibrant t)
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

(global-set-key (kbd "C-x s") 'my-open-multi-term-below)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;(bind-key "k" (lambda () (interactive) (kill-buffer (current-buffer))) ctl-x-map)
(defun kill-buffer-and-window ()
  "Kill the current buffer and its associated window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))
(global-set-key (kbd "C-x k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-o") 'ace-window)
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

;; c/c++ with eglot
;; (progn
;;     (customize-set-variable 'eglot-autoshutdown t)
;;     (customize-set-variable 'eglot-extend-to-xref nil)
;;     (customize-set-variable 'eglot-ignored-server-capabilities
;;         (quote (:documentFormattingProvider :documentRangeFormattingProvider)))

;;     (with-eval-after-load 'eglot
;;         (setq completion-category-defaults nil)
;;         (add-to-list 'eglot-server-programs
;;             '(c-mode c++-mode
;;                  . ("clangd"
;;                        "-j=4"
;;                        "--malloc-trim"
;;                        "--log=error"
;;                        "--background-index"
;;                        "--clang-tidy"
;;                        "--cross-file-rename"
;;                        "--completion-style=detailed"
;;                        "--pch-storage=memory"
;;                        "--header-insertion-decorators=0"))))

;;     (add-hook 'c-mode-hook #'eglot-ensure)
;;     (add-hook 'c++-mode-hook #'eglot-ensure)
;;     (add-hook 'rustic-mode-hook #'eglot-ensure))

;; eglot with c/c++ end
;; custom functions end-------------------------------------------

(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; ligatures support


(use-package ligature)
(ligature-set-ligatures 't '("www"))

;; Enable ligatures in programming modes 

(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)

;; ligature support another method

;; (when (window-system)
;;   (set-frame-font "Iosevka Nerd Font Mono-13"))
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

;; turnoff left-siderbar arrow word-wrap-glyph
(setq-default visual-line-fringe-indicators nil)

(setq-default fringe-indicator-alist '(
  (truncation left-arrow right-arrow)
  (continuation nil right-curly-arrow) ;; left-curly-arrow
  (overlay-arrow . right-triangle)
  (up . up-arrow)
  (down . down-arrow)
  (top top-left-angle top-right-angle)
  (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
  (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
  (empty-line . empty-line)
  (unknown . question-mark)))

;; custom-keybindings end

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; (setq backup-directory-alist '((".", "~/.emacs.d/.emacs_saves/"))
(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.
(setq vc-make-backup-files t)

(provide 'init)
;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
