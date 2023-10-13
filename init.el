(setq warning-minimum-level :emergency)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; ido mode configs
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;; ido mode configs end
(show-paren-mode 1)
(set-frame-font "Noto Sans Mono SemiCondensed-12")
;(set-frame-font "Fira Code-12")
;;(package-refresh-contents)

;; elpaca package manager setup
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)
;; elpaca setup done

;; elpaca package install

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil :demand t)
(evil-mode 1)

;; custom-keybindings
(define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)

;(use-package evil
;  :demand t
;  :init
;  (setq evil-want-C-j-bindings nil) ; Unbind C-j in insert state
;  :config
;  ;(define-key evil-insert-state-map "C-j" nil) ; Unmap C-j
;  (define-key evil-insert-state-map "M-j" 'evil-normal-state) ; Map C-j to evil-normal-state
;  (evil-mode 1))

;; Installed Packages
(elpaca lsp-mode)
(elpaca rust-mode)
(elpaca gruvbox-theme :ensure t)
(elpaca lsp-ui)
(elpaca projectile)
(elpaca lsp-ivy)
(elpaca lsp-haskell)
(elpaca lsp-treemacs)

(use-package flycheck
  :defer t
  :hook (after-init . global-flycheck-mode))

;;(add-hook 'after-init-hook #'global-flycheck-mode)
(elpaca company)
(add-hook 'after-init-hook 'global-company-mode)

(elpaca ivy)
(elpaca all-the-icons)


(setq lsp-rust-server 'rust-analyzer)
;; LSP SETUP --------------------

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
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
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)


;; custom-keybindings end

(setq backup-directory-alist '((".", "~/.emacs_saves"))

;; global keys
(global-set-key [(control x) (k)] 'kill-this-buffer)
