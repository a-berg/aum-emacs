;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
                        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(eval-and-compile
  (push "./modules" load-path))


(require 'aum-functions-macros)

(use-package emacs
  :config
  (setq inhibit-startup-message t
        visible-bell t) ;; hmmmm do I want this?
  ;; Frame size (for early development purposes)
  (setq default-frame-alist
        (append (list
                 '(width . 100)               ;; width (in characters)
                 '(height . 60))))            ;; height (in characters)

  (setq-default fill-column 88)

  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (blink-cursor-mode 0)       ; I hate blinking cursors
  (menu-bar-mode -1)          ; Disable the menu bar

  ;; line numbers!
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq-default display-line-numbers-type 'relative
                display-line-numbers-current-absolute t
                display-line-numbers-width 3
                display-line-numbers-widen t)
  (dolist (mode '(term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))
  )

(defvar aum/default-font-size 120) ;; variable. Maybe use noweb in org or some other config file?
(use-package emacs
  :config
  (set-face-attribute 'default nil :font "Iosevka SS04" :height aum/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka SS04" :height aum/default-font-size))
;; (set-face-attribute 'variable-pitch nil :font "Fira Sans" :height 130 :weight 'regular)

(use-package all-the-icons)

(use-package silkworm-theme
  :ensure t
  :config
  (load-theme 'silkworm t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 35)))

(use-package emacs
  :config
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package general
  :config
  (general-create-definer aum/leader-keys
                          :keymaps '(normal insert visual emacs)
                          :prefix "SPC"
                          :global-prefix "C-SPC"))

(require 'aum-keybindings)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(defun aum/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . aum/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
 (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred))))  ; or lsp

(use-package conda
  :config
  ;; Need to do this a bit better, but ~ doesnt work...
  (setq conda-anaconda-home "/home/adrian/miniconda3"
        conda-env-home-directory "/home/adrian/miniconda3"))

(use-package company
  :diminish company-mode
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))
