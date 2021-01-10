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

(defvar aum/default-font-size 120) ;; variable. Maybe use noweb in org or some other config file?

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(blink-cursor-mode 0)       ; I hate blinking cursors
(menu-bar-mode -1)          ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Frame size (for early development purposes)
(setq default-frame-alist
    (append (list
          '(width . 100)               ;; width (in characters)
          '(height . 60))))            ;; height (in characters)

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

(set-face-attribute 'default nil :font "Iosevka SS04" :height aum/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka SS04" :height aum/default-font-size)
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

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))
