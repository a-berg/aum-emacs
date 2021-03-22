;; Initialize package sources
(require 'package)
(setq-default load-prefer-newer t)

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

(setq user-emacs-directory "~/code/personal/aum-emacs/modules")
(add-to-list 'load-path user-emacs-directory)

(require 'aum-functions-macros)

(use-package emacs
  :hook ((before-save . delete-trailing-whitespace))
  :config
  ;; default to utf-8 for all the things
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Stop Emacs from littering the system with backup files.
  (setq make-backup-files nil
	auto-save-default nil
	create-lockfiles nil))

(use-package validate)

(use-package emacs
  :config
  (setq inhibit-startup-message t
	visible-bell t) ;; hmmmm do I want this?
  ;; Frame size (for early development purposes)
  (setq default-frame-alist
	(append (list
		 '(width . 100)               ;; width (in characters)
		 '(height . 60))))            ;; height (in characters)

  (toggle-frame-maximized)

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

  ;; Use visual line motions even outside_ of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
  (evil-add-to-alist 'evil-surround-pairs-alist
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ?\) '("( " . " )")
   ?\] '("[ " . " ]")
   ?\} '("{ " . " }")))

(use-package evil-commentary
  :ensure t
  :init
  (evil-commentary-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay .3))

(use-package hydra
  :config
  ;; (use-package hydra-posframe
  ;;   :custom
  ;;   (hydra-posframe-parameters
  ;;     '((left-fringe . 5)
  ;; 	(right-fringe . 5)))
  ;;   :custom-face
  ;;   (hydra-posframe-border-face ((t (:background "#6272a4"))))
  ;;   :hook (after-init . hydra-posframe-enable))
  (use-package major-mode-hydra
    :demand t
    :ensure t
    :general
    (:states '(normal visual)
	     "," 'major-mode-hydra)))

(require 'aum-hydras)

(use-package hydra-posframe
  :load-path "//home/adrian/code/ext/github.com/Ladicle/hydra-posframe"
  :hook (after-init . hydra-posframe-enable)
  :custom
  (hydra-posframe-parameters
   '((left-fringe . 5)
     (right-fringe . 5))))



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

(use-package evil-mc
  ;; :functions
  :config
  (global-evil-mc-mode  1)
  :bind
  ("M-u" . evil-mc-hydra/body)
  :pretty-hydra
  ((:title (with-faicon "i-cursor" "MULTIPLE CURSORS") :quit-key "q" :color amaranth)
   ("Basic"
    (("a" evil-mc-make-all-cursors "make all")
     ("u" evil-mc-undo-all-cursors "undo all")
     ("j" evil-mc-make-and-goto-next-match "make & goto next")
     ("k" evil-mc-make-and-goto-prev-match "make & goto prev"))
    ""
    (("<SPC>" nil "quit hydra" :color blue))
    ;; ("Skip" (("" nil "")))
    )))

(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(require 'aum-org-basic)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
	 ("\\.md\\'"       . markdown-mode)
	 ("README\\.md\\'" . gfm-mode))
  :config
  (use-package edit-indirect)
  (setq markdown-enable-math nil
	markdown-enable-wiki-links t
	;; markdown-nested-imen u-heading-index t
	markdown-asymmetric-header t
	markdown-footnote-location 'immediately
	markdown-use-pandoc-style-yaml-metadata t)
  :mode-hydra
  ((:title (with-octicon "markdown" "Markdown mode" 1 -0.05) :quit-key "q")
   ("Format"
    (("c" markdown-insert-code "code")
     ("b" markdown-insert-bold "bold")
     ("i" markdown-insert-italic "italic")
     ("s" markdown-insert-strike-through "strikethrough")
     )
    "Insert"
    (("`" markdown-insert-gfm-code-block "code block")
     ("h" markdown-insert-header "header")
     ("f" markdown-insert-footnote "footer")
     ("l" markdown-insert-link "link")
     )
    "Other"
    (("N" markdown-navigation-hydra/body "navigation")
     ("P" run-pandoc "pandoc")))
   )
  :pretty-hydra
  (markdown-navigation-hydra
   (:title "✜ NAVIGATION" :quit-key "q" :color amaranth)
   ("Movement"
    (("k" markdown-outline-previous "↑" :exit nil)
     ("j" markdown-outline-next "↓" :exit nil)
     ("u" markdown-outline-up "up level" :exit nil)
     ("p" markdown-outline-previous-same-level "prev" :exit nil)
     ("n" markdown-outline-next-same-level "next" :exit nil))
    "Rearrange"
    (("K" markdown-move-subtree-up "move subtree up" :exit nil)
     ("J" markdown-move-subtree-down "move subtree down" :exit nil)
     ("C-k" markdown-move-up "move header up" :exit nil)
     ("C-j" markdown-move-down "move header down" :exit nil)
     ("[" markdown-promote-subtree "promote subtree" :exit nil)
     ("]" markdown-demote-subtree "demote subtree" :exit nil)))))

  ;; :hook
  ;; ('markdown-mode-hook . '(lambda ()
  ;;                           ;; (turn-on-flyspell)
  ;;                           ;; (hl-todo-mode)
  ;;                           (auto-fill-mode)
  ;;                           ;; (centered-cursor-mode 1)
  ;;                           (git-gutter-mode 1))))

(use-package auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.latex\\'" . latex-mode))
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    ;; (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    ;; (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq-default TeX-engine 'xetex)
    (setq TeX-auto-save t
          TeX-parse-self t
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil)))

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t))
  :config
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource")))

(use-package bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))

(pdf-loader-install)

(use-package pandoc-mode
  :ensure t
  :config
  (setq pandoc-use-async t)
  ;; stop pandoc from just hanging forever and not completing conversion
  ;; see https://github.com/joostkremers/pandoc-mode/issues/44
  (setq pandoc-process-connection-type nil)
  (progn
    (defun run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)))

(use-package yaml-mode)

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

(pretty-hydra-define lsp-hydra
  (:foreign-keys warn :title "LSP" :quit-key "q")
  ("Buffer"
   (("=" lsp-format-buffer)
    ("m" lsp-ui-imenu)
    ("x" lsp-execute-code-action))
   "Server"
   (("C-s" lsp-describe-session)
    ("C-r" lsp-restart-workspace)
    ("S" lsp-shutdown-workspace))
   "Symbol"
   (("d" lsp-find-declaration)
    ("D" lsp-ui-peek-find-definitions)
    ("R" lsp-ui-peek-find-references)
    ("i" lsp-ui-peek-find-implementation))
   ""
   (("t" lsp-find-type-definition)
    ("s" lsp-signature-help)
    ("o" lsp-describe-thing-at-point)
    ("r" lsp-rename))
   ))

(use-package python-mode
  :config
  (use-package conda
    :config
    ;; Need to do this a bit better, but ~ doesnt work...
    (setq conda-anaconda-home "/home/adrian/miniconda3"
	  conda-env-home-directory "/home/adrian/miniconda3"))
  :mode-hydra
  ((:title "Python mode" :quit-key "q")
   ("conda/envs"
   (("a" conda-env-activate "activate env")
    ("d" conda-env-deactivate "deactivate env"))
   "LSP"
   (("L" lsp-hydra/body "lsp hydra"))
   "✜ Navigation"
   (("j" python-nav-forward-defun "next defun" :exit nil)
    ("k" python-nav-backward-defun "prev defun" :exit nil)
    ("$" python-nav-end-of-defun "end of defun" :exit nil))
   "Eval"
   (("'" run-python "new shell")
    ("b" python-shell-send-buffer "buffer")
    ("e" python-shell-send-defun "defun")
    ("r" python-shell-send-region "region"))
   )))

(use-package lsp-mode
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook
  ((python-mode . lsp)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :bind (:map evil-normal-state-map
	      ("gd" . lsp-ui-peek-find-definitions)
	      ("gr" . lsp-ui-peek-find-references)))

(major-mode-hydra-define emacs-lisp-mode (:title (with-mode-icon major-mode "Emacs Lisp") :quit-key "q")
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "REPL"
   (("I" ielm "ielm"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (validate-setq
   yas-verbosity 1                      ; No need to be so verbose
   yas-wrap-around-region t)

  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '(yasnippet-snippets-dir)))

  (yas-reload-all)
  (yas-global-mode)
  :pretty-hydra
       (Yasnippet (:title (with-faicon "scissors" "SNIPPETS") :quit-key "q" :color amaranth)
	("Inserts"
	 (("i" yas-insert-snippet "Insert a Snippet":color blue)
	  ("a" yas-reload-all "Reload Snippets" :color blue)
	  ("e" yas-activate-extra-mode "Extra Mode")
	  ("d" yas-load-directory "Load a Snippet Directory")
	  ("m" yas/minor-mode "Activate Yas Minor":toggle t)
	  ("g" yas/global-mode "Always Use Yas":toggle t))
	 "Manage Snippets"
	 (("l" yas-describe-tables "List Snippets" :color blue)
	  ("n" yas-new-snippet "Create A New Snippet")
	  ("t" yas-tryout-snippet "Tryout A Snippet")
	  ("s" yas-load-snippet-buffer-and-close "Load and Save New Snippet" :color blue)

	  ("f" yas-visit-snippet-file "Edit Snippet File" :color blue))
	 "Auto Snippets"
	 (("w" aya-create "Create an Auto Snippet")
	  ("y" aya-expand "Expand an Auto Snippet" :color blue)
	  ("o" aya-open-line "Open Line")
	  ("h" hydra-helm/body "Return To Helm" :color blue )
	  ("<SPC>" nil "Exit Hydra" :color blue )))))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

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

(use-package smartparens
  :init
  (smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

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

(use-package persp-mode
  :pin melpa)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval add-hook
	   (quote after-save-hook)
	   (lambda nil
	     (org-babel-tangle))
	   nil t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
