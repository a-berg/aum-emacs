(with-eval-after-load 'evil
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "<SPC>" 'counsel-M-x
   "TAB" 'aum/switch-to-previous-buffer
   )

  (general-create-definer aum/spc-definer
    :states '(normal motion visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :keymaps 'override)

;;;;  Applications Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "a"  '(:ignore t :which-key "Applications")
   "ad" 'dired-jump
   ;; "as" 'sunrise-commander
   )

;;;;  Buffer Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "b"  '(:ignore t :which-key "Buffers")
   "ba" 'counsel-ibuffer
   "bb" 'ivy-switch-buffer
   ;; "bc" 'aum/copy-whole-buffer-to-clipboard
   "bd" 'aum/kill-this-buffer
   "bD" 'kill-bufer-and-window
   "be" 'erase-buffer
   "bn" 'evil-buffer-new
   "br" 'revert-buffer
   "bs" 'counsel-switch-buffer-other-window
   )

;;;;  Comment Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "c"   '(:ignore t :which-key "Commenting")
   ;; "cb"  '(nil :wk "Block Wrap")
   ;; "cbo" 'org-block-wrap
   ;; "cby" 'cpm/yaml-wrap
   "cc"  'evil-commentary
   "cl"  'evil-commentary-line
   "cy"  'evil-commentary-yank-line)

;;;;  File Keybindings
   (general-define-key
    :states '(normal motion visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC"

    "f"  '(:ignore t :which-key "Files")
    "fb" 'counsel-bookmark
    "fC"  '(nil :wk "Config files")
    ;; "fCb" 'bashrc
    "ff" 'counsel-find-file
    "fl" 'counsel-locate
    "fs" 'save-buffer
    "fr" 'counsel-recentf
    "ft" #'treemacs-select-window)

;;; Quit Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "q"  '(:ignore t :which-key "Quit")
   ;; "qq" 'cpm/save-desktop-save-buffers-kill-emacs
   ;; "qd" 'cpm/kill-emacs-capture-daemon
   "qq" 'evil-quit-all
   ;; "qQ" 'cpm/kill-all-emacsen
   "qr" 'restart-emacs
   )

;;; Version Control (Git) Keybindings
  (general-define-key
   :states '(normal motion visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "g"  '(:ignore t :which-key "Git")
   "gb" 'magit-blame
   "gc" 'magit-commit
   "gd" 'magit-diff
   ;; "gh" #'hydra-git-gutter/body
   "gl" 'magit-log
   ;; "gn" 'git-gutter:next-hunk
   ;; "gp" 'git-gutter:previous-hunk
   "gr" 'magit-reflog
   "gs" 'magit-status)

  (aum/spc-definer
   "t" '(aum-toggles/body :which-key "toggles (hydra)"))

  (aum/spc-definer
   "w" '(aum-window/body :which-key "windows (hydra)"))

  (aum/spc-definer
    "y" '(Yasnippet/body :which-key "snippets"))
)

(provide 'aum-keybindings)
