;; hydras that don't belong anywhere else

;; Window hydra from Jerry Penz
(defvar aum-window--title (with-faicon "windows" "Window Management" 1 -0.05))
(pretty-hydra-define aum-window
  (:foreign-keys warn :title aum-window--title :quit-key "q" :color amaranth)
  ("Actions"
   (("TAB" other-window "switch")
    ("x" ace-delete-window "delete")
    ("m" ace-delete-other-windows "maximize")
    ("s" ace-swap-window "swap")
    ("a" ace-select-window "select"))

   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("b" split-window-right "horizontally")
    ("B" split-window-horizontally-instead "horizontally instead")
    ("v" split-window-below "vertically")
    ("V" split-window-vertically-instead "vertically instead"))

   "Zoom"
   (("+" zoom-in "in")
    ("=" zoom-in)
    ("-" zoom-out "out")
    ("0" jp-zoom-default "reset"))))

;; Toggles from Jerry Penz
(defvar aum-toggles--title (with-faicon "toggle-on" "Toggles" 1 -0.05))
(pretty-hydra-define aum-toggles
  (:color amaranth :quit-key "q" :title aum-toggles--title)
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("o" olivetti-mode "olivetti" :toggle t))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(provide 'aum-hydras)
