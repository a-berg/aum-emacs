(general-define-key
 :states '(normal motion visual insert emacs)
 :keymaps 'override
 :prefix "SPC"
 :non-normal-prefix "C-SPC"

 "<SPC>" 'counsel-M-x

 "b"  '(:ignore t :which-key "Buffers")
 "ba" 'counsel-ibuffer
 "bb" 'ivy-switch-buffer
 "bd" 'aum/kill-this-buffer

 "f"  '(:ignore t :which-key "Files")
 "ff" 'counsel-find-file
 "fs" 'save-buffer)





(provide 'aum-keybindings)
