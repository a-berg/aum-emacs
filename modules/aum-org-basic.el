(use-package org
  :ensure t
  ;; :pin elpa
  :config
  (setq-default org-footnote-section nil
                org-return-follows-link t
                org-hide-emphasis-markers t
                org-pretty-entities t
                org-hide-leading-stars t
                )

  ;; Structures
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-tempo t))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  )

;; Babel conf
(use-package ob-python
  :ensure nil
  :defer t
  :commands (org-babel-execute:python)
  :config
  (progn
    (setq org-babel-python-command "python3")))

(use-package ob-shell
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :ensure nil
  :defer t
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :ensure nil
  :defer t
  :commands
  (org-babel-execute:latex))

;;; Org Babel Tangle
(use-package ob-tangle
  :ensure nil
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))
;; Look and feel
;;; org-superstar for bullets, this package supersedes org-bullets, giving an
;;; even nicer look.
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("❱" "⟩" "⟫" "▸"))
  (setq org-superstar-item-bullet-alist
        '((?+ . ?➤)
          (?* . ?•)
          (?- . ?–))))
  ;; (org-superstar-mode 1))



(provide 'aum-org-basic)
