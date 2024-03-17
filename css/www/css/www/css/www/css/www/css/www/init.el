(setq user-full-name "Ronaldo Gligan"
      user-mail-address "ronaldogligan@outlook.com")

(defun gligan/today () (interactive)
  (format-time-string "%Y-%m-%d"))
(global-set-key (kbd "M-g d") 'gligan/today)

;; Maximise window at start up
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha-background 95)

;; No window decorations
(add-to-list 'default-frame-alist '(undecorated . t))

;; Use two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)

;; Use a small bar as a cursor
(setq-default cursor-type 'bar)

;; I don't like line numbers
(setq display-line-numbers-type nil)

;; No welcome message; no scratch buffer message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Auto-save & back up files
(setq auto-save-default t
      make-backup-files t)

;; Show line and column
(column-number-mode 1)

(set-frame-font "Iosevka 12" nil t)

(setq gligan/dark-theme 'monokate)
(setq gligan/light-theme 'modus-operandi)

(setq gligan/current-theme gligan/dark-theme)

(load-theme gligan/current-theme t)

(defun gligan/toggle-theme () (interactive)
       (disable-theme gligan/current-theme)
       (setq next-theme (if (equal gligan/current-theme gligan/light-theme)
                            gligan/dark-theme
                            gligan/light-theme))
       (setq gligan/current-theme next-theme)
       (load-theme next-theme t))

(global-set-key (kbd "M-g t") 'gligan/toggle-theme)

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode 1))

(use-package flycheck
  :ensure t
  :defer t
  :config
  (global-flycheck-mode 1))

(use-package lsp-ui
  :ensure t
  :defer t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-doc-enable t)
  ;; You might want this:
  ; (lsp-ui-doc-show-with-cursor nil)
  ;; Also this because isearch gets broken otherwise
  ; (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-imenu-window-width 20))

(use-package lsp-mode
  :commands lsp
  :defer t
  :init
  ;; You need to restart emacs for keymap prefix to take effect
  ; (setq lsp-keymap-prefix "M-L")

  :config
  ;; so that dir-locals have effect:
  ;;   https://github.com/emacs-lsp/lsp-mode/issues/405
  (add-hook 'hack-local-variables-hook (lambda () (when lsp-mode (lsp))))

  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred)
         (lsp-managed-mode . lsp-modeline-diagnostics-mode)
         ;; if you want which-key integration
         ; (lsp-mode . (lambda () (lsp-enable-which-key-integration t)))
         )

  :custom
  (lsp-progress-via-spinner nil) ;; spinner seems to cause problems
  (lsp-restart 'ignore)
  (lsp-keep-workspace-alive nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-enable-snippet nil)

  ;; :global/:workspace/:file
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-file-watch-threshold 2000)
  ; (setq lsp-idle-delay 0.500)
  (lsp-completion-provider :capf))

(use-package tuareg
  :ensure t
  :defer t
  :delight "🐫")

(use-package dune
  :ensure t
  :defer t)

(use-package merlin
  :ensure t
  :defer t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :defer t
  :hook (tuareg . merlin-eldoc-setup))

(use-package flycheck-ocaml
  :ensure t
  :defer t
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :ensure t
  :defer t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package haskell-mode
  :ensure t
  :defer t
  :delight "λ"
  :config)

(use-package flycheck-haskell
  :ensure t
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package lsp-haskell
  :ensure t
  :defer t
  :after (haskell-mode lsp-mode)
  :config
  ;; Comment/uncomment this line to see interactions between lsp client/server.
  ;; (setq lsp-log-io t)
  :custom
  ;(lsp-haskell-process-args-hie '("-d" "-l" "/tmp/hie.log"))
  ;(lsp-haskell-server-args ())
  (lsp-haskell-server-path "haskell-language-server"))

(use-package company-irony
  :ensure t
  :defer t)
(use-package irony-eldoc
  :ensure t
  :defer t)
(use-package flycheck-irony
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

(setq org-directory "~/org/")

(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'fold)
(setq org-startup-indented t)

(setq org-preview-latex-default-process 'dvipng)

(setq journal-dir "~/org/journal/")
(use-package org-journal
  :ensure t
  :config
   (setq org-journal-date-format "%A, %d %B %Y") ; e.g. "Friday, 8 March 2024"
   (global-set-key (kbd "M-g j") 'org-journal-new-entry))

(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(define-skeleton org-header-skeleton
  "Header for my org files, mainly for blog posts"
  "Document title: " "#+title: " str "\n"
                     "#+author: " user-full-name "\n"
                     ;; "#+EMAIL: " user-mail-address "\n"
                     "#+date: " (format-time-string "%Y-%m-%d") "\n"
                     "#+setupfile: blog.config")

(use-package org-modern
  :ensure t
  :defer t
  :hook (org-mode . org-modern-mode))

(use-package olivetti
  :ensure t
  :defer t
  :hook (org-mode . olivetti-mode))

(setq org-latex-src-block-backend 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(electric-pair-mode 1)

(use-package ligature
  :ensure t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode t))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(use-package delight
  :ensure t)

(use-package vertico
  :ensure t
  :config
  (vertico-mode))

(use-package savehist
  :ensure t
  :config
  (savehist-mode))

(use-package orderless
  :ensure t
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-auto-tangle zig-mode vterm vertico utop org-modern org-journal orderless olivetti merlin-eldoc lsp-ui lsp-haskell ligature irony-eldoc htmlize hl-todo flycheck-ocaml flycheck-irony flycheck-haskell dune delight company-irony)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
