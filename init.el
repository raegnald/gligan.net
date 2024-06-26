(setq user-full-name "Ronaldo Gligan"
      user-mail-address "ronaldogligan@outlook.com")

(defun gligan/today ()
  "Insert today's date as YYYY-MM-DD"
  (interactive) 
  (insert (format-time-string "%Y-%m-%d")))

(global-set-key (kbd "M-g d") 'gligan/today)

(defun gligan/open-terminal-emulator ()
  "Tries to open the first one available: vterm, eshell, term"
  (interactive)
  (cond
   ((package-installed-p 'vterm)
    (vterm))
   ((package-installed-p 'eshell)
    (eshell))
   (t
    (term))))

(global-set-key (kbd "M-g e") 'gligan/open-terminal-emulator)

;; Window transparency
(set-frame-parameter (selected-frame) 'alpha-background 95)

;; Use two spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(delete-selection-mode 1)

(global-subword-mode)

;; Use a small bar as a cursor
(setq-default cursor-type 'bar)

;; I don't like line numbers
(setq display-line-numbers-type nil)

;; No welcome message; no scratch buffer message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; C is more suitable as a default mode to me
(setq initial-major-mode 'c-mode)

;; Auto-save & back up files
(setq auto-save-default t
      make-backup-files t)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(defun display-startup-echo-area-message ()
  (message "%s with %d collections"
       (format "%.2f seconds"
               (float-time
                (time-subtract after-init-time before-init-time)))
       gcs-done))

(setq gligan/fixed-pitch-font "Iosevka Comfy Motion"
      gligan/mixed-pitch-font "Iosevka Comfy Motion Duo"
      gligan/default-font-size 130)

(defun gligan/set-font-when-available (face font height)
  (if (member font (font-family-list))
      (set-face-attribute face nil
                          :family font
                          :height height)
    (message (concat font " not available"))))

(gligan/set-font-when-available 'default
                                gligan/fixed-pitch-font
                                gligan/default-font-size)
(gligan/set-font-when-available 'fixed-pitch
                                gligan/fixed-pitch-font
                                gligan/default-font-size)
(gligan/set-font-when-available 'variable-pitch
                                gligan/mixed-pitch-font
                                gligan/default-font-size)

(defun gligan/org-mode-font-setup ()
  ;; These numbers below define the proportions of the org mode
  ;; headers in contrast to the paragraph text.
  (dolist (face '((org-document-title . 1.8)
                  (org-level-1        . 1.3)
                  (org-level-2        . 1.2)
                  (org-level-3        . 1.1)))
    (set-face-attribute (car face) nil
                        :font gligan/mixed-pitch-font
                        :weight 'regular
                        :height (cdr face))))

(use-package lsp-mode
  :ensure t
  :hook ((c-mode                        ; Alphabetically ordered
        haskell-mode
        nim-mode
        python-mode
        rust-mode
        tuareg-mode
        zig-mode)
         . lsp-deferred)
  :config
  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :ensure t)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  :init
  (global-corfu-mode))

(use-package irony-eldoc
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t
  :config)

(use-package tuareg
  :ensure t
  :defer t)

(use-package dune
  :ensure t
  :defer t)

(use-package merlin
  :ensure t
  :defer t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (setq merlin-error-after-save nil))

(use-package merlin-eldoc
  :ensure t
  :defer t
  :hook (tuareg . merlin-eldoc-setup))

(use-package utop
  :ensure t
  :defer t
  :config
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(use-package zig-mode
  :ensure t
  :defer t)

(use-package nim-mode
  :ensure t
  :defer t)

(use-package fish-mode
  :config
  (setq fish-indent-offset 2))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(defun gligan/tangle-only-org ()
  (when (equal major-mode 'org-mode)
    (org-babel-tangle)))

(use-package org
  :hook ((org-mode   . gligan/org-mode-font-setup)
         (after-save . gligan/tangle-only-org)) ; Auto tangle on save
                                                ; only in Org buffers
  :config
  (setq org-directory "~/Documents/Org/")

  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'fold)
  (setq org-startup-indented t)

  (setq org-confirm-babel-evaluate nil)

  (setq org-preview-latex-default-process 'dvipng))

(use-package org-journal
  :ensure t
  :bind (("M-g j j" . 'org-journal-new-entry)
         ("M-g j s" . 'org-journal-search-forever))
  :config
  (setq org-journal-dir "~/Documents/Org/Journal/"
        org-journal-date-format "%A, %d %B %Y")) ; e.g. "Friday, 8 March 2024")

(define-skeleton org-header-skeleton
  "Header for my org files, mainly for blog posts"
  nil
  "#+title: " (skeleton-read "Document title: ") "\n"
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
  ;; :hook (org-mode . olivetti-mode)
  )

(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode))

(setq org-latex-src-block-backend 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq copyright-query nil)              ; Don't ask
(add-hook 'before-save-hook #'copyright-update)

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ligature
  :ensure t
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures
   't
   '("!!" "!!." "!=" "!==" "#!" "##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{" "$>" "%%" "&&" "(*" "*)" "**" "***" "*/" "*>" "++" "+++" "+:" "+>" "--" "---" "--->" "-->" "-:" "-<" "-<<" "->" "->>" "-|" "-~" ".-" ".." "..." "..<" ".=" ".?" "/*" "//" "///" "/=" "/==" "/>" ":+" ":-" "://" "::" ":::" "::=" ":<" ":=" ":>" ";;" "<!--" "<!---" "<$" "<$>" "<*" "<******>" "<*>" "<+" "<+>" "<-" "<--" "<---" "<---->" "<--->" "<-->" "<-<" "<->" "</" "</>" "<:" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<===" "<====>" "<===>" "<==>" "<=>" "<>" "<|" "<|>" "<||" "<|||" "<~" "<~>" "<~~" "=!=" "=/=" "=:" "=:=" "=<<" "==" "===" "===>" "==>" "=>" "=>>" ">-" ">->" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "?." "?:" "?=" "??" "[|" "\\\\" "]#" "^=" "__" "_|_" "www" "{|" "|-" "|=" "|>" "|]" "||" "||=" "||>" "|||>" "|}" "~-" "~=" "~>" "~@" "~~" "~~>")))

(use-package hl-todo
  :defer t
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (vertico-mode))

(use-package savehist
  :ensure t
  :config
  (savehist-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless flex)
        completion-category-overrides '((eglot (styles . (orderless flex))))))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("M-g o" . 'consult-recent-file)
         ("M-g s" . 'consult-line-multi)
         ("M-g m" . 'consult-man))
  :config
  (recentf-mode))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-bar-width 0)
  (column-number-mode 1))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode))

(use-package yafolding
  :ensure t
  :hook (prog-mode . yafolding-mode))

(use-package transpose-frame
  :bind ("M-g t w" . transpose-frame))

(use-package string-inflection
  :bind ("M-s M-s" . string-inflection-all-cycle))

(use-package move-text
  :config
  (move-text-default-bindings))
