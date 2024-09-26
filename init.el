(defun display-startup-echo-area-message ()
  (message "%s seconds with %d collections"
           (emacs-init-time "%.2f")
           gcs-done))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(global-subword-mode)

(column-number-mode 1)
(size-indication-mode)

(setq-default word-wrap t)

(setq confirm-kill-emacs 'y-or-n-p)

(pixel-scroll-precision-mode 1)

(setq-default cursor-type 'bar)

;; No welcome message; no scratch buffer message
(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

(use-package mixed-pitch
  :ensure t
  :defer 3
  :defer t
  :hook (text-mode . mixed-pitch-mode))

(use-package dired
  ;; :hook (dired-mode-hook . 'dired-hide-details-mode)
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-lah"))

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c x") 'recompile)

(when (equal system-type 'darwin)
  (set-frame-parameter nil 'ns-transparent-titlebar 't))

(defun gligan/toggle-theme ()
  "Toggle between the system's light and dark modes"
  (interactive)
  ;; Mac OS
  (when (equal system-type 'darwin)
    (do-applescript
     "tell application \"System Events\"
        tell appearance preferences
          set dark mode to not dark mode
        end tell
      end tell")))

(global-set-key (kbd "M-g t t") 'gligan/toggle-theme)

(use-package auto-dark
  :ensure t
  :config
  (setq auto-dark-light-theme gligan/light-theme)
  (setq auto-dark-dark-theme gligan/dark-theme)

  (auto-dark-mode t))

(set-face-attribute 'default nil
                    :family gligan/fixed-pitch-font
                    :height gligan/fixed-pitch-font-size)
(set-face-attribute 'fixed-pitch nil
                    :family gligan/fixed-pitch-font
                    :height gligan/fixed-pitch-font-size)
(set-face-attribute 'variable-pitch nil
                    :family gligan/variable-pitch-font
                    :height gligan/variable-pitch-font-size)

(use-package ligature
  :ensure t
  :hook (after-init . global-ligature-mode)
  :config
  (ligature-set-ligatures
   't
   '("!!" "!!." "!=" "!==" "#!" "##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{" "$>" "%%" "&&" "(*" "*)" "**" "***" "*/" "*>" "++" "+++" "+:" "+>" "--" "---" "--->" "-->" "-:" "-<" "-<<" "->" "->>" "-|" "-~" ".-" ".." "..." "..<" ".=" ".?" "/*" "//" "///" "/=" "/==" "/>" ":+" ":-" "://" "::" ":::" "::=" ":<" ":=" ":>" ";;" "<!--" "<!---" "<$" "<$>" "<*" "<******>" "<*>" "<+" "<+>" "<-" "<--" "<---" "<---->" "<--->" "<-->" "<-<" "<->" "</" "</>" "<:" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<===" "<====>" "<===>" "<==>" "<=>" "<>" "<|" "<|>" "<||" "<|||" "<~" "<~>" "<~~" "=!=" "=/=" "=:" "=:=" "=<<" "==" "===" "===>" "==>" "=>" "=>>" ">-" ">->" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "?." "?:" "?=" "??" "[|" "\\\\" "]#" "^=" "__" "_|_" "www" "{|" "|-" "|=" "|>" "|]" "||" "||=" "||>" "|||>" "|}" "~-" "~=" "~>" "~@" "~~" "~~>")))

(use-package eglot
  :ensure t
  :defer t
  :hook ((c++-mode
          c-mode
          haskell-mode
          nim-mode
          python-mode
          rust-mode
          tuareg-mode
          zig-mode)
         . eglot-ensure)
  :custom
  (eglot-autoshutdown t))

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

(use-package sml-mode
  :config
  (setq sml-indent-level 2
        sml-indent-args  2))

(use-package zig
  :defer t
  :config
  (setq zig-format-on-save nil))

(setq sh-basic-offset 2)

(setq fish-indent-offset 2)

(setq css-indent-offset 2)

(defun gligan/org-mode-font-setup ()
  (dolist (face '((org-document-title . 1.8)
                  (org-level-1        . 1.3)
                  (org-level-2        . 1.2)
                  (org-level-3        . 1.1)))
    (set-face-attribute (car face) nil
                        :font gligan/variable-pitch-font
                        :weight 'regular
                        :height (cdr face))))

(use-package org
  :hook (org-mode . gligan/org-mode-font-setup)
  :config
  (setq org-directory 'gligan/org-documents-folder)

  ;; (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'fold)
  (setq org-startup-indented t)
  (setq org-highlight-latex-and-related '(latex))

  (setq org-confirm-babel-evaluate nil))

(use-package org-roam
  ;; :after org
  :custom
  (org-roam-directory (file-truename gligan/org-roam-folder))
  :config
  (org-roam-db-autosync-mode)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)		    
         (:map org-mode-map
               (("C-c n i" . org-roam-node-insert)
                ("C-c n o" . org-id-get-create)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-modern
  :ensure t
  :defer t
  :hook (org-mode . org-modern-mode))

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :defer 3
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package nerd-icons
  :defer 2
  :ensure t)

(use-package nerd-icons-dired
  :defer 2
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-corfu
  :defer 2
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package which-key
  :ensure t
  :defer 2
  :config
  (which-key-mode))

(use-package yafolding
  :ensure t
  :defer 8
  :hook (prog-mode . yafolding-mode))

(use-package pulsar
  :ensure t
  :hook (after-init-mode . pulsar-global-mode))

(setq gc-cons-threshold (* 2 1024 1024))
