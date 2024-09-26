;; init.el --- https://gligan.net/emacs/init.el

;;; Commentary:
;; 
;; A configuration for a decent Emacs experience without resorting to
;; external packages.

;;; Code:

;; Ellas theme ad hoc
(let ((main-bg              "#f8ede4")
      (main-fg              "#3d3b23")

      (bleached-silk        "#f6f6f5")

      (athens-sculpture-1   "#d3c3b6")
      (athens-sculpture-2   "#d3c3b6")
      (athens-sculpture-3   "#f1e7de")

      (warm-grey            "#59574c")
      (street-black         "#2a241f")

      (street-pink          "#be8a90")
      (charming-red         "#c24639")
      (agora-tile-red       "#b45952")
      (tourist-shoes-brown  "#3c2a19")
      (aged-jade            "#6b6757")
      (dark-sunshine        "#b09a49")
      (olive-green          "#898442")
      (olive-green-darker   "#665f18")
      (blueish-grey         "#607a96")
      (blueish-grey-lighter "#97c0eb")
      (ellas-blue           "#2b324c")
      (dark-neon-blue       "#001e86")
      (sky-blue-athens      "#074a96")
      (nice-light-blue      "#497fc1")
      )

  (deftheme ellas "A trip to Greece")

  (custom-theme-set-faces
   'ellas

   `(default ((t (:foreground ,main-fg :background ,main-bg ))))
   `(cursor ((t (:background ,olive-green ))))

   `(region ((t (:background ,athens-sculpture-1 ))))

   `(ansi-color-black ((t (:foreground ,street-black :background ,street-black))))
   `(ansi-color-red ((t (:foreground ,charming-red :background ,charming-red))))
   `(ansi-color-green ((t (:foreground ,olive-green :background ,olive-green))))
   `(ansi-color-yellow ((t (:foreground ,dark-sunshine :background ,dark-sunshine))))
   `(ansi-color-blue ((t (:foreground ,sky-blue-athens :background ,blueish-grey-lighter))))
   `(ansi-color-magenta ((t (:foreground ,street-pink :background ,street-pink))))
   `(ansi-color-cyan ((t (:foreground ,dark-neon-blue :background ,dark-neon-blue))))
   `(ansi-color-white ((t (:foreground ,bleached-silk :background ,bleached-silk)))) ; This may have a VERY bad costrast

   `(success ((t (:foreground ,olive-green :weight bold))))
   `(warning ((t (:foreground ,agora-tile-red :weight bold))))

   `(line-number ((t (:inherit default :foreground ,warm-grey ))))
   `(line-number-current-line ((t (:inherit default :foreground ,street-black ))))

   `(highlight ((t (:background ,athens-sculpture-1))))
   `(secondary-selection ((t (:background ,blueish-grey-lighter ))))

   `(fringe ((t (:background ,main-bg))))

   `(mode-line-active ((t (:foreground ,main-fg :background ,athens-sculpture-1 :box (:line-width 1 :color ,athens-sculpture-1) ))))
   `(mode-line-inactive ((t (:background ,athens-sculpture-3 :box (:line-width 1 :color ,athens-sculpture-1 ) ))))
   `(mode-line-buffer-id ((t (:bold t :color ,ellas-blue ))))

   `(vertical-border ((t (:foreground ,athens-sculpture-1 ))))

   `(font-lock-builtin-face ((t (:foreground ,tourist-shoes-brown :bold t ))))
   `(font-lock-comment-face ((t (:foreground ,olive-green :slant italic ))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,aged-jade :slant italic ))))
   `(font-lock-doc-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-punctuation-face ((t (:foreground ,aged-jade )))) ; Seems it's not working
   `(font-lock-function-name-face ((t (:foreground ,sky-blue-athens ))))
   `(font-lock-keyword-face ((t (:foreground ,dark-neon-blue ))))
   `(font-lock-string-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-escape-face ((t (:foreground ,olive-green ))))
   `(font-lock-number-face ((t (:foreground ,sky-blue-athens ))))
   `(font-lock-type-face ((t (:foreground ,olive-green-darker ))))
   `(font-lock-constant-face ((t (:foreground ,charming-red ))))
   `(font-lock-variable-name-face ((t (:foreground ,sky-blue-athens ))))
   `(font-lock-warning-face ((t (:foreground ,charming-red :bold t ))))

   `(minibuffer-prompt ((t (:foreground ,sky-blue-athens :bold t ))))
   '(hl-line ((t (:inherit nil :extend t))))

   '(italic ((t (:slant italic :underline nil))))

   ;; Matches
   '(match ((t nil)))
   '(show-paren-match ((t (:inverse-video t))))

   ;; Clickable elements
   `(link ((t (:foreground ,warm-grey :underline t ))))
   `(custom-button ((t (:foreground ,warm-grey :box (:line-width 3 :style ,warm-grey) :height 0.9 ))))
   `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))
   `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))

   ;; Org mode
   `(org-code ((t (:foreground ,sky-blue-athens ))))
   `(org-verbatim ((t (:foreground ,olive-green ))))
   `(org-block ((t (:background ,athens-sculpture-3 ))))
   )


;;;###autoload
  (and load-file-name
       (boundp 'custom-theme-load-path)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory load-file-name))))

  (provide-theme 'ellas))
;; end of Ellas theme

;; The looks
(load-theme 'ellas t)
(setq-default cursor-type 'bar)

;; Minimalistic appereance
(scroll-bar-mode 0)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; Font size (expressed in 10 * points per inch)
(set-face-attribute 'default nil :height 150)

;; Good (better) default behaviour
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(electric-pair-mode)
(delete-selection-mode 1)
(global-subword-mode)
(setq-default word-wrap t)
(setq initial-major-mode 'c-mode)
(setq confirm-kill-emacs 'y-or-n-p)
(setq use-short-answers t)
(pixel-scroll-precision-mode 1)
(column-number-mode 1)
(size-indication-mode)

(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

;; macOS cmd and alt key switch
(when (eq system-type 'darwin)
  (setq mac-command-modifier       'meta
        mac-option-modifier        'alt
        mac-right-option-modifier  nil
        mac-pass-control-to-system nil))

;; Serif font in text buffers
(add-hook 'text-mode-hook #'variable-pitch-mode)

;; Bind `compile' and `recompile' commands to unused key bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c x") 'recompile)

;; If we want people to use Emacs, they should be able to use their
;; muscle memory. I enable CUA mode so that common actions are bound
;; to their "normal" keybindings (those used by virtually all
;; programs).
(cua-mode)
