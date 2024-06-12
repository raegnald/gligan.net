;;; ellas-theme.el-theme.el --- ellas-theme.el
;;; Version: 1.0
;;; Commentary:
;;; Ellas is an Emacs theme inspired by a trip made to Greece
;;; Code:

(let ((main-bg              "#090e13")
      (main-fg              "#dee8f1")

      (modeline-bg          "#24384B")
      (modeline-bg-incative "#162330")

      (liliac               "#BFB5FF")
      (pineapple            "#e8d283")
      (champagne            "#e2d7ae")
      (keyword-yellow       "#f1d384")

      (region-blue          "#225487")
      (make-up              "#e2bbae")
      (almost-grey          "#ccc9c9")
      (cotton-candy         "#f5c5d1")
      (autumn               "#ffb951")
      (modern-blue          "#b7d0ea")
      (nice-blue            "#9dcdff")

      (salmonish            "#ff8660")
      (old-green-wall       "#aad2ae")
      (almost-black         "#05080a")
      )

  (deftheme somnus "A theme to focus and not to strain your eyes during a late night software development session.")

  (custom-theme-set-faces
   'somnus

   `(default ((t (:foreground ,main-fg :background ,main-bg ))))
   `(cursor ((t (:background ,pineapple ))))

   `(region ((t (:background ,modeline-bg ))))

   `(ansi-color-black ((t (:foreground ,almost-black :background ,almost-black))))
   `(ansi-color-red ((t (:foreground ,salmonish :background ,salmonish))))
   `(ansi-color-green ((t (:foreground ,old-green-wall :background ,old-green-wall))))
   `(ansi-color-yellow ((t (:foreground ,keyword-yellow :background ,keyword-yellow))))
   `(ansi-color-blue ((t (:foreground ,region-blue :background ,region-blue))))
   `(ansi-color-magenta ((t (:foreground ,cotton-candy :background ,cotton-candy))))
   `(ansi-color-cyan ((t (:foreground ,nice-blue :background ,nice-blue))))
   `(ansi-color-white ((t (:foreground ,main-fg :background ,main-fg))))

   `(success ((t (:foreground ,old-green-wall :weight bold))))
   `(warning ((t (:foreground ,salmonish :weight bold))))

   `(line-number ((t (:inherit default :foreground ,almost-grey ))))
   `(line-number-current-line ((t (:inherit default :foreground ,almost-grey ))))

   `(highlight ((t (:background ,modeline-bg))))
   `(secondary-selection ((t (:background ,modeline-bg-incative ))))

   `(fringe ((t (:background ,main-bg))))

   `(mode-line ((t (:foreground ,main-fg :background ,modeline-bg ))))
   `(mode-line-inactive ((t (:background ,modeline-bg-incative ))))
   `(mode-line-buffer-id ((t (:bold t :color ,pineapple ))))

   `(vertical-border ((t (:foreground ,modeline-bg ))))

   `(font-lock-builtin-face ((t (:foreground ,liliac :bold t ))))
   `(font-lock-comment-face ((t (:foreground ,make-up :slant italic ))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,almost-grey :slant italic ))))
   ;; `(font-lock-doc-face ((t (:foreground ,agora-tile-red ))))
   `(font-lock-punctuation-face ((t (:foreground ,modeline-bg-incative )))) ; Seems it's not working
   `(font-lock-function-name-face ((t (:foreground ,nice-blue ))))
   `(font-lock-variable-name-face ((t (:foreground ,modern-blue ))))
   `(font-lock-keyword-face ((t (:foreground ,keyword-yellow ))))
   `(font-lock-string-face ((t (:foreground ,cotton-candy ))))
   ;; `(font-lock-escape-face ((t (:foreground ,olive-green ))))
   `(font-lock-number-face ((t (:foreground ,autumn ))))
   `(font-lock-constant-face ((t (:foreground ,autumn ))))
   `(font-lock-type-face ((t (:foreground ,champagne ))))
   ;; `(font-lock-warning-face ((t (:foreground ,charming-red :bold t ))))

   ;; `(minibuffer-prompt ((t (:foreground ,sky-blue-athens :bold t ))))
   ;; '(hl-line ((t (:inherit nil :extend t))))

   '(italic ((t (:slant italic :underline nil))))

   ;; Matches
   '(match ((t nil)))
   '(show-paren-match ((t (:inverse-video t))))

   ;; ;; Clickable elements
   ;; `(link ((t (:foreground ,warm-grey :underline t ))))
   ;; `(custom-button ((t (:foreground ,warm-grey :box (:line-width 3 :style ,warm-grey) :height 0.9 ))))
   ;; `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))
   ;; `(custom-button-mouse ((t (:foreground ,bleached-silk :box (:line-width 3 :style ,bleached-silk) :height 0.9 ))))

   ;; ;; Org mode
   ;; `(org-code ((t (:foreground ,sky-blue-athens ))))
   ;; `(org-verbatim ((t (:foreground ,olive-green ))))
   ;; `(org-block ((t (:background ,athens-sculpture-3 ))))

   ;; ;; Key cast package
   ;; `(keycast-key ((t (:box nil :foreground ,bleached-silk :background ,warm-grey ))))

   ;; ;; Corfu package
   ;; `(corfu-current ((t (:background ,athens-sculpture-2 ))))
   ;; `(corfu-border ((t (:background ,athens-sculpture-1 ))))
   ;; `(corfu-bar ((t (:background ,tourist-shoes-brown ))))
   )


;;;###autoload
  (and load-file-name
       (boundp 'custom-theme-load-path)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory load-file-name))))

  (provide-theme 'somnus))

;;; somnus-theme.el ends here
