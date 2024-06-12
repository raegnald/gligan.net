;;; monokate-theme.el-theme.el --- monokate-theme.el
;;; Version: 1.0
;;; Commentary:
;;; A simple port of the Monokate theme from Sublime Text
;;; Code:

(let ((main-bg     "#1e1e1e")           ; Background colours
      (main-fg     "#b0b0b0")           ; Foreground colours for text
      (white-fg    "#f6f6f5")           ; "Bleached silk"

      (grey-1      "#2e2e2e")           ; Neutral grey colours
      (grey-2      "#3e3e3e")
      (grey-3      "#5e5e5e")

      (rosin           "#3b3a32")
      (warm-grey       "#59574c")
      (sylvan          "#97937f")
      (string-green    "#b8bb26")
      (lemonade        "#ffe792")
      (plague-brown    "#ddb700")
      (autumn-yellow   "#e99500")
      (orange-popsicle "#fe8019")
      (french-pink     "#fc7f6d")
      (tutuji-pink     "#e95293")
      (violet          "#ac58ff")
      (pastel-blue     "#b7c6da")
      (blue-jeans      "#5bc0eb")
      (pomegranate     "#ba3b46")
      )

  (deftheme monokate "A simple port of the Colorsublime Monokate theme with modifications")

  (custom-theme-set-faces 'monokate
                          `(default ((t (:foreground ,main-fg :background ,main-bg ))))
                          `(cursor ((t (:background ,french-pink ))))

                          ;; `(ansi-color-black ((t (:foreground ,zenburn-bg :background ,zenburn-bg-1))))
                          `(ansi-color-red ((t (:foreground ,pomegranate :background ,pomegranate))))
                          `(ansi-color-green ((t (:foreground ,string-green :background ,string-green))))
                          `(ansi-color-yellow ((t (:foreground ,lemonade :background ,lemonade))))
                          `(ansi-color-blue ((t (:foreground ,blue-jeans :background ,blue-jeans))))
                          `(ansi-color-magenta ((t (:foreground ,tutuji-pink :background ,tutuji-pink))))
                          `(ansi-color-cyan ((t (:foreground ,pastel-blue :background ,pastel-blue))))
                          `(ansi-color-white ((t (:foreground ,white-fg :background ,white-fg))))

                          `(success ((t (:foreground ,string-green :weight bold))))
                          `(warning ((t (:foreground ,orange-popsicle :weight bold))))

                          `(line-number ((t (:inherit default :foreground ,warm-grey ))))
                          `(line-number-current-line ((t (:inherit default :foreground ,sylvan ))))

                          `(highlight ((t (:background ,grey-1))))
                          `(secondary-selection ((t (:background ,grey-3 ))))

                          `(fringe ((t (:background ,main-bg))))

                          `(mode-line ((t (:foreground ,main-fg :background ,grey-1 ))))
                          `(mode-line-inactive ((t (:background ,main-bg ))))
                          `(mode-line-buffer-id ((t (:bold t :foreground ,lemonade ))))

                          `(vertical-border ((t (:foreground ,grey-2 ))))

                          `(region ((t (:background ,rosin ))))

                          `(font-lock-builtin-face ((t (:foreground ,orange-popsicle ))))
                          `(font-lock-comment-face ((t (:foreground ,sylvan :slant italic ))))
                          `(font-lock-comment-delimiter-face ((t (:foreground ,warm-grey :slant italic ))))
                          `(font-lock-doc-face ((t (:foreground ,pastel-blue ))))
                          `(font-lock-function-name-face ((t (:foreground ,autumn-yellow ))))
                          `(font-lock-keyword-face ((t (:foreground ,tutuji-pink ))))
                          `(font-lock-string-face ((t (:foreground ,string-green ))))
                          `(font-lock-escape-face ((t (:foreground ,plague-brown ))))
                          `(font-lock-number-face ((t (:foreground ,pastel-blue ))))
                          `(font-lock-type-face ((t (:foreground ,white-fg ))))
                          `(font-lock-constant-face ((t (:foreground ,violet ))))
                          `(font-lock-variable-name-face ((t (:foreground ,pastel-blue ))))
                          `(font-lock-warning-face ((t (:foreground ,plague-brown :bold t ))))

                          `(minibuffer-prompt ((t (:foreground ,violet :bold t ))))
                          '(hl-line ((t (:inherit nil :extend t))))

                          '(italic ((t (:slant italic :underline nil))))
                          
                          ;; Matches
                          '(match ((t nil)))
                          '(show-paren-match ((t (:inverse-video t))))

                          ;; Clickable elements
                          `(link ((t (:foreground ,sylvan :underline t ))))
                          `(custom-button ((t (:foreground ,sylvan :box (:line-width 3 :style ,sylvan) :height 0.9 ))))
                          `(custom-button-mouse ((t (:foreground ,white-fg :box (:line-width 3 :style ,white-fg) :height 0.9 ))))
                          `(custom-button-mouse ((t (:foreground ,white-fg :box (:line-width 3 :style ,white-fg) :height 0.9 ))))
                          
                          ;; Org mode
                          `(org-code ((t (:foreground ,autumn-yellow ))))
                          `(org-verbatim ((t (:foreground ,sylvan ))))

                          ;; Key cast package
                          `(keycast-key ((t (:box nil :foreground ,white-fg :background ,grey-3 ))))
                          )

;;;###autoload
  (and load-file-name
       (boundp 'custom-theme-load-path)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory load-file-name))))

  (provide-theme 'monokate))

;;; monokate-theme.el ends here
