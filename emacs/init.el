;; init.el --- https://gligan.net/emacs/init.el

;;; Commentary:
;; 
;; A configuration for a decent Emacs experience without resorting to
;; external packages.

;;; Code:

;; The looks
(load-theme 'leuven-dark t)
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
