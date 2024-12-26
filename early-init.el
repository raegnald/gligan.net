(setq gc-cons-threshold (* 32 1024 1024)
      gc-cons-percentage 0.5)

(setq read-process-output-max (* 3 1024 1024))

(defconst gligan/light-theme 'ellas)
(defconst gligan/dark-theme 'somnus)

(defconst gligan/fixed-pitch-font "Iosevka Comfy Motion")
(defconst gligan/variable-pitch-font
  (if (eq system-type 'darwin)
      "Inter"
    "Iosevka Comfy Motion Duo"))

(defconst gligan/fixed-pitch-font-size 130)
(defconst gligan/variable-pitch-font-size 160)

(defconst gligan/scratch-buffer-mode 'c-mode)

(defconst gligan/org-documents-folder "~/Documents/Org/")
(defconst gligan/org-roam-folder
  (concat gligan/org-documents-folder "Notes/"))

(defconst gligan/c-compiler
  (if (eq system-type 'darwin) "clang" "gcc"))

(setq user-full-name "Ronaldo Gligan"
      user-mail-address "ronaldogligan@outlook.com")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq initial-major-mode gligan/scratch-buffer-mode)

(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(when (eq system-type 'darwin)
  (setq mac-command-modifier       'meta
        mac-option-modifier        'alt
        mac-right-option-modifier  nil
        ns-function-modifier       'hyper
        mac-pass-control-to-system nil))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq use-short-answers t)

(setq backup-directory-alist
      `("." . ,(expand-file-name "backup" user-emacs-directory)))
(setq auto-save-default t
      create-lockfiles nil
      make-backup-files nil)
(setq kill-buffer-delete-autosave-files t)

(setq resize-mini-windows 'grow-only)

(setq custom-safe-themes t)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar  . t)))
