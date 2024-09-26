(setq gc-cons-threshold (* 32 1024 1024)
      gc-cons-percentage 0.5)

(setq read-process-output-max (* 3 1024 1024))

(setq gligan/light-theme 'ellas
      gligan/dark-theme 'somnus

      gligan/fixed-pitch-font "Iosevka Comfy Motion"
      gligan/variable-pitch-font (if (equal system-type 'darwin)
                                     "Inter"
                                   "Iosevka Comfy Motion Duo")
      gligan/fixed-pitch-font-size 130
      gligan/variable-pitch-font-size 160

      gligan/scratch-buffer-mode 'c-mode

      gligan/org-documents-folder "~/Documents/Org/"
      gligan/org-roam-folder (concat gligan/org-documents-folder "Notes/"))

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
        mac-pass-control-to-system nil))

(when (not (equal system-type 'darwin))
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
