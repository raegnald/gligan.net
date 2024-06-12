(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.5)

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Title
(setq frame-title-format "%b - Emacs")

;; Minimal window without buttons or scrollbars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Maximised at start up
;; (add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; Fullscreen at start up
;; (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; No window decorations -- good for tiling window managers or minimal
;; configs
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Declare all themes as safe. That way, Emacs won't ask the user if
;; it's okay to load a theme
(setq custom-safe-themes t)

(setq gligan/dark-theme  'somnus)       ; 'monokate, 'somnus
(setq gligan/light-theme 'ellas)

(defun gligan/toggle-theme ()
  "Switch between Gligan's dark and light themes"
  (interactive)
  (disable-theme gligan/current-theme)
  (setq next-theme (if (equal gligan/current-theme gligan/light-theme)
                       gligan/dark-theme
                     gligan/light-theme))
  (setq gligan/current-theme next-theme)
  (load-theme next-theme t))

(global-set-key (kbd "M-g t t") 'gligan/toggle-theme)

;; We start with the dark theme
(setq gligan/current-theme gligan/dark-theme)

;; Between these two hours, when opening Emacs, the light theme should be the default
(defvar gligan/light-theme-hour-start (string-to-number (getenv "GLIGAN_DAY_START")))
(defvar gligan/light-theme-hour-end   (string-to-number (getenv "GLIGAN_DAY_END")))

;; Switch to light theme if we're between the bounds defined above
(let ((hour (string-to-number (substring (current-time-string) 11 13))))  
  (if (and
       (<= gligan/light-theme-hour-start hour)
       (>  gligan/light-theme-hour-end   hour))
    (gligan/toggle-theme)
    (load-theme gligan/current-theme t)))
