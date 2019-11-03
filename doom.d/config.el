;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(super-save-mode +1)

(setq
 display-line-numbers-type 'relative
 projectile-project-search-path '("~/Dev/repos"))

;; Keybindings to toggle prev/next buffers
(map! :ne "SPC e" #'switch-to-prev-buffer)
(map! :ne "SPC r" #'switch-to-next-buffer)

;; veelenga/curly.el
(require 'curly)
(defun copy-project-filepath () (interactive) (curly-copy-loc "f"))
(defun copy-project-filepath-and-line () (interactive) (curly-copy-loc "f:l"))

(map! :ne "SPC f RET")
(map! :ne "SPC f RET" 'curly-copy-loc)
(map! :ne "SPC f '" 'copy-project-filepath)
(map! :ne "SPC f ;" 'copy-project-filepath-and-line)

;; terminal-focus-reporting
(unless (display-graphic-p)
  (terminal-focus-reporting-mode))
