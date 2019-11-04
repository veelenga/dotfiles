;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(super-save-mode +1)

(setq
 doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 18)
 display-line-numbers-type 'relative
 projectile-enable-caching t
 projectile-completion-system 'ivy
 projectile-project-search-path '("~/Dev/repos"))

(map! :ne "M-/" #'comment-or-uncomment-region)

;; veelenga/curly.el
(use-package! curly
  :config
  (defun copy-project-filepath () (interactive) (curly-copy-loc "f"))
  (defun copy-project-filepath-and-line () (interactive) (curly-copy-loc "f:l"))

  (map! :ne "SPC f RET")
  (map! :ne "SPC f RET" 'curly-copy-loc)
  (map! :ne "SPC f '" 'copy-project-filepath)
  (map! :ne "SPC f ;" 'copy-project-filepath-and-line))

;; terminal-focus-reporting
(unless (display-graphic-p)
  (terminal-focus-reporting-mode))

;; rvm
(add-hook! enh-ruby-mode
  (rvm-activate-corresponding-ruby))
