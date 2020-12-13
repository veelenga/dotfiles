;;; .doom.d/config.el -*- lexical-binding: t; -*-

(defalias 'forward-evil-word 'forward-evil-symbol)

(setq
 doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 16)
 display-line-numbers-type 'relative
 projectile-enable-caching t
 projectile-completion-system 'ivy
 projectile-project-search-path '("~/Dev/repos")
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024)
 lsp-prefer-capf t
 lsp-auto-configure nil
 lsp-idle-delay 0.500)
(set-face-attribute 'region nil :background "#fdff00")

(load-theme 'zenburn t)

(defun setup-indent (n)
  (setq c-basic-offset n)
  (setq coffee-tab-width n)
  (setq javascript-indent-level n)
  (setq typescript-indent-level n)
  (setq js-indent-level n)
  (setq js2-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n))

(setup-indent 2)

;; Remapping built-in bindings
(map! :leader
  :desc "M-x" "SPC" #'execute-extended-command ;; Default: "SPC :"

  (:when (featurep! :ui workspaces)
    (:prefix-map ("l" . "workspace") ;; Default: "SPC TAB ..."
      :desc "Switch to 1st workspace"   "1"   #'+workspace/switch-to-0
      :desc "Switch to 2nd workspace"   "2"   #'+workspace/switch-to-1
      :desc "Switch to 3rd workspace"   "3"   #'+workspace/switch-to-2
      :desc "Switch to 4th workspace"   "4"   #'+workspace/switch-to-3
      :desc "Switch to 5th workspace"   "5"   #'+workspace/switch-to-4
      :desc "Switch to 6th workspace"   "6"   #'+workspace/switch-to-5
      :desc "Switch to 7th workspace"   "7"   #'+workspace/switch-to-6
      :desc "Switch to 8th workspace"   "8"   #'+workspace/switch-to-7
      :desc "Switch to 9th workspace"   "9"   #'+workspace/switch-to-8))

  (:prefix-map ("/" . "search")
    :desc "Search project" "/" #'+default/search-project)) ;; Default: "SPC / p"

;; Integration with tmux movement
(unless (display-graphic-p)
  (defun windmove-emacs-or-tmux(dir tmux-cmd)
    (interactive)
    (if (ignore-errors (funcall (intern (concat "windmove-" dir))))
      nil                       ;; Moving within emacs
      (shell-command tmux-cmd)) ;; At edges, send command to tmux
  )

  (map! :ne "C-k" '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux select-pane -U")))
  (map! :ne "C-j" '(lambda () (interactive) (windmove-emacs-or-tmux "down"  "tmux select-pane -D")))
  (map! :ne "C-l" '(lambda () (interactive) (windmove-emacs-or-tmux "right" "tmux select-pane -R")))
  (map! :ne "C-h" '(lambda () (interactive) (windmove-emacs-or-tmux "left"  "tmux select-pane -L")))

  ;; Related config in tmux.conf
  ; is_editor='echo "#{pane_current_command}" | grep -iqE "(^|\/)g?(view|n?vim?)(diff)?$|emacs.*$"'
  ; bind -n C-h if-shell "$is_editor" "send-keys C-h" "select-pane -L"
  ; bind -n C-j if-shell "$is_editor" "send-keys C-j" "select-pane -D"
  ; bind -n C-k if-shell "$is_editor" "send-keys C-k" "select-pane -U"
  ; bind -n C-l if-shell "$is_editor" "send-keys C-l" "select-pane -R"

  ;; Mouse scroll support
  (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
)

;; ivy - https://github.com/abo-abo/swiper
(after! ivy
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command))

;; curly - https://github.com/veelenga/curly.el
(use-package! curly
  :config
  (defun copy-project-filepath () (interactive) (curly-copy-loc "f"))
  (defun copy-project-filepath-and-line () (interactive) (curly-copy-loc "f:l"))

  (map! :ne "SPC f RET" 'curly-copy-loc)
  (map! :ne "SPC f '" 'copy-project-filepath)
  (map! :ne "SPC f ;" 'copy-project-filepath-and-line))

;; super-save - https://github.com/bbatsov/super-save
(use-package! super-save
  :config
  (super-save-mode +1))

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

;; rvm
(add-hook! enh-ruby-mode
  (rvm-activate-corresponding-ruby))

;; lsp
(after! lsp-mode
  (map! :ne "SPC m d" #'lsp-find-definition)
  (map! :ne "SPC m R" #'lsp-rename))

;; crystal-mode
(after! crystal-mode
  (add-hook 'before-save-hook #'crystal-tool-format))

;; js2-mode
(add-hook! js2-mode
  (setq js2-basic-offset 2 js-indent-level 2))

;; hl-line-mode
(after! hl-line
  (set-face-underline 'highlight nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("624f3b1e86a81d1873b93edc3cce0947f2042bfeebecc480b393ff1e0aa4abfd" default)))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages (quote (org-plus-contrib flycheck-grammarly grammarly)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
