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
 lsp-idle-delay 0.500)
(set-face-attribute 'region nil :background "#fdff00")

;; (load-theme 'zenburn t)

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

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

;; super-save - https://github.com/bbatsov/super-save
(use-package! super-save
  :config
  (super-save-mode +1))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-ocean t))

;; zoom - https://github.com/cyrus-and/zoom
(use-package! zoom
  :config
  (zoom-mode))

(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

;; rvm
(add-hook! enh-ruby-mode
  (rvm-activate-corresponding-ruby))

;; lsp
(after! lsp-mode
  (map! :ne "SPC m d" #'lsp-find-definition)
  (map! :ne "SPC m R" #'lsp-rename))

;; crystal-mode
(add-hook! crystal-mode
  (add-hook 'before-save-hook #'crystal-tool-format nil t))

;; js2-mode
(add-hook! js2-mode
  (setq js2-basic-offset 2 js-indent-level 2))

;; cursor
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

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
   '("9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "73320ccc14ab4987fe2e97cfd810b33a1f4a115f5f056c482c3d38a4429e1705" "78c01e1b7f3dc9e47bdd48f74dc98dc1a345c291f83b68ac8a1b40191f24d658" "620b9018d9504f79344c8ef3983ea4e83d209b46920f425240889d582be35881" "a4395e069de3314001de4e139d6a3b1a83dcf9c3fdc68ee7b13eef6c2cba4ae3" "7236acec527d58086ad2f1be6a904facc9ca8bf81ed1c19098012d596201b3f1" "e58e0bd0ca1f1a8c1662aeb17c92b7fb49ed564aced96435c64df608ee6ced6d" "0769aa1641a0dcd5043e37ab22c401da838ff30b015da104c87cce1573d4c3ef" "8f54cfa3f010d83d782fbcdc3a34cdc9dfe23c8515d87ba22d410c033160ad7e" "98db748f133d9bb82adf38f8ae7834eefa9eefd6f7ea30909213164e1aa36df6" "5291f0f582034afcc93ba4ff66a1c3d598c2c8d67279acf0f03834cb50760f3f" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "624f3b1e86a81d1873b93edc3cce0947f2042bfeebecc480b393ff1e0aa4abfd" default))
 '(fci-rule-color "#383838")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(package-selected-packages
   '(back-button kaolin-themes org-plus-contrib flycheck-grammarly grammarly))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#191D26")
 '(pos-tip-foreground-color "#d4d4d6")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
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
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
