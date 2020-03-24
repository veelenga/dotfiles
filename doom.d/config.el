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

(load-theme 'zenburn t)

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

;; terminal-focus-reporting - https://github.com/veelenga/terminal-focus-reporting
(unless (display-graphic-p)
  (use-package! terminal-focus-reporting
    :config
    (terminal-focus-reporting-mode)))

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
