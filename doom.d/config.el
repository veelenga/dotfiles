;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq
 doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 16)
 display-line-numbers-type 'relative
 projectile-enable-caching t
 projectile-completion-system 'ivy
 projectile-project-search-path '("~/Dev/repos"))

(load-theme 'wombat t)

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
  ;; (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
  ;; (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))
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
(after! lsp-ui
  (map! :ne "SPC m d" #'lsp-find-definition)
  (map! :ne "SPC m R" #'lsp-rename)
  (map! :ne "SPC m m" #'lsp-ui-imenu)
  (setq
    lsp-ui-flycheck-enable nil
    lsp-ui-sideline-show-diagnostics nil
    lsp-ui-sideline-update-mode 'line
    lsp-ui-sideline-show-code-actions nil
    lsp-ui-doc-include-signature nil)
  (flycheck-add-next-checker 'javascript-eslint))
