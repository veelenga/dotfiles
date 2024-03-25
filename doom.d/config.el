;;; .doom.d/config.el -*- lexical-binding: t; -*-

(defalias 'forward-evil-word 'forward-evil-symbol)

(setq
 doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 16)
 display-line-numbers-type 'relative
 projectile-enable-caching t
 projectile-completion-system 'ivy
 projectile-project-search-path '("~/Dev/repos")
 web-mode-enable-auto-closing t
 gc-cons-threshold 100000000
 read-process-output-max (* 1024 1024)
 lsp-restart 'auto-restart
 lsp-file-watch-threshold 3000
 lsp-idle-delay 0.500)

;; https://www.nordtheme.com/
(load-theme 'doom-nord t)
(set-face-attribute 'region nil :background "#EBCB8B")
(set-face-foreground 'line-number "#2E3440")
(set-face-background 'line-number "#4C566A")
(set-face-foreground 'line-number-current-line "#4C566A")
(set-face-background 'line-number-current-line "#EBCB8B")
(set-face-attribute 'region nil :background "#666")

(global-so-long-mode nil)
(setq-default show-trailing-whitespace t)
(global-set-key [escape] 'minibuffer-keyboard-quit)

;; https://github.com/veelenga/terminal-focus-reporting.el
(add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

;; Remapping built-in bindings
(map! :leader
  :desc "M-x" "SPC" #'execute-extended-command ;; Default: "SPC :"
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

  (map! :ne "C-x s" '(lambda () (interactive) (windmove-emacs-or-tmux "up"  "tmux last-window")))

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

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c l")
    (require 'llm-ollama)
  (setopt ellama-provider
    (make-llm-ollama
     :chat-model "llama2:13b"
     :embedding-model "llama2:13b"))
  )

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :bind (("C-c C-r" . 'ivy-resume)
    ("C-x b" . 'ivy-switch-buffer))
  :config (setq ivy-use-virtual-buffers t
    ivy-use-selectable-prompt t
    ivy-display-style 'fancy
    ivy-count-format "(%d/%d) "
    ivy-wrap t)
  (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)
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

;; rvm
(add-hook! enh-ruby-mode (rvm-activate-corresponding-ruby))

;; autoformat in ruby mode
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)

;; crystal-mode
(add-hook! crystal-mode (add-hook 'before-save-hook #'crystal-tool-format nil t))

;; enable prettier-mode
(add-hook 'js2-mode-hook (lambda() (prettier-mode)))
(add-hook 'typescript-tsx-mode-hook (lambda() (prettier-mode)))
(add-hook 'typescript-mode-hook (lambda() (prettier-mode)))

;; go-mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook
  (lambda ()
    (add-hook 'after-save-hook 'gofmt-before-save)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)
    (setq tab-width 4)
    (setq indent-tabs-mode 1)))
