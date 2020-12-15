;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! super-save)
(package! curly :recipe (:host github :repo "veelenga/curly.el"))
(package! rvm)
(package! zenburn-theme)
(package! ameba)
(package! evil-terminal-cursor-changer)
(disable-packages! evil-snipe)
(disable-packages! lsp-ui)
(disable-packages! company-lsp)
