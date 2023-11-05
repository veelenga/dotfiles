;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! super-save)
(package! curly :recipe (:host github :repo "veelenga/curly.el"))
(package! rvm)
(package! ameba)
(package! prettier)
(package! rubocopfmt)
(package! graphql-mode)
(package! tree-sitter)
(package! tree-sitter-langs)

(disable-packages! helm-lsp)
(disable-packages! evil-snipe)
(disable-packages! eglot)
