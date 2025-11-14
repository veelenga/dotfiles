;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

;; (package! super-save)
(package! rvm)
(package! ameba)
(package! prettier)
(package! rubocopfmt)
(package! graphql-mode)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! eat
  :recipe (:host codeberg
           :repo "akib/emacs-eat"
           :files ("*.el" "*.c" "*.h" "Makefile")))

(disable-packages! helm-lsp)
(disable-packages! evil-snipe)
(disable-packages! eglot)
