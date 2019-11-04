;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! super-save)
(package! terminal-focus-reporting)
(package! curly :recipe (:host github :repo "veelenga/curly.el"))
(package! rvm)
(disable-packages! evil-snipe)
