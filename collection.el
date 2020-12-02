;;; collection.el -*- lexical-binding: t; -*-

;; practice writing code
(defun oli-mul (arg1 arg2)
  (* arg1 arg2) )

(defun oli-log (arg1 arg2)
  (interactive)
  (message arg1 arg2))

(oli-mul 3 4)
