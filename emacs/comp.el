;;; comp.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Robert Duane Edmonds
;;
;; Author: Duane Edmonds
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 01, 2023
;; Modified: August 01, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/dedmonds/comp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(provide 'comp)

(defun unary-command (f)
  (lambda (stack) (cons (number-to-string (funcall f (string-to-number (car stack))))
                        (cdr stack))))

(defun binary-command (f)
  (lambda (stack) (cons (number-to-string (funcall f (string-to-number (cadr stack))
                                               (string-to-number (car stack))))
                        (cddr stack))))

(defun command-swap (stack)
  (let ((a (car stack))
        (b (cadr stack))
        (rst (cddr stack)))
    (append (list b a) rst)))

(defun command-iota (stack)
  (let ((a (car stack))
        (rst (cdr stack)))
    (append
     (mapcar 'number-to-string (number-sequence 1 (1+ (string-to-number a))))
     rst)))

(setq cmds
      '(("inv"  . (unary-command (lambda (a) (/ 1.0 a))))
        ("sqrt" . (unary-command 'sqrt))
        ("+"    . (binary-command '+))
        ("-"    . (binary-command '-))
        ("*"    . (binary-command '*))
        ("x"    . (binary-command '*))  ; helpful on command line ("*" has to be escaped)
        ("/"    . (binary-command '/))
        ("^"    . (binary-command 'expt))
        ("dup"  . (lambda (stack) (cons (car stack) stack)))
        ("iota" . command-iota)
        ("swap" . command-swap)))

(defun process-op (op stack)
  (let ((cmd (assoc op cmds)))
    (if cmd
        (funcall (cdr cmd) stack)
      (cons op stack))))  ; op is not command, add to stack

(defun evaluate-ops (ops stack)
  (reduce 'process-op ops :initial-value stack))

(defun evaluate-sexp (s-exp)
  (let ((res (reverse (evaluate-ops (split-string s-exp) '()))))
    (message "%s" res)))

(defun comp ()
  "Evaluate S-expression"
  (interactive)
  (let ((sexp (read-string "Enter S-expression: ")))
    (evaluate-sexp sexp)))
