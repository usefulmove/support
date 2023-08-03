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
      (mapcar 'number-to-string (number-sequence 1 (string-to-number a)))
      rst)))

; define primitive commands
(defvar cmds nil)

(add-to-list 'cmds `("inv"  . ,(unary-command (lambda (a) (/ 1.0 a)))))
(add-to-list 'cmds `("sqrt" . ,(unary-command 'sqrt)))
(add-to-list 'cmds `("+"    . ,(binary-command '+)))
(add-to-list 'cmds `("-"    . ,(binary-command '-)))
(add-to-list 'cmds `("*"    . ,(binary-command '*)))
(add-to-list 'cmds `("x"    . ,(binary-command '*)))
(add-to-list 'cmds `("/"    . ,(binary-command '/)))
(add-to-list 'cmds `("^"    . ,(binary-command 'expt)))
(add-to-list 'cmds `("dup"  . ,(lambda (stack) (cons (car stack) stack))))
(add-to-list 'cmds `("iota" . ,command-iota))
(add-to-list 'cmds `("swap" . ,command-swap))

; process-op :: string -> [string] -> [stack]
(defun process-op (stack op)
  (let ((cmd (assoc op cmds)))
    (if cmd
        (funcall (cdr cmd) stack)
        (cons op stack))))  ; op is not command, add to stack

; foldl :: (U -> T -> U) -> U -> [T] -> U
(defun foldl (f acc lst)
  (if (null lst)
      acc
      (foldl f (funcall f acc (car lst)) (cdr lst))))

; evaluate-ops :: string -> [string] -> [string]
(defun evaluate-ops (ops stack)
  (foldl 'process-op stack ops))

(defun evaluate-sexp (s-exp)
  (let ((result (evaluate-ops (split-string s-exp) '())))
    (message "%s" result)))

(defun comp ()
  "Evaluate RPN expression"
  (interactive)
  (let ((sexp (read-string "Enter expression: ")))
    (evaluate-sexp sexp)))
