;;; comp.el --- RPN interpreter -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023 Duane Edmonds
;;
;; Author: Duane Edmonds
;; Maintainer: Duane Edmonds <duane.edmonds@gmail.com>
;; Created: August 01, 2023
;; Modified: August 25, 2023
;; Version: 0.0.1
;; Keywords: convenience data tools
;; Homepage: https://github.com/dedmonds/comp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:  RPN interpreter
;;
;;; Code:

; unary-command :: (number -> number) -> ([string] -> [string])
(defun unary-command (f)
  (lambda (stack) (cons (number-to-string (funcall f (string-to-number (car stack))))
                        (cdr stack))))

; binary-command :: (number -> number -> number) -> ([string] -> [string])
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

(defun command-sum (stack)
  (let ((res (cond ((null stack) 0)
                   (t (+ (string-to-number (car stack))
                         (string-to-number (car (command-sum (cdr stack)))))))))
    (list (number-to-string res))))

(defun command-prod (stack)
  (let ((res (cond ((null stack) 1)
                   (t (* (string-to-number (car stack))
                         (string-to-number (car (command-prod (cdr stack)))))))))
    (list (number-to-string res))))


; define primitive commands
(defvar cmds nil)

(add-to-list 'cmds `("abs"  . ,(unary-command 'abs)))
(add-to-list 'cmds `("inv"  . ,(unary-command (lambda (a) (/ 1.0 a)))))
(add-to-list 'cmds `("sqrt" . ,(unary-command 'sqrt)))
(add-to-list 'cmds `("+"    . ,(binary-command '+)))
(add-to-list 'cmds `("-"    . ,(binary-command '-)))
(add-to-list 'cmds `("*"    . ,(binary-command '*)))
(add-to-list 'cmds `("x"    . ,(binary-command '*)))
(add-to-list 'cmds `("/"    . ,(binary-command '/)))
(add-to-list 'cmds `("^"    . ,(binary-command 'expt)))
(add-to-list 'cmds `("mod"  . ,(binary-command 'mod)))
(add-to-list 'cmds `("%"    . ,(binary-command 'mod)))
(add-to-list 'cmds `("dup"  . ,(lambda (stack) (cons (car stack) stack))))
(add-to-list 'cmds `("pi"   . ,(lambda (stack) (cons (number-to-string pi) stack))))
(add-to-list 'cmds `("iota" . ,'command-iota))
(add-to-list 'cmds `("io"   . ,'command-iota))
(add-to-list 'cmds `("swap" . ,'command-swap))
(add-to-list 'cmds `("sum"  . ,'command-sum))
(add-to-list 'cmds `("prod" . ,'command-prod))

; process-op :: string -> [string] -> [string]
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

; evaluate-sexp :: string -> nil (side-effects)
(defun evaluate-sexp (s-exp)
  (let ((result (evaluate-ops (split-string s-exp) '())))
    (kill-new (car result))  ; copy to clipboard
    (message "%s" result)))  ; display as user message

; cmp :: string -> nil (side-effects)
(fset 'cmp #'evaluate-sexp)

; interactive command
(defun comp ()
  "Evaluate RPN expression"
  (interactive)
  (let ((sexp (read-string "Enter expression: ")))
    (evaluate-sexp sexp)))


(provide 'comp)
;; end of comp.el
