;;; magic.el --- Magic 8-ball -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Robert Duane Edmonds
;;
;; Author: Duane <dedmonds@sb>
;; Maintainer: Duane <dedmonds@sb>
;; Created: August 24, 2023
;; Modified: August 24, 2023
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/dedmonds/magic
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description:  Magic 8-ball advice
;;
;;; Code:


(defun magic8 ()
  "Magic 8-ball advice."
  (interactive)
  (let ((magic8-hash (make-hash-table :test 'equal)))
    (puthash 0 "it is certain." magic8-hash)
    (puthash 1 "it is decidedly so." magic8-hash)
    (puthash 2 "without a doubt." magic8-hash)
    (puthash 3 "yes definitely." magic8-hash)
    (puthash 4 "you may rely on it." magic8-hash)
    (puthash 5 "as I see it, yes." magic8-hash)
    (puthash 6 "most likely." magic8-hash)
    (puthash 7 "outlook good." magic8-hash)
    (puthash 8 "yes." magic8-hash)
    (puthash 9 "signs point to yes." magic8-hash)
    (puthash 10 "reply hazy, try again." magic8-hash)
    (puthash 11 "ask again later." magic8-hash)
    (puthash 12 "better not tell you now." magic8-hash)
    (puthash 13 "cannot predict now." magic8-hash)
    (puthash 14 "concentrate and ask again." magic8-hash)
    (puthash 15 "don't count on it." magic8-hash)
    (puthash 16 "my reply is no." magic8-hash)
    (puthash 17 "my sources say no." magic8-hash)
    (puthash 18 "outlook not so good." magic8-hash)
    (puthash 19 "very doubtful." magic8-hash)
      (message (concat "magic8: "(gethash (random (hash-table-count magic8-hash)) magic8-hash)))))


(provide 'magic)
;;; magic.el ends here
