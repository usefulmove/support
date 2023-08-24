;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "MonoLisa" :size 16))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord-aurora)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(menu-bar-mode 1)

(use-package! org-superstar
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

(defun magic8 ()
  "Magic 8-ball advice"
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

; custom keybinding layer
(global-set-key (kbd "C-c b") 'term) ; bash terminal
(global-set-key (kbd "C-c t") 'term) ; ( remove when ready . same as above )
(global-set-key (kbd "C-c e") 'eros-eval-last-sexp)
(global-set-key (kbd "C-c o") 'org-open-at-point)

(custom-set-faces
  '(rainbow-delimiters-depth-1-face ((t (:foreground "#e9c687"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "#858585"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "#87ffaf"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "#00c0ff")))))

(setq fancy-splash-image "~/.config/doom/logo-alpha.png")

(load-file "/home/dedmonds/repos/support/emacs/comp.el")

(after! doom-themes
  (setq evil-normal-state-cursor '(box "#fff670")
        evil-insert-state-cursor '(bar "#00c0ff")
        evil-visual-state-cursor '(hollow "#e9c687"))
        (blink-cursor-mode 1))
