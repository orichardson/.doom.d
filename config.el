;;; $DOOMDIR/config.el -*- lexical-binding: t; -*e

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Oliver Richardson"
      user-mail-address "oli@cs.cornell.edu")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;
(setq doom-theme 'oli-one
      oli-one-brighter-modeline t
     ;; doom-one-colorful-headers nil ; default
     )   ; great
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-vibrant)
;; (setq doom-theme 'doom-manegarm)
;; (setq doom-theme 'doom-henna)  ;
;; (setq doom-theme 'doom-nord-light)
;; (setq doom-theme 'doom-henna)  ;
;; (setq doom-theme 'doom-monokai-pro)

(after! ws-butler
  (setq ws-butler-keep-whitespace-before-point t) )

(setq doom-font (font-spec :name "DaddyTimeMono" :size 14 )
      ;; doom-serif-font (font-spec :name "FiraCode" :size 14 )
      ;; doom-variable-pitch-font (font-spec :name "CormorantGaramond" :size 19 :weight '
      doom-variable-pitch-font (font-spec :family "Vollkorn" :size 14 :weight 'regular)
      ;; doom-variable-pitch-font (font-spec :family "FiraCode" :size 14)
      display-line-numbers-type t
      ;; Some changes that seemed good from https://tecosaur.github.io/emacs-config/config.html
      undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      ;; auto-save-default t
      global-auto-revert-mode t      )

;; https://github.com/hlissner/doom-emacs/issues/2225
;; (use-package zoom
;;   :hook (doom-first-input . zoom-mode)
;;   :config
;;   (setq zoom-size '(0.7 . 0.7)
;;         zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu)
;;         zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*")
;;         zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")
;;         zoom-ignore-predicates (list (lambda () (> (count-lines (point-min) (point-max)) 20)))))


(setq projectile-sort-order 'recently-active) ; this might be expensive, but..
(doom/set-frame-opacity 0.97)

 ;;; Latex
(setq TeX-electric-sub-and-superscript nil
      font-latex-script-display '((raise -0.1) raise 0.1))

(custom-set-faces!
  '(aw-leading-char-face
    :foreground "white" :background "red"
    :weight bold :height 2.5 :box (:line-width 10 :color "red")))

(after! hl-todo
  (setq hl-todo-keyword-faces
        (append hl-todo-keyword-faces   ; start with the defaults (they're nice!)
          '(("DONE" success
            ("OOPS" error bold)
            ("NOTE" all-the-icons-blue bold)
            ("NEVERMIND" all-the-icons-pink bold)
          )))))
(setq-default line-spacing 0.13)

;; (after! org (load! "org-config.el"))
(load! "org-config.el")                 ; Now I'm thinking I want to do the 'after! inside, for flexibility.
(load! "keys.el")

;; (setq centaur-tabs-style "chamfer")

;;; Math input mode.
(require 'math-symbol-lists)
(quail-define-package "math" "UTF-8" "Ω" t)
(quail-define-rules ; add whatever extra rules you want to define here...
 ("\\from"    #X2190)
 ("\\to"      #X2192)
 ("\\lhd"     #X22B2)
 ("\\rhd"     #X22B3)
 ("\\unlhd"   #X22B4)
 ("\\unrhd"   #X22B5))
(mapc (lambda (x)
        (if (cddr x)
            (quail-defrule (cadr x) (car (cddr x)))))
      (append math-symbol-list-basic math-symbol-list-extended))

;;; Agda Mode.
(setq agda2-backend "GHC")
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

;; From https://github.com/bastibe/org-journal
;; Section on "iCalendar Export" but also enables agenda integration.
;; (setq org-journal-enable-agenda-integration t
;;       org-icalendar-store-UID t
;;       org-icalendar-include-todo "all"
;;       org-icalendar-combined-agenda-file "~/path/to/org-journal.ics")
;;
;; ;; VESTIGAL CODE COPIED TO INTEGRATE CAPTURE TEMPLATES AND INTEGRATE AGENDA.
;; Maybe the latter will actually end up being useful
;; (setq org-journal-enable-agenda-integration t)
;; (defun org-journal-find-location ()
;;   ;; Open today's journal, but specify a non-nil prefix argument in order to
;;   ;; inhibit inserting the heading; org-capture will insert the heading.
;;   (org-journal-new-entry t)
;;   ;; Position point on the journal's top-level heading so that org-capture will add the new entry as a child entry.
;;   (goto-char (point-min)))
;; (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               ;; "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
