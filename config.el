;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "Source Code Pro" :size 13))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(load "~/.doom.d/org-config.el")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq-default line-spacing 0.2)
(load "~/.doom.d/keys.el")

;; (setq centaur-tabs-style "chamfer")

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





;; @OLI: I don't understand this but it is what makes it possible to use math input
;; without tex mode.

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




;; @OLI: And now I want some syntax highlighting for org-mode
(font-lock-add-keywords 'org-mode '(
    ;; ("BOOM" .  'org-table-header)
    ("\"[^\"]*\"" . 'org-special-keyword) ; quotes
    ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
))



;; From https://github.com/bastibe/org-journal
;; Section on "iCalendar Export" but also enables agenda integration.
;; (setq org-journal-enable-agenda-integration t
;;       org-icalendar-store-UID t
;;       org-icalendar-include-todo "all"
;;       org-icalendar-combined-agenda-file "~/path/to/org-journal.ics")

;; ;; @OLI HAVE BEEN MEANING TO DO THIS FOREVER
;; But at least I can copy-paste from org-journal...

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

;; (setq org-capture-templates '(("j" "Journal entry" entry (function org-journal-find-location)
                               ;; "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Research/brz/log.org" "~/Research/brz/agendas/agenda-16jul20.org" "~/Research/Joe/agent-goals/log.org" "~/Research/Joe/agent-goals/future.org" "~/Research/Joe/agent-goals/papers/query/database-relationship.org" "~/org/journal.org" "~/org/zly/EXE.org" "~/org/zly/vestibule.org" "~/org/zly/roaming.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#1E2029")))))
