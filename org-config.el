;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")           ; must be set before org loads!

(require 'org-habit)
(add-to-list 'org-modules 'org-habit t)

(setq org-startup-folded 'showall)

(setq org-treat-insert-todo-heading-as-state-change t) ; log todo creation
(setq org-log-into-drawer t)                           ; log into LOGBOOK drawer
(setq org-support-shift-select t)                      ; use shift + up down to work like
(setq org-ellipsis "⤵")
;; (setq evil-respect-visual-line-mode t)       ; This doesn't work, but it can be made to work in
                                                ; init.el (and in fact it is set there).

;;;; ;;;;;;;;;;;;;;;;  JOURNAL ;;;;;;;;;;;;;;;; ;;;;
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
;; (setq org-default-notes-file (concat org-directory "/v/notes.org"))
(setq org-journal-dir (concat org-directory "/journal/"))

;;; I started with these, but I no longer believe
;; (setq org-tag-alist '( ("pdg" . ?p)
;;                        ("org" . ?o) ))

;; This block taken from
;; (defun my-old-carryover (old_carryover)
;;   (save-excursion
;;     (let ((matcher (cdr (org-make-tags-matcher org-journal-carryover-items))))
;;       (dolist (entry (reverse old_carryover))
;;         (save-restriction
;;           (narrow-to-region (car entry) (cadr entry))
;;           (goto-char (point-min))
;;           (org-scan-tags '(lambda ()
;;                             (org-set-tags ":carried:"))
;;                          matcher org--matcher-tags-todo-only))))))
;; (setq org-journal-handle-old-carryover 'my-old-carryover)

;;;; Protocols and Servers.
;; org-roam protocol setup is not easy... 
;; https://www.orgroam.com/manual.html#Roam-Protocol
;; (server-start)               ; disabling server start, b/c only one instance of emacs.
(require 'org-protocol)
(require 'org-roam-protocol)
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-export-inline-images t
      org-roam-server-serve-files nil
      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
      org-roam-server-network-poll t
      org-roam-server-network-arrows nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20)

;;; To save the clock history across Emacs sessions, use
;;; This may not be necessary; commenting out for now
;; (if (file-exists-p org-clock-persist-file)
;;   (shell-command (concat "touch " org-clock-persist-file)))

(setq org-journal-file-format "%F.org")

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; From https://github.com/bastibe/org-journal
;; Section on "iCalendar Export" but also enables agenda integration.
;; (setq org-journal-enable-agenda-integration t
;;       org-icalendar-store-UID t
;;       org-icalendar-include-todo "all"
;;       org-icalendar-combined-agenda-file "~/path/to/org-journal.ics")

;;;; ;;;;;;;;;;;;;;;;  EYE CANDY  ;;;;;;;;;;;;;;;; ;;;;
;;; Add Review to keywords
(after! org (setq org-todo-keywords
                  '((sequence "TODO(t)" "PROJ(p)" "REVIEW(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)")
                   (sequence "[ ](T)A" "[-](S)" "[?](W)" "|" "[X](D)")) ))
;;; Custom syntax highlighting in regexp mode
(add-hook 'org-mode-hook
   (lambda () (font-lock-add-keywords nil
   '(
      ;; ("BOOM" .  'org-table-header)
        ("\"[a-zA-Z][^\"]*\\S-\"" . 'org-special-keyword) ; quotes
        ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
      )))
   )

;;;; The Version with add-to-list instead of direct fontification
;; (add-hook 'org-mode-hook
;;    (lambda () (add-to-list 'org-font-lock-extra-keywords
;;    '(
;;       ;; ("BOOM" .  'org-table-header)
;;       ("\"[a-zA-Z][^\"]*\\S-\"" . 'org-special-keyword) ; quotes
;;       ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
;;     ))))

;;;; The direct version
;; (font-lock-add-keywords 'org-mode '(
;;     ;; ("BOOM" .  'org-table-header)
;;     ("\"[^\"]*\"" . 'org-special-keyword) ; quotes
;;     ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
;; ))


;;;; ;;;; Create capture templates for everything in ~/org/V ;;;; ;;;;
;; SOME SCRATCH
;; (defun make-capture-template (filename)
;;   "Make a capture template out of file name (for us in each V/)"
;;   (list (concat "v" (substring filename 0 1))
;;             filename 'entry (list 'file (concat "~/org/V/" filename)) "* %u %?\n%i\n%a"))
;;
;; (make-capture-template "yo.txt")
;; (setq v-subitems (mapcar 'make-capture-template
;;         (-filter (lambda ( x ) (not (string-prefix-p "." x))) (directory-files "~/org/V"))))

(setq v-subitems (mapcar #'(lambda ( x ) (list (concat "v" (substring x 0 1))
            x 'entry (list 'file (concat "~/org/v/" x)) "* %u %?\n%i\n%a"))
        (-filter (lambda ( x ) (not (string-prefix-p "." x))) (directory-files "~/org/v"))))
;; (-filter (lambda ( x ) (not (string-prefix-p "." x))) (directory-files "~/org/V"))
;; (mapcar '(lambda ( x ) (+ x 2)) '(1 2 3 4 5))
;; (substring "asdf" 0 1)
;; ((lambda (x) x) (substring "asdf" 0 1))
;; (-filter (lambda (x) (> x 2)) '(0 1 2 3 4 5 6))

(setq default-org-capture-templates
      '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox")
         "* [ ] %?\n%i\n%a" :prepend t)

        ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t)

        ("w" "org-protocol" entry (file+headline "~/org/refile.org" "Protocol-Inbox")
          "* TODO Review %a\n%U\n%i\n" :immediate-finish t)

        ;; ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
        ;;  "* %U %?\n%i\n%a" :prepend t)

        ("p" "Templates for projects")
           ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox")
            "* TODO %?\n%i\n%a" :prepend t)
           ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox")
            "* %U %?\n%i\n%a" :prepend t)
           ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased")
            "* %U %?\n%i\n%a" :prepend t)

        ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))


(let ((V '("V" "Vestibule" entry (file "~/org/V.org")
           "* REVIEW %?\n%i\n*Clipboard*:%x\n*Called from:* in %x"))
     ;; (v '("v" "vestibule subentries"))        ; disabld for now, until my tree is better organied by letter.
     )
  (setq org-capture-templates
       ;; (append (list V) (cons v v-subitems) default-org-capture-templates))
       (cons V default-org-capture-templates))
  )
