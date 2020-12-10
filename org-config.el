;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")           ; must be set before org loads!

(after! org
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)

  (setq org-startup-folded 'showall)
  (setq org-treat-insert-todo-heading-as-state-change t ; log todo creation
        org-log-into-drawer t                           ; log into LOGBOOK drawer
        org-support-shift-select t                      ; use shift + up down to work like
        org-cycle-separator-lines 1                     ; Don't fold blank lines.
        ;; org-log-done 'time
        ;; org-log-note-previous-state t
        org-ellipsis "⤵")
  ;; (setq evil-respect-visual-line-mode t)       ; This doesn't work, but it can be made to work in
                                                          ; init.el (and in fact it is set there).
  ;; (setq org-default-notes-file (concat org-directory "/v/notes.org"))
  (setq org-clock-persist 'history)               ; Save clock history across emacs sessions.
  (org-clock-persistence-insinuate))

;;; ;;;;;;;;;;;;;;;;  JOURNAL ;;;;;;;;;;;;;;;; ;;;;
(after! org-journal
  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  (setq org-journal-dir (concat org-directory "/journal/"))
  (setq org-journal-carryover-items "") ; protect yourself from org-journal's craziness.
                                        ;  (we're about to replace it with our own craziness)
  (setq org-journal-time-prefix "** @-"
        org-journal-skip-carryover-drawers (list "LOGBOOK"))

;;; This may not be necessary; commenting out for now
;; (if (file-exists-p org-clock-persist-file)
;;   (shell-command (concat "touch " org-clock-persist-file)))

  (setq org-journal-file-format "%F.org")
  (defun org-journal-is-journal ()
    "Determine if file is a journal file."
    (and (buffer-file-name)
         (string-match (org-journal--dir-and-file-format->pattern) (buffer-file-name))))
  )
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

;; From https://github.com/bastibe/org-journal
;; Section on "iCalendar Export" but also enables agenda integration.
;; (setq org-journal-enable-agenda-integration t
;;       org-icalendar-store-UID t
;;       org-icalendar-include-todo "all"
;;       org-icalendar-combined-agenda-file "~/path/to/org-journal.ics")


;;;; Protocols and Servers.
;; org-roam protocol setup is not easy... 
;; https://www.orgroam.com/manual.html#Roam-Protocol
;; (server-start)               ; disabling server start, b/c only one instance of emacs.

(after! org-roam
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
        org-roam-server-network-label-wrap-length 20))

;;;; ;;;;;;;;;;;;;;;;  EYE CANDY  ;;;;;;;;;;;;;;;; ;;;;
;;; Add Review to keywords

;; (setq org-todo-keyword-faces
;;       '(("BETRAYAL" . (:inherit '(font-lock-constant-face) :foreground "dark red" :weight 'bold :width 'extra-condensed))
;;         ("DECIDE" . (:inherit '(bold font-lock-constant-face) :foreground "turquoise2" ))
;;         ("DECIDED" . (:inherit '(bold font-lock-constant-face) :foreground "gold1" :weight 'extra-bold))
;;         ("KILL" . (:inherit '(bold font-lock-constant-face) :foreground "white" :weight 'extra-light :strike-through "#000000"))
;;         ("REVIEW" . (:inherit '(bold font-lock-constant-face) :foreground "dark orchid" :weight 'extra-bold))
;;         ("PUSHED" . (:inherit '(bold font-lock-constant-face) :foreground "wheat3" :weight 'extra-bold))))


(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PROJ(p@)" "REVIEW(r)" "STRT(s)" "WAIT(w@/!)" "HOLD(h!)" "|" "DONE(d!)" "KILL(k!)" "PUSHED(u!)")
          (sequence "DECIDE(E)" "DECIDED(D@!)" "BETRAYAL(L!)" "|" "HONOR(R)" )
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](X)"))
        org-todo-keyword-faces
        '(("BETRAYAL" .   (:weight extra-bold :family "mono" :foreground "dark red" ))
          ("STRT" .       (:weight extra-bold :family "mono" :foreground "rosy brown"))
          ("KILL" .       (:weight extra-bold :family "mono" :foreground "snow" :strike-through "black"))
          ("DECIDE" .     (:weight extra-bold :family "mono" :foreground "DeepSkyBlue2"))
          ("DECIDED" .    (:weight extra-bold :family "mono" :foreground "gold1"))
          ("DONE" .       (:weight extra-bold :family "mono" :foreground "dark slate gray"))
          ("HONOR" .      (:weight extra-bold :family "mono" :foreground "olive-drab"))
          ("WAIT" .       (:weight extra-bold :family "mono" :foreground "slate blue"))
          ("PUSHED" .     (:weight extra-bold :family "mono" :foreground "wheat3"))
          ("REVIEW" .     (:weight extra-bold :family "mono" :foreground "medium orchiyd"))
          ("HOLD" .       (:weight extra-bold :family "mono" :foreground "royal blue"))
          ("TODO" .       (:weight extra-bold :family "mono" :foreground "salmon"))
          ("[X]" .        (:weight extra-bold :family "mono" :foreground "lime green"))        ))

  (setq org-highlight-latex-and-related '(native script entities))
  (set-face-attribute 'org-drawer nil :foreground "#141414")
  (custom-set-faces!
    '((outline-4 outline-5 outline-6)
      :weight normal))
  )


(use-package! org-superstar ; "prettier" bullets
  :after org
  :hook (org-mode . org-superstar-mode)
  :init
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-superstar-remove-leading-stars t
        ;; org-superstar-todo-bullet-alist
        ;;         '(("TODO" . 9744)
        ;;           ("[ ]"  . 9744)
        ;;           ("DONE" . 9745)
        ;;           ("[X]"  . 9745))
                )

  ;; (setq org-hidden-keywords '(title))
  ;; set basic title font
  ;; (set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
  ;; Low levels are unimportant => no scaling
  ;; (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  ;; (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  ;; (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  ;; (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
  ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
  ;; (set-face-attribute 'org-level-3 nil :height 1.2) ;\large
  (set-face-attribute 'org-level-2 nil :height 1.44) ;\Large
  (set-face-attribute 'org-level-1 nil :height 1.728) ;\LARGE
  ;; Only use the first 4 styles and do not cycle.
  (setq org-cycle-level-faces nil)
  ;; (setq org-n-level-faces 4)
  ;; Document Title, (\huge)
  (set-face-attribute 'org-document-title nil
                      :height 2.074
                      :foreground 'unspecified
                      :inherit 'org-level-8)
  ;; (set-face-attribute 'org-superstar-item nil :height 1.2)
  ;; (set-face-attribute 'org-superstar-header-bullet nil :height 1.2)
  ;; (set-face-attribute 'org-superstar-leading nil :height 1.3)
  )

;; (custom-set-faces!
        ;; (org-todo-parent-face :weight extra-bold))
;; (set-face-font 'org-todo-parent-face doom-font)


;;; Custom syntax highlighting in regexp mode
(after! org
  (add-hook 'org-mode-hook
   (lambda () (font-lock-add-keywords nil
   '(
      ;; ("BOOM" .  'org-table-header)
        ("\"[a-zA-Z][^\"]*\\S-\"" . 'highlight-quoted-symbolhr) ; quotes
        ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
      )))))

;;;; The Version with add-to-list instead of direct fontification
;; (after! org
;;  (add-to-list 'org-font-lock-extra-keywords
;;    '(
;;       ;; ("BOOM" .  'org-table-header)
;;       ("\"[a-zA-Z][^\"]*\\S-\"" . 'org-special-keyword) ; quotes
;;       ("\(\([^\)]*\)\)" . 'custom-comment-tag) ; asides are very light
;;     )))

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

(after! org

  ;; ;; from https://stackoverflow.com/a/17492723/13480314
  ;; (defun org-cycle-hide-drawers (state)
  ;;   "Re-hide all drawers after a visibility state change."
  ;;   (when (and (derived-mode-p 'org-mode)
  ;;              (not (memq state '(overview folded contents))))
  ;;     (save-excursion
  ;;       (let* ((globalp (memq state '(contents all)))
  ;;              (beg (if globalp
  ;;                       (point-min)
  ;;                     (point)))
  ;;              (end (if globalp
  ;;                       (point-max)
  ;;                     (if (eq state 'children)
  ;;                         (save-excursion
  ;;                           (outline-next-heading)
  ;;                           (point))
  ;;                       (org-end-of-subtree t)))))
  ;;         (goto-char beg)
  ;;         (while (re-search-forward org-drawer-regexp end t)
  ;;           (save-excursion
  ;;             (beginning-of-line 1)
  ;;             (when (looking-at org-drawer-regexp)
  ;;               (let* ((start (1- (match-beginning 0)))
  ;;                      (limit
  ;;                       (save-excursion
  ;;                         (outline-next-heading)
  ;;                         (point)))
  ;;                      (msg (format
  ;;                            (concat
  ;;                             "org-cycle-hide-drawers:  "
  ;;                             "`:END:`"
  ;;                             " line missing at position %s")
  ;;                            (1+ start))))
  ;;                 (if (re-search-forward "^[ \t]*:END:" limit t)
  ;;                     (outline-flag-region start (point-at-eol) t)
  ;;                   (user-error msg))))))))))


  (setq v-subitems (mapcar #'(lambda ( x ) (list (concat "v" (substring x 0 1))
                                            x 'entry (list 'file (concat "~/org/v/" x)) "* %u %?\n%i\n%a"))
                           (-filter (lambda ( x ) (not (string-prefix-p "." x))) (directory-files "~/org/v"))))
  ;; (-filter (lambda ( x ) (not (string-prefix-p "." x))) (directory-files "~/org/V"))
  ;; (mapcar '(lambda ( x ) (+ x 2)) '(1 2 3 4 5))
  ;; (substring "asdf" 0 1)
  ;; ((lambda (x) x) (substring "asdf" 0 1))
  ;; (-filter (lambda (x) (> x 2)) '(0 1 2 3 4 5 6))

  (setq default-org-capture-templates
        '(("t" "TODO into journal" entry (file+olp org-journal--get-entry-path "TODO-LIST" "Novel")
           "* TODO %?")

          ("n" "Personal notes" entry (file+headline org-journal--get-entry-path "Notes")
           "* REVIEW %u %?\n%i ~ %a" :prepend t)

          ("w" "org-protocol" entry (file+headline "~/org/refile.org" "Protocol-Inbox")
           "* REVIEW %a\n%U\n%i\n" :immediate-finish t)

          ;; ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
          ;;  "* %U %?\n%i\n%a" :prepend t)

          ;; ("p" "Templates for projects")
          ;; ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox")
          ;;  "* TODO %?\n%i\n%a" :prepend t)
          ;; ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox")
          ;;  "* %U %?\n%i\n%a" :prepend t)
          ;; ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased")
          ;;  "* %U %?\n%i\n%a" :prepend t)

          ;; ("o" "Centralized templates for projects")
          ;; ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ;; ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ;; ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)
          ))


  (let ((V '("v" "Vestibule" entry (file "~/org/V.org")
             "* REVIEW %?\n%i\n*Clipboard*:%x\n*Called from:* in %x"))
        ;; (v '("v" "vestibule subentries"))        ; disabld for now, until my tree is better organied by letter.
        )
    (setq org-capture-templates
          ;; (append (list V) (cons v v-subitems) default-org-capture-templates))
          (cons V default-org-capture-templates))))
