;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-

(setq org-directory "~/org/")           ; must be set before org loads!

(after! org
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)

  (setq org-startup-folded 'showall)
  (setq org-treat-insert-todo-heading-as-state-change t ; log todo creation
        org-log-into-drawer t                           ; log into LOGBOOK drawer
        org-support-shift-select t                      ; use shift + up down to work like
        ;; org-cycle-separator-lines 1                     ; Don't fold blank lines.
        org-cycle-separator-lines 2                     ; Actually do fold blank lines.
        ;; org-log-done 'time
        ;; org-log-note-previous-state t
        org-ellipsis "⤵")
  ;; (setq evil-respect-visual-line-mode t)       ; This doesn't work, but it can be made to work in
                                        ; init.el (and in fact it is set there).
;;; This may not be necessary; commenting out for now
;; (if (file-exists-p org-clock-persist-file)
;;   (shell-command (concat "touch " org-clock-persist-file)))
  (setq org-default-notes-file (concat org-directory "/v/notes.org"))
  (setq org-clock-persist 'history)               ; Save clock history across emacs sessions.
  (org-clock-persistence-insinuate)

  (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  ;; add to refile targets immediately
  (defun +org/opened-buffer-files ()
    "Return the list of files currently opened in emacs"
    (delq nil
          (mapcar (lambda (x)
                    (if (and (buffer-file-name x)
                             (string-match "\\.org$"
                                           (buffer-file-name x)))
                        (buffer-file-name x)))
                  (buffer-list))))

  (setq org-refile-targets '((+org/opened-buffer-files :maxlevel . 5)))
  (setq org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with helm/ivy
  ;; (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm) )

;;; ;;;;;;;;;;;;;;;;  JOURNAL ;;;;;;;;;;;;;;;; ;;;;
; adapted from  $DOOM-EMACS/modules/lang/org/contrib/journal.el
(use-package! org-journal
  :defer t
  :init
  ;; HACK `org-journal' adds a `magic-mode-alist' entry for detecting journal
  ;;      files, but this causes us lazy loaders a big problem: an unacceptable
  ;;      delay on the first file the user opens, because calling the autoloaded
  ;;      `org-journal-is-journal' pulls all of `org' with it. So, we replace it
  ;;      with our own, extra layer of heuristics.
  (add-to-list 'magic-mode-alist '(+org-journal-p . org-journal-mode))

  (defun +org-journal-p ()
    "Wrapper around `org-journal-is-journal' to lazy load `org-journal'."
    (when-let (buffer-file-name (buffer-file-name (buffer-base-buffer)))
      (if (or (featurep 'org-journal)
              (and (file-in-directory-p
                    buffer-file-name (expand-file-name org-journal-dir org-directory))
                   (require 'org-journal nil t)))
          ;; the altered content of org-journal-is-journal
          ;; (org-journal-is-journal)
          (and (buffer-file-name)
               (string-match (org-journal--dir-and-file-format->pattern) (buffer-file-name))))
      ))

  ;; `org-journal-dir' defaults to "~/Documents/journal/", which is an odd
  ;; default, so we change it to {org-directory}/journal (we expand it after
  ;; org-journal is loaded).
  (setq org-journal-dir "journal/"
        org-journal-cache-file (concat doom-cache-dir "org-journal"))

  :config
  ;; Remove the orginal journal file detector and rely on `+org-journal-p'
  ;; instead, to avoid loading org-journal until the last possible moment.
  (setq magic-mode-alist (assq-delete-all 'org-journal-is-journal magic-mode-alist))

  ;; `org-journal' can't deal with symlinks, so resolve them here.
  (setq org-journal-dir (expand-file-name org-journal-dir org-directory)
        ;; Doom opts for an "open in a popup or here" strategy as a default.
        ;; Open in "other window" is less predictable, and can replace a window
        ;; we wanted to keep visible.
        org-journal-find-file #'find-file)
  (setq org-journal-carryover-items "") ; protect yourself from org-journal's craziness.

  (setq org-journal-time-prefix "** @-"
        org-journal-date-prefix "* Timeline: "
        org-journal-skip-carryover-drawers (list "LOGBOOK"))
  (setq org-journal-file-format "%F.org"
        org-journal-date-format "%A, %d %b %Y")  )
  ;; Setup carryover to include all configured TODO states. We cannot carry over
  ;; [ ] keywords because `org-journal-carryover-items's syntax cannot correctly
  ;; interpret it as anything other than a date.
  ;; (setq org-journal-carryover-items  "TODO=\"TODO\"|TODO=\"PROJ\"|TODO=\"STRT\"|TODO=\"WAIT\"|TODO=\"HOLD\"")

  (set-popup-rule! "^\\*Org-journal search" :select t :quit t)

  (map! (:map org-journal-mode-map
         :n "]f"  #'org-journal-next-entry
         :n "[f"  #'org-journal-previous-entry
         :n "C-n" #'org-journal-next-entry
         :n "C-p" #'org-journal-previous-entry)
        (:map org-journal-search-mode-map
         "C-n" #'org-journal-search-next
         "C-p" #'org-journal-search-previous)
        :localleader
        (:map org-journal-mode-map
         ;; "c" #'org-journal-new-entry
         ;; "d" #'org-journal-new-date-entry
         "n" #'org-journal-next-entry
         "p" #'org-journal-previous-entry
         (:prefix "s"
          "s" #'org-journal-search
          "f" #'org-journal-search-forever
          "F" #'org-journal-search-future
          "w" #'org-journal-search-calendar-week
          "m" #'org-journal-search-calendar-month
          "y" #'org-journal-search-calendar-year))
        (:map org-journal-search-mode-map
         "n" #'org-journal-search-next
         "p" #'org-journal-search-prev))
                                        ;  (we're about to replace it with our own craziness)
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
        '((sequence "TODO(t)" "PROJ(p)" "REVIEW(r)" "STRT(s)" "WAIT(w!)" "HOLD(h!)" "PUSHED(u!)" "|" "DONE(d!)" "KILL(k!)")
          (sequence "TODO(t)" "NOPE(n!)" "|" "DONE(d!)")
          (sequence "DECIDE(E)" "DECIDED(D@!)" "BETRAYAL(L!)" "|" "HONOR(R!)" )
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
          ("HOLD" .       (:weight extra-bold :family "mono" :foreground "turquoise"))
          ("PUSHED" .     (:weight extra-bold :family "mono" :foreground "wheat3"))
          ("REVIEW" .     (:weight extra-bold :family "mono" :foreground "medium orchiyd"))
          ("TODO" .       (:weight extra-bold :family "mono" :foreground "salmon"))
          ("[ ]" .        (:weight extra-bold :family "mono" :foreground "salmon"))
          ("NOPE" .       (:weight extra-bold :family "mono" :foreground "tomato4"))
          ("[X]" .        (:weight extra-bold :family "mono" :foreground "lime green"))        ))

  ;; (setq org-highlight-latex-and-related '(native script entities))
  (set-face-attribute 'org-drawer nil :foreground "#303035")
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
        org-superstar-remove-leading-stars nil
        org-superstar-headline-bullets-list '(9673 10036 10057 10028 10040 10047)
        org-superstar-special-todo-items t
        org-superstar-todo-bullet-alist
                 '(("TODO" . 9723)      ; 9744
                   ("[ ]"  . 11116)
                   ("DONE" . 10004)
                   ("NOPE" . 10007)
                ))

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
  (set-face-attribute 'org-level-2 nil :height 1.25) ;\Large 1.44
  (set-face-attribute 'org-level-1 nil :height 1.5) ;\LARGE 1.728
  ;; Only use the first 4 styles and do not cycle.
  ;; (setq org-cycle-level-faces nil)
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
;;; END SCRATCH
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
        '(
          ("j" "Insert into current journal")
          ("jt" "TODO for today" entry (file+olp org-journal--get-entry-path "TODO-LIST" "Novel")
           "* TODO %?")
          ("jn" "NOTES for today" entry (file+olp org-journal--get-entry-path "TODO-LIST" "Notes")
           "* REVIEW %?")

          ("t" "Global TODO" entry (file+headline "~/org/todo.org" "Inbox")
           "* TODO %?")

          ;; ("n" "Personal notes" entry (file+headline org-journal--get-entry-path "Notes")
          ;;  "* REVIEW %u %?\n%i ~ %a" :prepend t)

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
