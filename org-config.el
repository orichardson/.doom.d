;;; ~/.doom.d/org-config.el -*- lexical-binding: t; -*-


(setq org-directory "~/org/")

(require 'org-habit)
(add-to-list 'org-modules 'org-habit t)

;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)
;; log into LOGBOOK drawer
(setq org-log-into-drawer t)


;; (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-agenda-files (directory-files org-directory 'full "\\.org$"))
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; @OLI: Also adding my org stuff here.
(setq org-tag-alist '(
                      ("pdg" . ?p)
                      ("org" . ?o) ))



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


;; PERSONAL KEY
;; TODO toggle latex completion
;; (map! :map normal-mode)
