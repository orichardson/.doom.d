;;; ~/.doom.d/keys.el -*- lexical-binding: t; -*-

(map! :map org-mode-map "C-c DEL" 'org-mark-ring-last-goto)
(map! :after org
      (:map evil-org-mode-map
       :n [C-iso-lefttab] #'org-mark-ring-last-goto ; was [C-backtab] before. 
       :n [S-return] #'org-toggle-latex-fragment
       :i [backtab] #'+org/dedent))
;;;;;;;;;;;;;;;;;;
;; The below gives an alternate leader key. If I wanted to replace the leader key,
;; I'd do it from init [[https://github.com/hlissner/doom-emacs/issues/820#issuecomment-468470706]]
(map! :map general-override-mode-map
      :g "M-SPC" #'doom/leader)         ; (used to be just-one space)

(map! :leader "M-SPC" #'+org-capture/open-frame ; Double tap M-SPC for capture
               "j"  #'org-journal-open-current-journal-file    ; (M)-SPC j for journal
               "M-;" #'comment-line)    ; Don't need to take finger off of M key.
                                        ;   Moreover, only difference between commend and
                                        ;   comment-dwim" is the leader key.

(defun insert-a-space-here ()
    "calls (insert \" \")"
    (interactive)
    (insert " "))
(map! :n "S-SPC" #'insert-a-space-here)
(map! :n "RET" #'newline)              ; I really need to be able to break lines without going into insert mode..


;; ;;;;;;;;;;;;;;;;;;  LOG  ;;;;;;;;;;;;; ;;;;
;; DONE BIND      S-SPC to insert a single space in normal mode
;; NEVERMIND      you can always use [M-i] to insert if you want indentation,
;; and you can use [C-q tab] to insert a tab no matter what
;;     (map! :i "TAB" #'self-insert-command)
;; NEVERMIND      <failed attempt at adding to local-leader> I wanted to something like
;;     (map! :map doom-leader-map :g "M-SPC" #'doom/localleader)
;;   but alas it is not possible to alias localleader. See:
;;      - https://github.com/hlissner/doom-emacs/issues/448
;;      - https://github.com/hlissner/doom-emacs/issues/1364#issuecomment-487345393
;;
;;        "Ack, sorry, I forgot it was the localleader you wanted. There is no
;;         `doom/localleader` and it isn't possible to create one (with the
;;         current system), as local leaders keys aren't their own keymaps. In
;;         that case, I'm not aware of any good workaround for this issue. I'll
;;         have to rethink how Doom handles leader/localleader keybinds from the
;;         ground up. It will be a while before I can get to that.
;;
;;;; ;;;;;;;;;;;;;;;;  PENDING ;;;;;;;;;;;;;;;; ;;;;
;; TODO FUNC    <Create-roam-document-in-real-file> instead of in a capture document (with optional goto).
;; TODO FUNC    Takes argument string, parsed for log  (:leader l?)
;;        Example: [SPC l <number> <word>] logs that quantity of that activity at the given time.
;;              Can later extend to more advanced usage if I want (:time x) (: f)...
;;        Another possible key: M-SPC M-L

;; TODO FUNC      Go to the closest ancestral headline titled "Vestibule" or "v".
;; TODO BIND      Oli's private keymap. C-o?
;; TODO FUNC      demote and next line possible pairing:
;; TODO BIND      C-down to swap lines
;;
