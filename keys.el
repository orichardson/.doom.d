;;; ~/.doom.d/keys.el -*- lexical-binding: t; -*-

(map! :map org-mode-map "C-c DEL" 'org-mark-ring-last-goto)
(map! :after org
      (:map evil-org-mode-map
       :n [C-backtab] #'org-mark-ring-last-goto
       :n [S-return] #'org-toggle-latex-fragment))


(map! "C-|" #'neotree-toggle)
