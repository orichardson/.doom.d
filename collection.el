;;; PRACTICE WRITING CODE

(defun oli-mul (arg1 arg2)
  (* arg1 arg2) )

(defun oli-log (arg1 arg2)
  (interactive)
  (message arg1 arg2))

(oli-mul 3 4)


;;; USEFUL FUNCTIONS
;; Custom carryover
;; Let's go
(defun oli-custom-carryover ()
  ""
  ()
  )


;; TODO
(defun goto-or-open-current-journal ()
  (get-buffer-window (get-file-buffer org-journal-  h h))




;; copied from [[org-journal/org-journal.el]]
(defun org-journal--carryover ()
"Moves all items matching `org-journal-carryover-items' from the
previous day's file to the current file."
  (interactive)
  (let* ((org-journal-find-file 'find-file)
         (mapper (lambda ()
                   (let ((headings (org-journal--carryover-item-with-parents)))
                     ;; Since the next subtree now starts at point,
                     ;; continue mapping from before that, to include it
                     ;; in the search
                     (setq org-map-continue-from (point))
                     headings)))
         carryover-paths prev-buffer)

    ;; Get carryover paths
    (save-excursion
      (save-restriction
        (when (org-journal--open-entry t t)
          (setq prev-buffer (current-buffer))
          (unless (org-journal--daily-p)
            (org-narrow-to-subtree))
          (setq carryover-paths (org-map-entries mapper org-journal-carryover-items)))))

    (when (and prev-buffer carryover-paths)
      (let (cleared-carryover-paths text)
        ;; Construct the text to carryover, and remove any duplicate elements from carryover-paths
        (cl-loop
           for paths in carryover-paths
           with prev-paths
           do (cl-loop
                 for path in paths
                 with cleared-paths
                 count t into counter
                 do (when (or (not (and prev-paths (nth counter prev-paths)))
                              (> (car path) (car (nth counter prev-paths))))
                      (setq text (concat text (cddr path)))
                      (if cleared-paths
                          (setcdr (last cleared-paths) (list path))
                        (setq cleared-paths (list path))))
                 finally (if cleared-carryover-paths
                             (setcdr (last cleared-carryover-paths) cleared-paths)
                           (setq cleared-carryover-paths cleared-paths))
                   (setq prev-paths paths)))
        (org-journal-carryover-items text cleared-carryover-paths prev-buffer))
      (org-journal--carryover-delete-empty-journal prev-buffer))

    (when org-journal--kill-buffer
      (mapc 'kill-buffer org-journal--kill-buffer)
      (setq org-journal--kill-buffer nil))))
