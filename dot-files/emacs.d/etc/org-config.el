;;; org-config -*- lexical-binding: nil -*- --- Org Mode
;;; customizations.  Commentary: Org Mode customizations.  Assumes
;;; this is lazily loaded by a hook such that org is guaranteed
;;; already required by now.
;;;
;;; Code:
;;

;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(defvar org-todo-keywords)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(defvar org-todo-keyword-faces)
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(defvar org-todo-state-tags-triggers)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

;; -------------------------------------------------------------------
;; Tags: make must/should/could mutually exclusive, plus meta/urgent
;; -------------------------------------------------------------------
(setq org-tag-alist
      '((:startgroup)
        ("must"   . ?m)
        ("should" . ?s)
        ("could"  . ?c)
        (:endgroup)
        ("meta"   . ?M)
        ("urgent" . ?u)
        ("website"       . ?w)
        ("skewed_emacs"  . ?k)
        ("infra"         . ?i)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-log-done 'time)
(setq org-log-note-clock-out t)
(defvar org-clock-out-when-done)
(setq org-clock-out-when-done t)
;;(setq org-clock-persistence t)
(defvar org-clock-persist)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an org-mode buffer"
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$"))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

(setq org-directory "~/projects/org")

;; Show 21 days in agenda by default (3 weeks) so upcoming deadlines are visible
(setq org-agenda-span 21)
;; Start agenda on today, not Monday
(setq org-agenda-start-on-weekday nil)

;; Only projects.org drives normal agenda views
(setq org-agenda-files
      '("~/projects/org/projects.org"))

;; Capture: quick TODOS into future.org, journal into journal.org
(setq org-capture-templates
      '(("t" "Todo (Inbox)" entry (file+headline "~/projects/org/future.org" "Inbox")
         "** TODO %?\n   :PROPERTIES:\n   :CREATED: %U\n   :END:\n")
        ("j" "Journal" entry (file+olp+datetree "~/projects/org/journal.org")
         "* %U\n%?\n" :tree-type month)))

;; Refile targets - allow refiling to any heading up to level 2 in projects.org
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)
                           ("~/projects/org/projects.org" :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; -------------------------------------------------------------------
;; Helper: detect if future.org has any :urgent: items
;; Used so the "⚡ Urgent (from Inbox)" header only appears when needed
;; -------------------------------------------------------------------
(defun my/org-has-urgent-inbox-p ()
  "Return non-nil if there are any TODOs tagged :urgent: in future.org."
  (let ((files '("~/projects/org/future.org"))
        (found nil))
    (org-agenda-prepare-buffers files)
    (org-map-entries
     (lambda () (setq found t))
     "urgent"
     files)
    found))

;; Custom agenda views
;;
;; Daily Focus (C-c a d): Your routine daily view
;;   - Today's scheduled/deadline items from projects.org
;;   - Urgent items from inbox (tagged :urgent: in future.org)
;;   - Must/Should/Could priority buckets from projects.org
;;   - Excludes :meta: tasks (system/planning work)
;;
;; Inbox Review (C-c a i): Weekly review of captured items in future.org
;; Meta Tasks (C-c a m): System and planning tasks in projects.org
;; Full Backlog (C-c a p): All TODOs from projects.org
(setq org-agenda-custom-commands
      '(("d" "Daily Focus"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-day "today")))
          (tags-todo "urgent"
                     ((org-agenda-files '("~/projects/org/future.org"))
                      ;; Only show the visible header when there are urgent items.
                      ;; Otherwise, override with an empty string so the default
                      ;; "Tags search" header doesn’t appear.
                      (org-agenda-overriding-header
                       (if (my/org-has-urgent-inbox-p)
                           "⚡ Urgent (from Inbox)"
                         ""))))
          (tags-todo "must"
                     ((org-agenda-overriding-header "Must Do")))
          (tags-todo "should"
                     ((org-agenda-overriding-header "Should Do")))
          (tags-todo "could"
                     ((org-agenda-overriding-header "Could Do"))))
         ;; Hide :meta: tasks from this Daily Focus view
         ((org-agenda-tag-filter-preset '("-meta"))))

        ("i" "Inbox Review"
         ((alltodo ""
                   ((org-agenda-files '("~/projects/org/future.org"))
                    (org-agenda-overriding-header "Inbox - Review & Refile")
                    (org-agenda-sorting-strategy
                     '(priority-down time-up scheduled-up))))))

        ("m" "Meta / Planning Tasks"
         ((tags-todo "meta"
                     ((org-agenda-overriding-header "System & Planning Tasks")))))

        ("p" "Full Backlog"
         ((todo ""
                ((org-agenda-overriding-header "Complete Backlog (all TODOs)")))))))

(provide 'org-config)
