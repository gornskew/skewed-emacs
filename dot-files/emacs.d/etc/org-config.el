;;; org-config -*- lexical-binding: nil -*- --- Org Mode
;;; customizations.  Commentary: Org Mode customizations.  Assumes
;;; this is lazily loaded by a hook such that org is guaranteed
;;; already required by now.
;;;
;;; Code:
;;

;;
;; Standard key bindings
;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(defvar org-todo-keywords)

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

;; -------------------------------------------------------------------
;; TODO keywords and faces
;; -------------------------------------------------------------------
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                  "CANCELLED(c@/!)" "PHONE" "MEETING")))

(defvar org-todo-keyword-faces)
(setq org-todo-keyword-faces
      '(("TODO"      :foreground "red"          :weight bold)
        ("NEXT"      :foreground "blue"         :weight bold)
        ("DONE"      :foreground "forest green" :weight bold)
        ("WAITING"   :foreground "orange"       :weight bold)
        ("HOLD"      :foreground "magenta"      :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)
        ("MEETING"   :foreground "forest green" :weight bold)
        ("PHONE"     :foreground "forest green" :weight bold)))

(defvar org-todo-state-tags-triggers)
(setq org-todo-state-tags-triggers
      '(;; Blocked states: add state tag, remove priority tags
        ("CANCELLED" ("CANCELLED" . t) ("must") ("should") ("could"))
        ("WAITING"   ("WAITING"   . t) ("must") ("should") ("could"))
        ("HOLD"      ("WAITING") ("HOLD" . t) ("must") ("should") ("could"))
        ;; Completion: remove blocking tags
        (done        ("WAITING") ("HOLD"))
        ;; Active states: remove blocking tags (priority tags stay)
        ("TODO"      ("WAITING") ("CANCELLED") ("HOLD"))
        ("NEXT"      ("WAITING") ("CANCELLED") ("HOLD"))
        ("DONE"      ("WAITING") ("CANCELLED") ("HOLD"))))

;; -------------------------------------------------------------------
;; Tags: make must/should/could mutually exclusive, plus meta/urgent
;; -------------------------------------------------------------------
(setq org-tag-alist
      '((:startgroup)
        ("must"   . ?m)
        ("should" . ?s)
        ("could"  . ?c)
        (:endgroup)
        ("meta"         . ?M)
        ("urgent"       . ?u)
        ("website"      . ?w)
        ("skewed_emacs" . ?k)
        ("infra"        . ?i)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-log-done 'time)
(setq org-log-note-clock-out t)

(defvar org-clock-out-when-done)
(setq org-clock-out-when-done t)

(defvar org-clock-persist)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(defun org-find-dangling-clock ()
  "Find a dangling clock entry in an org-mode buffer."
  (interactive)
  (re-search-forward "CLOCK: \\[[^]]*\\] *$"))

(defun org-archive-done-tasks ()
  "Archive all DONE tasks in current tree."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;; -------------------------------------------------------------------
;; Org root bootstrap:
;;   1. ~/projects/org   (host)
;;   2. /projects/org    (container bind mount)
;;   3. nil              (build-time / no org repo yet)
;; -------------------------------------------------------------------
(defvar my/org-root
  (cond
   ((file-directory-p "~/projects/org")
    (expand-file-name "~/projects/org"))
   ((file-directory-p "/projects/org")
    "/projects/org")
   (t nil))
  "Root directory for personal Org files, or nil if not available.")

(defvar my/org-projects-file
  (when my/org-root (expand-file-name "projects.org" my/org-root)))
(defvar my/org-future-file
  (when my/org-root (expand-file-name "future.org" my/org-root)))
(defvar my/org-journal-file
  (when my/org-root (expand-file-name "journal.org" my/org-root)))

;; -------------------------------------------------------------------
;; Agenda basics
;; -------------------------------------------------------------------
;; Show 21 days in agenda by default (3 weeks) so upcoming deadlines are visible
(setq org-agenda-span 21)
;; Start agenda on today, not Monday
(setq org-agenda-start-on-weekday nil)

(when my/org-root
  (setq org-directory my/org-root)
  ;; Only projects.org drives normal agenda views
  (setq org-agenda-files
        (delq nil (list my/org-projects-file))))

;; -------------------------------------------------------------------
;; Capture templates (built conditionally so build-time is safe)
;; -------------------------------------------------------------------
(let ((templates nil))
  ;; Inbox TODOs -> future.org / "Inbox"
  (when my/org-future-file
    (push
     `("t" "Todo (Inbox)" entry
       (file+headline ,my/org-future-file "Inbox")
       "** TODO %?\n   :PROPERTIES:\n   :CREATED: %U\n   :END:\n")
     templates))

  ;; Daily journal with clock summary across projects + future
  (when (and my/org-journal-file my/org-projects-file my/org-future-file)
    (push
     `("d" "Daily journal with clock summary" entry
       (file+olp+datetree ,my/org-journal-file)
       ,(format
         (concat
          "* %%<%%Y-%%m-%%d %%A>\n:PROPERTIES:\n:CREATED: %%U\n:END:\n\n"
          "** Daily Notes\n%%?\n\n"
          "** Time Summary\n"
          "#+BEGIN: clocktable :scope (\"%s\" \"%s\") :maxlevel 3 :block %%<%%Y-%%m-%%d> :link t :compact t :narrow 40!\n"
          "#+END:\n")
         my/org-projects-file
         my/org-future-file)
       :tree-type month)
     templates))

  ;; Simple freeform journal entry
  (when my/org-journal-file
    (push
     `("j" "Journal" entry
       (file+olp+datetree ,my/org-journal-file)
       "* %U\n%?\n"
       :tree-type month)
     templates))

  (setq org-capture-templates (nreverse templates)))

;; -------------------------------------------------------------------
;; Refile targets (only if projects.org exists)
;; -------------------------------------------------------------------
(when my/org-projects-file
  ;; Refile targets - allow refiling to any heading up to level 2 in projects.org
  (setq org-refile-targets
        `((org-agenda-files :maxlevel . 2)
          (,my/org-projects-file :maxlevel . 2))))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; -------------------------------------------------------------------
;; Helper: detect if future.org has any :urgent: items
;; Used so the "⚡ Urgent (from Inbox)" header only appears when needed.
;; Safe if the file doesn't exist (e.g. Docker build).
;; -------------------------------------------------------------------
(defun my/org-has-urgent-inbox-p ()
  "Return non-nil if there are any TODOs tagged :urgent: in future.org."
  (when (and my/org-future-file
             (file-exists-p my/org-future-file))
    (let ((files (list my/org-future-file))
          (found nil))
      (org-agenda-prepare-buffers files)
      (org-map-entries
       (lambda () (setq found t))
       "urgent"
       files)
      found)))

;; -------------------------------------------------------------------
;; Custom agenda views
;;
;; Daily Focus (C-c a d): Routine daily view
;;   - Today's scheduled/deadline items from projects.org
;;   - Urgent items from inbox (tagged :urgent: in future.org)
;;   - Must/Should/Could buckets from projects.org
;;   - Excludes :meta: tasks
;;
;; Inbox Review (C-c a i): Weekly review of captured items in future.org
;; Meta Tasks (C-c a m): System and planning tasks in projects.org
;; Full Backlog (C-c a p): All TODOs from projects.org
;; -------------------------------------------------------------------
(setq org-agenda-custom-commands
      `(("d" "Daily Focus"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-day "today")
		   (org-agenda-skip-scheduled-if-done t)
		   (org-agenda-skip-deadline-if-done t)
		   (org-agenda-skip-timestamp-if-done t)))
          (tags-todo "urgent"
                     ((org-agenda-files ',(when my/org-future-file
                                            (list my/org-future-file)))
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
                 ((org-agenda-files ',(when my/org-future-file
                                        (list my/org-future-file)))
                  (org-agenda-overriding-header "Inbox - Review & Refile")
                  (org-agenda-sorting-strategy
                   '(priority-down time-up scheduled-up))))))

      ("m" "Meta / Planning Tasks"
       ((tags-todo "meta"
                   ((org-agenda-overriding-header "System & Planning Tasks")))))

      ("p" "Full Backlog"
       ((todo ""
              ((org-agenda-overriding-header "Complete Backlog (all TODOs)")))))))

;; -------------------------------------------------------------------
;; D-Bus notification fallback for containers
;; -------------------------------------------------------------------
;; In container environments (no desktop), D-Bus session bus is unavailable.
;; This suppresses the error and provides minibuffer + modeline flash instead.

(when my/in-docker-p
  ;; Suppress D-Bus :session errors silently
  (defvar skewed-emacs-dbus-session-available nil
    "D-Bus session bus not available in container.")

  (defun skewed-emacs--dbus-suppress-session (orig-fun bus &rest args)
    "Suppress D-Bus :session calls when session bus unavailable."
    (if (and (eq bus :session) (not skewed-emacs-dbus-session-available))
        nil
      (apply orig-fun bus args)))

  (advice-add 'dbus-call-method :around #'skewed-emacs--dbus-suppress-session)

  ;; Provide alternative notification via minibuffer + modeline flash
  (setq org-show-notification-handler
        (lambda (msg)
          (message "🔔 Org: %s" msg)
          (let ((orig-face (face-attribute 'mode-line :background)))
            (set-face-attribute 'mode-line nil :background "DarkOrange")
            (run-with-timer 0.5 nil
                            (lambda (bg)
                              (set-face-attribute 'mode-line nil :background bg))
                            orig-face)))))

;; -------------------------------------------------------------------
;; Auto-save org files on clock/state changes
;; -------------------------------------------------------------------
;; Save the current org buffer after clocking or state changes.
;; This ensures work is persisted before stepping away from keyboard.

(defun my/org-save-current-buffer ()
  "Save the current buffer if it's an org file visiting a file."
  (when (and (derived-mode-p 'org-mode)
             (buffer-file-name))
    (save-buffer)))

(defun my/org-save-all-org-buffers ()
  "Save all org buffers that are visiting files."
  (org-save-all-org-buffers))

;; Clock hooks
(add-hook 'org-clock-in-hook #'my/org-save-all-org-buffers)
(add-hook 'org-clock-out-hook #'my/org-save-all-org-buffers)
(add-hook 'org-clock-cancel-hook #'my/org-save-all-org-buffers)

;; State change hook (TODO state transitions)
(add-hook 'org-after-todo-state-change-hook #'my/org-save-current-buffer)

;; Tag change hook
(add-hook 'org-after-tags-change-hook #'my/org-save-current-buffer)

;; Note finalization (after C-c C-c stores the log note)
(advice-add 'org-store-log-note :after #'my/org-save-all-org-buffers)

;; Refile hook (when moving items between files/headings)
(add-hook 'org-after-refile-insert-hook #'my/org-save-all-org-buffers)

(provide 'org-config)

;;; org-config.el ends here
