;; Making It More Like Vim

;;;; Viper Mode
(setq viper-mode t)
(require 'viper)

;; Fixing The Nav In Dired Buffers

(eval-after-load "dired" '(progn
			    (define-key dired-mode-map "k" 'dired-previous-line)
			    (define-key dired-mode-map "v" 'dired-x-find-file)
			    (define-key dired-mode-map "V" 'dired-view-file)
			    (define-key dired-mode-map "j" 'dired-next-line)
			    (define-key dired-mode-map "J" 'dired-goto-file)
			    (define-key dired-mode-map "k" 'dired-previous-line)
			    (define-key dired-mode-map "K" 'dired-do-kill-lines)))

;; Fixing The Nav In Buffer Menu

;; TODO: I Have Not Gotten This To Work Yet

;;;; IDO
(require 'ido)
(ido-mode t)
(ido-everywhere 1)

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-show-dot-for-dired t)
(setq ido-use-filename-at-point t)
(setq ido-separator "\n")

(icomplete-mode 1)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

(global-display-line-numbers-mode 1)

(setq visible-bell t)

(load-theme 'tango-dark)

;; Mode Line Color
(set-face-background 'mode-line "#202020")
(set-face-foreground 'mode-line "#808080")
(set-cursor-color "#ffffff")

;; Minibuffer
(set-face-background 'viper-minibuffer-insert "gray19")
(set-face-foreground 'viper-minibuffer-insert "#ffffff")

(custom-set-faces
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight bold :height 109 :width normal)))))

(transient-mark-mode 1)

(electric-pair-mode 1)

(windmove-default-keybindings)

;; Orgmode Has Its Own Shift bindings
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(require 'org)

;; Make Org Work With Org Files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python .t)
   (shell . t)))

 (setq org-confirm-babel-evaluate nil)

(setq org-directory "~/notes")
(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards "~/notes/*.org")))

;; Create Templates For Inbox and Notes
(setq org-capture-templates
      (quote
       (("i" "Inbox" entry
	 (file "~/notes/inbox.org")
	 "* TODO %?\n/Entered on/ %U")
	("n" "Note" entry
	 (file "~/notes/notes.org")
	 "* Note (%a)\n/Entered on/ %U\n\n%?"))))

;; "C-c c opens the capture menu"
(global-set-key (kbd "C-c c") 'org-capture)

;; "C-c i" Captures a new TODO
(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(global-set-key (kbd "C-c i") 'org-capture-inbox)

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
	(todo . " ")
	(tags . " %i %-12:c")
	(search . " %i %-12:c")))

(setq org-refile-targets
      '(("TARGET-FILE.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

(defun gtd-save-org-buffers ()
  "Save 'org-agenda-files' buffers without user confirmation.
See also 'org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
			 (when (member (buffer-file-name) org-agenda-files)
			   t)))
  (message "Saving org-agenda-buffers ...done"))

(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
	     (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
	 ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-deadline-warning-days 0)))
	  (todo "NEXT"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format " %i %-12:c [%e] ")
		 (org-agenda-overriding-header "\nTasks\n")))
	  (agenda nil
		  ((org-agenda-entry-types '(:deadline))
		   (org-agenda-format-date "")
		   (org-deadline-warning-days 7)
		   (org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
		    (org-agenda-overriding-header "\nDeadlines\n")))
	  (tags-todo "inbox"
			     ((org-agenda-prefix-format " %?-12t% s")
			      (org-agenda-overriding-header "\nInbox\n")))
	  (tags "CLOSED>=\"<today>\""
		((org-agenda-overriding-header "\nCompleted Today\n")))))))

(setq type-break-mode t)
(setq type-break-good-rest-interval 60)
(setq type-break-interval 1800)
(setq type-break-mode-line-message-mode t)
(setq type-break-terse-messages t)
(setq type-break-time-warning-intervals '(300 120 60 30 15))
