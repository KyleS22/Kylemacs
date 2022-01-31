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

;; Compltion With Ido

;;;; IDO
(require 'ido)
(ido-mode t)
(ido-everywhere 1)

(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-show-dot-for-dired t)
(setq ido-use-filename-at-point t)
(setq ido-separator "\n")

;; Command Completion In Minibuffer

(icomplete-mode 1)

;; UI Changes

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(global-display-line-numbers-mode 1)

(setq visible-bell t)

;; Theme Related

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

;; Transients

(transient-mark-mode 1)

;; Brace Completion

(electric-pair-mode 1)
(show-paren-mode 1)

;; Window Switching
;; Enable Switching between windows using "<SHIFT>-arrow"

(windmove-default-keybindings)

;; Orgmode Has Its Own Shift bindings
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Configure Org

(require 'org)

;; Make Org Work With Org Files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Org Babel
;; Org mode will not run languages that are not configured, so specify them here.  Org will also prompt for running languages by default, but I don't want that.


(org-babel-do-load-languages
 'org-babel-load-languages
 '((python .t)
   (shell . t)))

 (setq org-confirm-babel-evaluate nil)

(setq org-directory "~/notes")
(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards "~/notes/*.org")))



;; To use GTD, we will have the concept of an inbox.  This is a place that we can quickly add new TODO items to deal with later.
;; We can use "C-c i" to quickly create a new todo item.  We can also use "C-c c" to open the org capture menu for other types of captures.


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

;; Configuring The Agenda
;; We want Quick Access to the agenda through "C-c a".  We also define an agenda prefix format for different items.


(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
	(todo . " ")
	(tags . " %i %-12:c")
	(search . " %i %-12:c")))




;; When we open the TODO list through "C-c a", we can get a list of all the TODO items in the inbox.  We wnat to be able to move those todo items to a new place, probably whatever notes file we are currently working in.
;; This allows us to organize TODOs based on projects. We can refile the task with 'C-c C-w'.  This will help us move the task.  Replace "TARGET_FILE.org" with your main notes file.
;; We also add the ability to auto save when we refile.

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



;; We also want to be able to activate tasks we are currently working on. To do this we will add new TODO states.


(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))



;; For time tracking, we can add the ability to automatically log the time that a task was switched to the NEXT state.  We can also automatically log when it is moved to the DONE state.


(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
	     (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
(setq org-log-done 'time)



;; Next, we add a special agenda view for GTD


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

;; Type Break
;; I like my hands, so remind me to rest them once in a while.


(setq type-break-mode t)
(setq type-break-good-rest-interval 60)
(setq type-break-interval 1800)
(setq type-break-mode-line-message-mode t)
(setq type-break-terse-messages t)
(setq type-break-time-warning-intervals '(300 120 60 30 15))
(setq type-break-query-mode t)

;; CTAGS
;; I often use ctags to navigate code bases. What I want here is to auto generate ctags files for the current project, without blocking.  


;; Currently this will generate tags for the current directory of the current file, and its file type.
(defun git-root-dir ()
  "Get the root directory of a git repo."
  (interactive)
  (substring (shell-command-to-string "git rev-parse --show-toplevel") 0 -1))

(defun generate-etags (dir-name extension)
  "Create a tags file."
  (interactive)
  (call-process "/bin/bash" nil t nil "-c" 
		(format "find %s -type f -name \"*.%s\" | etags -o %s/TAGS.new - && ! cmp --silent %s/TAGS %s/TAGS.new && mv %s/TAGS.new %s/TAGS" dir-name extension dir-name dir-name dir-name dir-name dir-name)))

(defun generate-etags-cur-buffer ()
  "Generates etags for the directory of the current buffer."
  (interactive)
  (if (vc-registered (buffer-file-name))
      (generate-etags (git-root-dir) (file-name-extension (buffer-file-name)))))

;; Call after save
;; This was causing problems where I could not leave
(add-hook 'after-save-hook #'generate-etags-cur-buffer)
