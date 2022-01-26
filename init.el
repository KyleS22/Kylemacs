
;;;; Basic Environment Settings
(setq inhibit-startup-message t)

(scroll-bar-mode -1) 	; Disable scroll bar
(tool-bar-mode -1)   	; Disble tool bar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 10)	; Give some breathing room

(menu-bar-mode -1)	; Disbale menu bar

(global-display-line-numbers-mode 1)
;; Set up visual bell
(setq visible-bell t)

;; Theme I like
(load-theme 'tango-dark)

;; Set modeline to a better colour
(set-face-background 'mode-line "#202020")
(set-face-foreground 'mode-line "#808080")

;; Enable Transient Mark Mode
(transient-mark-mode 1)

;; Auto Close Braces
(electric-pair-mode 1)

;;; Orgmode Config
;; Enable Orgmode
(require 'org)

;; Make Org work with .org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Enable Languages I Use
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)))

;; Disable prompt for running code
(defun my-org-confirm-babel-evaluate (lang body)
  (and (not (string= lang "python") not (string= lang "shell")))) ; Dont ask for python

(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/test_notes.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
