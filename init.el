(setq inhibit-startup-message t)

(scroll-bar-mode -1) 	; Disable scroll bar
(tool-bar-mode -1)   	; Disble tool bar
(tooltip-mode -1)	; Disable tooltips
(set-fringe-mode 10)	; Give some breathing room

(menu-bar-mode -1)	; Disbale menu bar

;; Set up visual bell
(setq visible-bell t)

(load-theme 'tango-dark)

(set-face-background 'mode-line "#202020")
(set-face-foreground 'mode-line "#808080")
