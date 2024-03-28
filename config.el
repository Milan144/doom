;; Theme and font
(setq doom-theme 'doom-one
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))

;; Line numbers
;; Disabling line number increase performances by a lot
(setq display-line-numbers-type nil)

;; Window
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(alpha . 90)) ;; Transparency
(add-hook 'window-setup-hook 'toggle-frame-maximized t) ;; Maximized on startup

;; Indentation (Tabs with 4 spaces lenght)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))

;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; Org mode
(setq org-directory "~/Documents/")

;; Time management

;; Wakatime
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

;; Odoo
(defun pointage ()
  (interactive)
  (shell-command "~/pointage"))
(global-set-key (kbd "M-x") 'pointage)
(defun pointage-entree ()
  (interactive)
  (shell-command "~/pointage entree"))
(global-set-key (kbd "M-x") 'pointage-entree)
(defun pointage-sortie ()
  (interactive)
  (shell-command "~/pointage sortie"))
(global-set-key (kbd "M-x") 'pointage-sortie)
(defun pointage-last ()
  (interactive)
  (shell-command "~/pointage last"))
(global-set-key (kbd "M-x") 'pointage-last)
(defun pointage-time ()
  (interactive)
  (shell-command "~/pointage time"))
(global-set-key (kbd "M-x") 'pointage-time)
