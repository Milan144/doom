;;; UI
(setq doom-theme 'catppuccin
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

(add-to-list 'default-frame-alist '(alpha . 80)) ;; Transparency

;; Line numbers
;; Disabling line number increase performances by a lot
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Indentation (Tabs with 4 spaces lenght)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

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

;; Org mode
(setq org-directory "~/Documents/")

;; Sync org notes
(defun sync-notes ()
  (interactive)
  (shell-command "~/Documents/Org/git-sync.sh"))
(global-set-key (kbd "M-x") 'sync-notes)

(add-hook 'org-mode-hook 'org-make-toc-mode)
