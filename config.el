;; Theme and font
(setq doom-theme 'catppuccin
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

;; Transparency
(add-to-list 'default-frame-alist '(alpha . 90)) 

;; Disabling line numbers
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Indentation (Tabs with 4 spaces lenght)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Wakatime
(use-package wakatime-mode
  :ensure t
  :init (global-wakatime-mode))

;; Org mode
(use-package org
  :defer t
  :config
  (setq org-directory "~/Documents/")
  (add-hook 'org-mode-hook 'org-make-toc-mode)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

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

;; Git sync notes
(defun sync-notes ()
  (interactive)
  (shell-command "~/Documents/Org/git-sync.sh"))
(global-set-key (kbd "M-x") 'sync-notes)