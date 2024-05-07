;; Theme and font
(setq doom-theme 'atom-one-dark
      doom-font (font-spec :family "JetBrainsMono" :size 14))

;; Transparency
(add-to-list 'default-frame-alist '(alpha . 95))

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Indentation (Tabs with 4 spaces lenght)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Wakatime
(use-package wakatime-mode
  :ensure t
  :init (global-wakatime-mode))

;; Org mode
(setq org-modern-label-border nil)
(global-org-modern-mode)

(use-package org
  :defer t
  :config
  (setq org-directory "~/Documents/")
  (add-hook 'org-mode-hook 'org-make-toc-mode)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
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

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
