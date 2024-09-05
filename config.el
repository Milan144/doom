;; Theme and font
(setq doom-theme 'doom-rose-pine
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; Transparency
(add-to-list 'default-frame-alist '(alpha-background . 90))

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Indentation (Tabs with 4 spaces lenght)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Set space e to open treemacs and close it
(map! :leader
      :desc "Toggle treemacs"
      "e" #'treemacs)

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
