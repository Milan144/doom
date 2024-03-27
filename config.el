;; Font
(setq doom-font (font-spec :family "JetbrainsMono Nerd Font" :size 15))

;; Theme
(setq doom-theme 'doom-one)

;; Line numbers
(setq display-line-numbers-type t)

;; Window
(add-to-list 'default-frame-alist '(alpha . 90))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Org mode
(setq org-directory "~/Documents/")

;; Wakatime
(use-package wakatime-mode
  :ensure t)

(global-wakatime-mode)
