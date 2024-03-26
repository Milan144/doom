(setq doom-font (font-spec :family "JetbrainsMono Nerd Font" :size 15))
;;(setq doom-theme 'doom-one)
(setq doom-theme 'catppuccin)
(setq display-line-numbers-type t)
(setq org-directory "~/Documents/")
(add-to-list 'default-frame-alist '(alpha . 90))
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(use-package wakatime-mode
  :ensure t)

(global-wakatime-mode)
