;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Milan Hommet"
      user-mail-address "milan.hommet@protonmail.com")

;; Theme and font
(setq doom-theme 'doom-rose-pine
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16))

;; Set a default indentation level
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)  ;; Use spaces instead of tabs

;; Remove Windows decorations
(setq default-frame-alist '((undecorated . t)))

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;;; :ui doom-dashboard
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
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

;; OPTIMIZATIONS

;; Increase the garbage collection threshold to 100MB
(setq gc-cons-threshold (* 100 1024 1024)) ;; Default is 800KB

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Set a higher delay for idle updates (default is 0.5)
(setq idle-update-delay 1.0)

;; Make company popups faster
(setq company-idle-delay 0.1
      company-minimum-prefix-length 1)

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  ;; Set 4 spaces for the following modes
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-to-list 'copilot-indentation-alist '(org-mode 4))
  (add-to-list 'copilot-indentation-alist '(text-mode 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 4)))

(defun start-android-emulator-async ()
  "Start the Android emulator asynchronously."
  (interactive)
  (async-shell-command "$HOME/Android/Sdk/emulator/emulator -avd Pixel_8_Pro_API_35"))
