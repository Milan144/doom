;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Milan Hommet"
      user-mail-address "milan.hommet@protonmail.com")

;; Theme and font
(setq doom-theme 'catppuccin
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14))

;; Set a default indentation level
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; An evil mode indicator is redundant with cursor shape
(setq doom-modeline-modal nil)

;; UI
(setq fancy-splash-image (file-name-concat doom-user-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(setq +doom-dashboard-functions '(doom-dashboard-widget-banner))

;; Open dashboard
(map! :leader
      :desc "Open dashboard"
      "d b" #'+doom-dashboard/open)

;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; ORG MODE
(use-package org
  :defer t
  :config
  (setq org-directory "~/Documents/")
  (add-hook 'org-mode-hook 'org-make-toc-mode)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (use-package org-modern
    :config
    (setq org-modern-label-border nil)
    (global-org-modern-mode)))

;; OPTIMIZATIONS
;; Increase the garbage collection threshold to 100MB
(setq gc-cons-threshold (* 100 1024 1024)) ;; Default is 800KB
;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; Set a higher delay for idle updates (default is 0.5)
(setq idle-update-delay 1.0)

;; TIME MANAGEMENT
(defun format-pointage-output (output)
  "Format the raw output of pointage."
  (require 'ansi-color)
  (let ((formatted-output (ansi-color-apply output)))
    ;; Optionally, clean up or further format the output here
    formatted-output))

(defun run-pointage (arg)
  "Run the pointage script with a specific argument ARG."
  (interactive "sEnter argument: ")
  (let ((output (shell-command-to-string (concat "~/pointage " arg))))
    (message (format-pointage-output output))))

;; Define specific functions
(defun pointage () (interactive) (run-pointage ""))
(defun pointage-entree () (interactive) (run-pointage "entree"))
(defun pointage-sortie () (interactive) (run-pointage "sortie"))
(defun pointage-last () (interactive) (run-pointage "last"))
(defun pointage-time () (interactive) (run-pointage "time"))

;; Theme toggling
(defun toggle-theme ()
  "Toggle between catppuccin flavors."
  (interactive)
  (if (equal catppuccin-flavor 'latte)
      (progn
        (setq catppuccin-flavor 'mocha)
        (catppuccin-reload))
    (progn
      (setq catppuccin-flavor 'latte)
      (catppuccin-reload))))

;; Keybinding to toggle theme
(map! :leader
      :desc "Toggle theme"
      "t T" #'toggle-theme)

;; Keybinding to toggle theme
(map! :leader
      :desc "Toggle theme"
      "t T" #'toggle-theme)

;; accept completion from copilot and fallback to company
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
  (add-to-list 'copilot-indentation-alist '(prog-mode . 4))
  (add-to-list 'copilot-indentation-alist '(org-mode . 4))
  (add-to-list 'copilot-indentation-alist '(text-mode . 4))
  (add-to-list 'copilot-indentation-alist '(closure-mode . 4))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode . 4)))
