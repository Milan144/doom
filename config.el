;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Milan Hommet"
      user-mail-address "milan.hommet@protonmail.com")

;; Theme and font
(setq doom-theme 'doom-rose-pine
      doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))

;; Set a default indentation level
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Hide line numbers
(setq display-line-numbers-type nil)

;; Hide Window decorations
(set-frame-parameter nil 'undecorated t)

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

;; FLUTTER
(defun start-android-emulator-async ()
  "Start the Android emulator asynchronously."
  (interactive)
  (async-shell-command "$HOME/Android/Sdk/emulator/emulator -avd Pixel_8_Pro_API_35"))

;; TIME MANAGEMENT (Only work at my work)
(defun format-pointage-output (output)
  "Format the raw output of pointage by removing ANSI codes and cleaning up the display."
  (require 'ansi-color)
  (let ((formatted-output (ansi-color-apply output)))
    ;; Optionally, clean up or further format the output here
    formatted-output))

(defun pointage ()
  "Run the pointage script and format its output."
  (interactive)
  (let ((output (shell-command-to-string "~/pointage")))
    (message (format-pointage-output output))))

(defun pointage-entree ()
  "Run the pointage script with 'entree' argument and format its output."
  (interactive)
  (let ((output (shell-command-to-string "~/pointage entree")))
    (message (format-pointage-output output))))

(defun pointage-sortie ()
  "Run the pointage script with 'sortie' argument and format its output."
  (interactive)
  (let ((output (shell-command-to-string "~/pointage sortie")))
    (message (format-pointage-output output))))

(defun pointage-last ()
  "Run the pointage script with 'last' argument and format its output."
  (interactive)
  (let ((output (shell-command-to-string "~/pointage last")))
    (message (format-pointage-output output))))

(defun pointage-time ()
  "Run the pointage script with 'time' argument and format its output."
  (interactive)
  (let ((output (shell-command-to-string "~/pointage time")))
    (message (format-pointage-output output))))
