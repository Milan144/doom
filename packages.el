;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; CUSTOM PACKAGES
(package! pdf-tools)
(package! wakatime-mode)
(package! atom-one-dark-theme)
(package! jsonrpc)
(package! editorconfig)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
(package! org-modern)
