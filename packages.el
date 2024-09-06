;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; CUSTOM PACKAGES
(package! pdf-tools)
(package! org-modern)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
