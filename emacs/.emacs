;; PACKAGE-MANAGING=====================================
(package-initialize)
(require 'package)
(setq user-emacs-directory "/home/daniel/.emacs.d") 
(require 'use-package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; this will download missing packages (I think/hope) https://ianyepan.github.io/posts/setting-up-use-package/
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
;; PACKAGE-MANAGING-DONE================================


(setq inhibit-splash-screen t) ; Disable welcome menu
(setq inhibit-startup-message t)

(tool-bar-mode -1)          ; Disable the toolbar
(scroll-bar-mode -1)        ; Disable the scrollbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(evil-mode 1)
(global-display-line-numbers-mode)

(use-package evil
             :ensure t)

(use-package org
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t)
             :ensure t)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; padding from left/right side 
(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))


; (org-babel-load-file "/home/daniel/.emacs.d/myinit.org")


(set-face-attribute 'default nil
  :font "Fira Code"
  :height 140)

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


;; ====ORG MODE================================================
;; hide emphasis markers (i.e. "render" italics, bold, etc)
(setq org-hide-emphasis-markers t)

;; word wrap
(with-eval-after-load 'org       
  (add-hook 'org-mode-hook #'visual-line-mode))
;; ====ORG MODE=END============================================


;; NEOTree
(global-set-key [f8] 'neotree-toggle)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (doom-modeline use-package no-littering neotree evil doom-themes darcula-theme auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
