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

;; Line numbering ======================================
(column-number-mode)
(global-display-line-numbers-mode)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;; Line numbering end===================================

(use-package evil
             :ensure t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; IVY=================================================
(use-package ivy
 :diminish
 :bind (("C-s" . swiper)
        :map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)
        ("C-l" . ivy-alt-done)
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map
        ("C-k" . ivy-previous-line)
        ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(use-package counsel)
;; IVY-DONE============================================

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1.5))

(use-package org
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-startup-indented t)
  (setq org-agenda-files
	'("/home/daniel/Documents/General Vault/orgfiles/tasks.org"))
  (setq org-file-apps '(("\\.tex\\'" . "gvim %s")))
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

;; increases latex image scaling
(plist-put org-format-latex-options :scale 2)
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
