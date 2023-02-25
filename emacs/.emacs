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
(dolist (mode '(term-mode-hook
		org-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq scroll-margin 3)
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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
	(append 
	'("/home/daniel/Documents/sync/personal/gen_per.org"
	  "/home/daniel/Documents/sync/personal/mac_stuff/list.org"
	  )
	(directory-files-recursively "/home/daniel/Documents/sync/study/semester-6/" "\\.org$"))
	)
  (setq org-file-apps '(("\\.tex\\'" . "gvim %s")))
  (setq org-log-done 'time)
  (setq org-agenda-tags-column 60)
  (setq org-tags-column 70)
  :ensure t)

;; (require 'org-tempo)
;; (global-set-key (kbd "C-c o") 'org-agenda)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.15))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.07))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.03))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
 

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

;; (use-package mixed-pitch
;;     :hook
;;     (text-mode . mixed-pitch-mode)
;;     :config
;;     (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
;;     (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
;;     (set-face-attribute 'variable-pitch nil :font "DejaVu Sans"))
;;   ;; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)

;; CUSTOMISATION
(set-face-attribute 'default nil
  :font "Fira Code"
  :height 140)
;; (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")

(add-hook 'org-mode-hook (lambda ()
                            (setq buffer-face-mode-face '(:family "Arial"))
                            (buffer-face-mode)))

(use-package doom-themes
  :init (load-theme 'doom-palenight t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; (use-package olivetti)


;; ====ORG MODE================================================
;; hide emphasis markers (i.e. "render" italics, bold, etc)
(setq org-hide-emphasis-markers t)

;; word wrap
(with-eval-after-load 'org       
  (add-hook 'org-mode-hook #'visual-line-mode))

;; increases latex image scaling
(plist-put org-format-latex-options :scale 2)
;; removes image sizing, so that it could be manually (re)scaled
(setq org-image-actual-width nil)

;; close all headings except current
(defun org-show-current-heading-tidily ()
  (interactive)  ;Inteactive
  "Show next entry, keeping other entries closed."
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-back-to-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (show-children)))

(global-set-key (kbd "C-c h") 'org-show-current-heading-tidily)
(global-set-key (kbd "C-c a") 'org-agenda)
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
    (which-key rainbow-delimiters counsel ivy lsp-ui lsp-mode doom-modeline use-package no-littering neotree evil doom-themes darcula-theme auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; ====LSP-MODE================================================

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; ;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))


;; ====LSP-MODE END============================================
