(set-frame-font "Fira Code 14" nil t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(xterm-mouse-mode 1)
(custom-set-variables
 '(custom-enabled-themes '(wombat)))
(custom-set-faces)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package all-the-icons
	;;:config
	;; (all-the-icons-install-fonts)
)

(use-package ivy
	:config
	(ivy-mode 1))

(use-package dashboard
  :init
	(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
	(setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items
        '((recents   . 5)
          (projects  . 10)
          (agenda    . 3)))
  (dashboard-setup-startup-hook))
(use-package projectile
  :config
  (setq projectile-completion-system 'ivy
        projectile-mode-line-prefix " Pro")
  (projectile-mode 1))

(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))

(use-package nix-mode
  :mode "\\.nix\\'")
