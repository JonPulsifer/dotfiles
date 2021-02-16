;;; init.el --- my init.el
;;; Commentary:

;;; Code:
(server-start)
(set-frame-font "Fira Code 16" nil t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(xterm-mouse-mode 1)
(global-prettify-symbols-mode +1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-buffer-choice "~/.emacs")
 '(package-selected-packages '(emojify kubernetes dockerfile-mode docker magithub deadgrep tide
				       rjsx-mode json-mode web-mode yaml-mode terraform-mode
				       company-quickhelp ws-butler projectile dashboard-hackernews
				       elisp-format use-package typescript-mode tablist pos-tip
				       nyan-mode nix-mode neotree magit-popup magit js2-mode ivy
				       flycheck exec-path-from-shell dracula-theme dashboard company
				       all-the-icons treemacs treemacs-all-the-icons treemacs-magit treemacs-projectile)))

(setq default-directory (getenv "HOME"))

(let ((auto-save-dir (concat user-emacs-directory "autosave"))
      (backup-dir (concat user-emacs-directory "backups")))
  (if (not (file-directory-p auto-save-dir))
      (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(defun is-mac-p ()
  "This is a MacOS device."
  (eq system-type 'darwin))

(defun is-linux-p ()
  "This is a Linux device."
  (eq system-type 'gnu/linux))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package
  exec-path-from-shell
  :if (is-mac-p)
  :init (exec-path-from-shell-initialize))

(use-package
  flycheck
  :init (global-flycheck-mode))

(use-package
  dracula-theme
  :config (load-theme 'dracula t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package
  elisp-format)

(use-package
  all-the-icons
  ;;:config
  ;; (all-the-icons-install-fonts)
  )

(use-package
  neotree
  :config (setq neo-theme 'icons)
  (setq neo-vc-integration '(face))
  :bind (("<f2>" . neotree-toggle)))

(use-package treemacs
  :init
  (use-package treemacs-all-the-icons)
  (use-package treemacs-magit)
  (use-package treemacs-projectile))

(use-package
  ivy
  :init (ivy-mode 1))

(use-package
  dashboard
  ;; :if (< (length command-line-args) 2)
  :init (use-package
	  dashboard-hackernews)
  (use-package
    page-break-lines)
  :hook (after-init . dashboard-refresh-buffer)
  :config (setq initial-buffer-choice
		(lambda ()
		  (get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/.dotfiles/glamanon.jpeg")
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((agenda    . 10)
			  (recents   . 10)
			  (projects  . 5)
			  (hackernews . 10)))
  (dashboard-setup-startup-hook))

(use-package
  projectile
  :config (setq projectile-completion-system 'ivy projectile-mode-line-prefix " Pro")
  (projectile-mode 1))

;; whitespace
(use-package
  ws-trim
  :load-path "lisp/")
(use-package
  ws-butler
  :hook (prog-mode . ws-butler-mode))
(setq-default show-trailing-whitespace nil)

;; completion
(use-package
  company
  :config (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode t))

(use-package
  company-quickhelp
  :init (use-package
	  pos-tip)
  :config (company-quickhelp-mode))

;; languages
(use-package
  nix-mode
  :mode "\\.nix$")

(use-package
  terraform-mode
  :hook (terraform-mode . #'terraform-format-on-save-mode))

(use-package
  yaml-mode
  :mode "\\.ya?ml$")

;; web
(use-package
  web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode))
  :config (setq web-mode-markup-indent-offset 2 web-mode-css-indent-offset 2
		web-mode-code-indent-offset 2 web-mode-block-padding 2 web-mode-comment-style 2
		web-mode-enable-css-colorization t web-mode-enable-auto-pairing t
		web-mode-enable-comment-keywords t web-mode-enable-current-element-highlight t)
  (add-hook 'web-mode-hook
	    (lambda ()
	      (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package
  typescript-mode
  :config (setq typescript-indent-level 2)
  :hook (typescript-mode . #'subword-mode))

(use-package
  js2-mode)

(use-package
  json-mode
  :mode "\\.json$")

(use-package
  rjsx-mode
  :mode "\\.js\\.*"
  :hook (rjsx-mode . tide-setup))

(defun setup-tide-mode ()
  "Set up tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package
  tide
  :after (typescript-mode company flycheck web-mode)
  :hook ((typescript-mode . tide-setup)
	 (typescript-mode . tide-hl-identifier-mode)
	 (before-save . tide-format-before-save)))

(use-package
  deadgrep)
(global-set-key (kbd "<f5>") #'deadgrep)

(use-package
  magit)
(use-package
  magithub
  :after magit
  :config (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/src/github.com"))

(use-package
  docker
  :bind ("C-c d" . docker))

(use-package
  dockerfile-mode
  :mode "Dockerfile")

(use-package
  kubernetes
  :commands (kubernetes-overview))

(use-package
  emojify
  :hook (after-init . global-emojify-mode))

(use-package
  nyan-mode
  :config (setq nyan-animate-nyancat nil)
  (setq nyan-wavy-trail t)
  (nyan-mode 1))

(provide 'init)
;;; init.el ends here
