(set-frame-font "Fira Code 16" nil t)
(custom-set-variables '(custom-enabled-themes '(wombat)))
(custom-set-faces)

(xterm-mouse-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(let ((auto-save-dir (concat user-emacs-directory "autosave"))
      (backup-dir (concat user-emacs-directory "backups")))
  (if (not (file-directory-p auto-save-dir))
      (make-directory auto-save-dir t))
  (setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))))

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
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
  elisp-format)

(use-package
  all-the-icons
  ;;:config
  ;; (all-the-icons-install-fonts)
  )

(use-package
  ivy
  :config (ivy-mode 1))

(use-package
  dashboard
  :init (use-package
	  dashboard-hackernews)
  (use-package
    page-break-lines)
  (setq initial-buffer-choice (lambda ()
				(get-buffer "*dashboard*")))
  (setq dashboard-startup-banner "~/.dotfiles/glamanon.jpeg")
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-items '((agenda    . 10)
			  (recents   . 5)
			  (projects  . 5)
			  (hackernews . 10)))
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  (dashboard-setup-startup-hook))

(use-package
  projectile
  :config (setq projectile-completion-system 'ivy projectile-mode-line-prefix " Pro")
  (projectile-mode 1))

(use-package
  nix-mode
  :mode "\\.nix\\'")

(use-package
  terraform-mode
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package
  yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\.erb\\'" . yaml-mode))
