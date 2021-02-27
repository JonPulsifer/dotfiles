;;; init.el --- my init.el
;;; Commentary:

;;; Code:

(defun jp/startup ()
  "See how long Emacs takes to load."
  (message "Emacs loaded in %s with %d garbos" (format "%.2f seconds" (emacs-init-time)) gcs-done))
(add-hook 'emacs-startup-hook #'jp/startup)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; keep customizations out of init.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

;; whoami
(setq user-full-name "Jonathan Pulsifer" user-mail-address "jonathan@pulsifer.ca")

(message "LETSGO..")

(eval-when-compile (add-to-list 'load-path "~/.emacs.d/lisp")
		   (require 'use-package))

(setq use-package-always-ensure t)

;; start emacs daemon
(server-start)

(set-frame-font "Fira Code 12" nil t)

(xterm-mouse-mode 1)
(global-prettify-symbols-mode +1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-hl-line-mode 1)
(global-display-line-numbers-mode)

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

(when (is-mac-p)
 (setq mac-option-modifier 'super
       mac-command-modifier 'meta))

(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package
  rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package
  exec-path-from-shell
  :if (is-mac-p)
  :init (exec-path-from-shell-initialize))

(use-package
  helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)))

(use-package
  yasnippet
  :diminish (yas-minor-mode . "")
  :hook (after-init . yas-global-mode))


(use-package
  yasnippet-snippets
  :after yasnippet)

(use-package
  flycheck
  :init (global-flycheck-mode))

(use-package
  dracula-theme
  :config (load-theme 'dracula t))

(use-package
  doom-modeline
  :init (doom-modeline-mode 1))

(use-package
  all-the-icons
  ;;:config
  ;; (all-the-icons-install-fonts)
  )


(use-package
  treemacs
  :init (use-package
	  treemacs-all-the-icons)
  (use-package
    treemacs-magit)
  (use-package
    treemacs-projectile)
  :bind (("<f2>" . treemacs)))


(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (go-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c s"))

(use-package helm
  :defer nil
  :diminish helm-mode
  :bind (("C-x c" . helm-command-prefix-key)
         ("C-c i" . helm-imenu)
         ("C-c m" . helm-all-mark-rings)
         ("C-x b" . helm-mini)
         ("C-c r" . helm-regexp)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode t))

(use-package helm-ag
  :bind ("C-c g" . helm-projectile-ag))

(use-package helm-atoms
  :defer t)

(use-package helm-descbinds)

(use-package helm-lsp
  :after (helm lsp-mode))


(use-package helm-projectile
  :after (helm projectile))


(use-package helm-system-packages
  :defer t)


(use-package helm-tramp
  :commands (helm-tramp))

(use-package
  diminish)

(use-package
  projectile
  :after helm
  :config (setq projectile-completion-system 'helm projectile-mode-line-prefix " Pro")
  (projectile-mode 1))

(use-package
  dashboard-hackernews)

(use-package
  page-break-lines
  :config (global-page-break-lines-mode))

(use-package
  dashboard
  :requires (dashboard-hackernews page-break-lines projectile)
  :hook (after-init . dashboard-refresh-buffer)
  :config (setq dashboard-startup-banner "~/.dotfiles/glamanon.jpeg")
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
  which-key
  :defer t
  :hook (after-init . which-key-mode)
  :init (which-key-setup-side-window-right-bottom))

;; whitespace
(use-package
  ws-trim
  :defer t
  :load-path "lisp/")

(use-package
  ws-butler
  :defer t
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
  :hook (typescript-mode . #'subword-mode)
  :config (setq typescript-indent-level 2))

(use-package
  js2-mode)

(use-package
  json-mode
  :mode "\\.json$'")

(use-package
  rjsx-mode
  :mode "\\.js$"
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

(use-package prettier-js
  :ensure t
  :hook (web-mode . prettier-js-mode))

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
  :if (display-graphic-p)
  :custom (setq nyan-animate-nyancat nil)
  (setq nyan-wavy-trail t)
  :config
  (nyan-mode 1))

(provide 'init)
;;; init.el ends here
