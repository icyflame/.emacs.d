(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; from purcell/emacs.d
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(package-initialize)

;; 1. Don't show splash screen at start-up
(setq inhibit-splash-screen t)

;; 2. Automatically wrap lines at 80 columns and auto-fill at 80 columns inside
;; all major modes
(setq-default fill-column 80)
(setq-default auto-fill-function 'do-auto-fill)

;; 3. Evil mode across most of Emacs
(require-package 'evil)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(require 'evil)
(evil-mode t)

;; 12. Install general package
(require-package 'general)

;; 13. Add key mappings for common actions using general
(general-evil-setup)
(general-nmap
  "DEL" 'evil-ex-nohighlight
  "C-h" 'evil-window-left
  "C-j" 'evil-window-down
  "C-k" 'evil-window-up
  "C-l" 'evil-window-right
  )

;; 5. Set the color scheme to solarized dark
(require-package 'solarized-theme)
(load-theme 'solarized-dark t)

;; 6. Move everything defined for the customize system to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; 7. Go mode settings
(require-package 'go-mode)
(general-nmap
  "gd" 'godef-jump
  "gD" 'godef-describe
  )

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

;; 8. Don't blink cursor
(blink-cursor-mode 0)

;; 9. Install helm
(require-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-b") 'helm-buffers-list)

;; 10. Install use-package
(require-package 'use-package)

;; 11. Install markdown mode
(require-package 'markdown-mode)
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.notes\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

;; 14. Disable audible bell and all related sounds that could come from Emacs
(setq ring-bell-function (lambda () ()))

;; 15. Install plantuml mode; depends on plantuml existing as an executable on
;; the system
(require-package 'plantuml-mode)
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(setq plantuml-default-exec-mode 'executable)

;; 16. Control text size using Ctrl-Shift-+ and Ctrl-Shift-- like in other
;; applications
;; We use the characters that are typically on top of the actual characters of
;; these keys to force the usage of shift
(general-nmap
  "C-+" 'text-scale-increase
  "C-_" 'text-scale-decrease
  )

;; 17. Get helm-projectile and bind to Ctrl-P
(require-package 'helm-projectile)
(general-nmap
  "C-p" 'helm-projectile)

;; 18. Org mode settings
;;; Set the done time for a TODO item when moving it to DONE
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; 19. Install editorconfig
(require-package 'editorconfig)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; 20. Set the default width of a tab character
(setq-default tab-width 4)

;; 22. Install git-link and bind OGF ex command to the main function
(require-package 'git-link)
(use-package git-link
  :config
  (evil-ex-define-cmd "OGF" 'git-link))
