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

(require 'evil)
(evil-mode t)

;; 4. Download fill-column-indicator.el and put it inside `~/.emacs.d/lisp`
;; Turn on fci-mode as a globalized minor mode for all files.
;; TODO: Fill column indicator doesn't move when the text-scale is increased or
;; decreased
;; (require 'fill-column-indicator)
;; (define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode 1)

;; 5. Set the color scheme to solarized dark
(require-package 'solarized-theme)
(load-theme 'solarized-dark t)

;; 6. Move everything defined for the customize system to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; 7. Go mode settings
(require-package 'go-mode)

;; 8. Don't blink cursor
(blink-cursor-mode 0)

;; 9. Install helm
(require-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

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
