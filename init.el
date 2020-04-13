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
(require 'fill-column-indicator)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; 5. Set the color scheme to solarized dark
(require-package 'solarized-theme)
(load-theme 'solarized-dark t)

;; 6. Move everything defined for the customize system to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
