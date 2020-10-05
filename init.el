(require 'package)

;; Links to useful Emacs Wiki
;; 1. https://www.emacswiki.org/emacs/EmacsKeyNotation
;; 2. Temporary files: "#*#" and "*~"; Find commands:
;; find ~ -iname "#*#"
;; find ~ -iname "*~"
;; 3. Evil EX commands list: Display a list of ex commands with the mapped functions
;; M-x describe-variable evil-ex-commands RET

;; Ongoing issues

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

;;; similar to require-package but using package-install-fil instead
(defun require-package-file (package file)
  "Install a package from the given location"
  (if (package-installed-p package)
	  t
	(package-install-file file)))

(package-initialize)

(require-package 'init-loader)
(setq init-loader-default-regexp "\\(?:^[[:digit:]]\\{1\\}\\).*\\.el\$") ;; default だと *.el~ も対象になってしまう。
(init-loader-load "~/.emacs.d/imported-confs")

;; 10. Install use-package
(require-package 'use-package)

;; 1. Don't show splash screen at start-up
(setq inhibit-splash-screen t)

;; 26. Remove trailing whitespace characters from all files
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 3. Evil mode across most of Emacs
(require-package 'evil)
(use-package evil
  :init
  (setq evil-search-module 'evil-search
		evil-want-C-u-scroll t
		evil-want-C-w-in-emacs-state t)
  ;; Normal mode is the default init mode
  (setq evil-emacs-state-modes nil)
  (setq evil-insert-state-modes nil)
  (setq evil-motion-state-modes nil)
  :config
  (evil-mode)
  (evil-ex-define-cmd "q" 'kill-this-buffer)
  )

;; 12. Install general package
(require-package 'general)
(use-package general
  :config
  ;; 13. Add key mappings for common actions using general
  (general-evil-setup)
  (general-nmap
	"DEL" 'evil-ex-nohighlight
	"C-h" 'evil-window-left
	"C-j" 'evil-window-down
	"C-k" 'evil-window-up
	"C-l" 'evil-window-right
	)
  ;; 16. Control text size using Ctrl-Shift-+ and Ctrl-Shift-- like in other
  ;; applications
  ;; We use the characters that are typically on top of the actual characters of
  ;; these keys to force the usage of shift
  (general-nmap
	"C-+" 'text-scale-increase
	"C-_" 'text-scale-decrease
	)

  (general-create-definer ctrl-keybindings
	:states '(normal visual insert)
	)

  (ctrl-keybindings
	:keymaps '(org-mode-map)
   "C-c l c" 'org-cycle-list-bullet
   "C-c e" 'org-table-edit-formulas
   "C-c t" 'org-show-todo-tree
   "C-a" 'org-agenda
   ;; Kill this buffer for these key bindings too
   "M-k" 'kill-this-buffer
   "s-k" 'kill-this-buffer
   )

  (general-evil-define-key '(normal visual) org-agenda-mode-map
	"F" 'org-agenda-later
	"B" 'org-agenda-earlier
	)

  ;; 23. SQL format highlighted region
  (general-evil-define-key 'visual sql-mode-map
	"gq" 'run-sqlbeautify
	)

  ;; 24. Keybindings that use the leader key functionality in normal and visual mode
  (general-create-definer leader-def-mode
	:prefix ","
	)

  (leader-def-mode
	:states '(normal)
	"c SPC" 'comment-line
	)

  (leader-def-mode
	:states '(visual)
	"c SPC" 'comment-or-uncomment-region
	"c s" 'comment-or-uncomment-region
	)

  (leader-def-mode
	:states '(normal visual)
	"w" 'evil-write
	)
  )

;; 5. Set the color scheme to solarized dark
(require-package 'solarized-theme)
(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t)
  )

;; 6. Move everything defined for the customize system to a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; 7. Go mode settings
;; (defun evil-set-jump-args (&rest ns) (evil-set-jump))
(require-package 'go-mode)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(require-package 'gotest)

;; 8. Don't blink cursor
(blink-cursor-mode 0)

;; 9. Install helm
(require-package 'helm)
(use-package helm
  :config

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "s-x") 'helm-M-x)

  (global-set-key (kbd "M-b") 'helm-buffers-list)
  (global-set-key (kbd "s-b") 'helm-buffers-list)

  (evil-ex-define-cmd ":" 'helm-locate)
  (evil-ex-define-cmd "X" 'helm-M-x)

  (helm-mode t)
  )

(setq-default fill-column 100)

;; 11. Install markdown mode
(require-package 'markdown-mode)
(use-package markdown-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.notes\\'" . gfm-mode)
   ("\\.markdown\\'" . gfm-mode))
  :init
  (add-hook 'markdown-mode-hook #'auto-fill-mode)
  (setq markdown-command "multimarkdown")
  (setq markdown-open-command "firefox"))

;; 33. Comp(lete) any(thing)
(require-package 'lsp-mode)
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (setq lsp-enable-file-watchers nil)
  (general-nmap
	"g t" 'lsp-goto-type-definition
	"g d" 'lsp-find-definition
	"C-]" 'lsp-find-definition
	)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
	 ("gopls.staticcheck" t t)))
  )

;; 14. Disable audible bell and all related sounds that could come from Emacs
(setq ring-bell-function (lambda () ()))

;; 15. Install plantuml mode; depends on plantuml existing as an executable on
;; the system
(require-package 'plantuml-mode)
(use-package plantuml-mode
  :mode
  (("\\.puml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path "/usr/local/bin/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)

  (setq org-plantuml-jar-path (expand-file-name "/usr/local/bin/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  )

(require-package 'helm-rg)

;; 17. Get helm-projectile and bind to Ctrl-P
(require-package 'helm-projectile)
(use-package helm-projectile
  :config
  (general-nmap
	"C-p" 'helm-projectile
	"C-b" 'helm-projectile-switch-to-buffer)
  (evil-ex-define-cmd "Ag" 'helm-projectile-rg)
  (evil-ex-define-cmd "Rg" 'helm-projectile-rg))

;; 18. Org mode settings
;;; Set the done time for a TODO item when moving it to DONE
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(setq org-agenda-files '("~/work/notes/Monthly/Current.org"))

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

;; 25. Yaml Mode
(require-package 'yaml-mode)
(use-package yaml-mode
  :mode
  ("\\.yml\\'" . yaml-mode)
  )

;; 27. Include powerline
(require-package 'powerline)
(use-package powerline
    :config
    (powerline-center-evil-theme))

;; 29. JSON mode
(require-package 'json-mode)

;; 30. ggtags
(require-package 'ggtags)

;; 28. PHP mode
(require-package 'php-mode)
(use-package php-mode
  :config
  (general-nmap
	:keymaps 'php-mode-map
	"g d" 'ggtags-find-definition
	"g D" 'ggtags-find-tag-dwim
	))

;; 32. Magit
(require-package 'magit)

;; Language server protocol client
(require-package 'lsp-mode)
;; 33. Comp(lete) any(thing)
(require-package 'company)
;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 3))

;; 34. Protobuf mode
(require-package 'protobuf-mode)

;; 35. Yasnippets
(require-package 'yasnippet)
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'company-mode)
  )

;; (require-package 'company-go)
;; (add-hook 'go-mode-hook (lambda ()
;; (set (make-local-variable 'company-backends) '(company-go))))

;; 34. Org Roam

;; Don't use this until the `title/titles` thing has been cleaned up
;; (require-package 'org-roam)

(require-package-file 'org-roam "~/.emacs.d/lisp/org-roam")
(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/personal/notes/org-roam")

  :config
  (setq org-roam-capture-templates
		'(("d" "default"
		   plain #'org-roam-capture--get-point "%?"
		   :file-name "%<%Y-%m-%d>-${slug}"
		   :head "#+title: ${title}
#+author:
#+roam_key:

** Source



** Related

- "
		   :unnarrowed t
		   )))

  ;; (push ("d" "default" plain (function org-roam--capture-get-point)
  ;; 		 "%?"
  ;; 		 :file-name "%<%Y%m%d%H%M%S>-${slug}"
  ;; 		 :head "#+title: ${title}\n"
  ;; 		 :unnarrowed t) 'org-capture-templates)

  ;; Think about moving these to use general.el later
  ;; ex commands might be better, if these keybindings are not working well
  :bind (:map org-roam-mode-map
			  (("C-c n l" . org-roam)
			   ("C-c n f" . org-roam-find-file)
			   ("C-c n g" . org-roam-graph))
			  :map org-mode-map
			  (("C-c n i" . org-roam-insert))
			  (("C-c n I" . org-roam-insert-immediate))))

(require-package 'org-ref)
(use-package org-ref
  :config
  ;; Enable downloading PDFs / getting bibtex entries using DOI
  (require 'doi-utils)
  (require 'org-ref-arxiv)

  (setq reftex-default-bibliography '("~/personal/notes/bibliography/references.bib"))
  ;; Required for org-ref
  (setq org-ref-bibliography-notes "~/personal/notes/bibliography/notes.org"
		org-ref-default-bibliography '("~/personal/notes/bibliography/references.bib")
		org-ref-pdf-directory "~/personal/notes/bibliography/bibtex-pdfs/")

  ;; Required for helm-bibtex
  (setq bibtex-completion-bibliography "~/personal/notes/bibliography/references.bib"
		bibtex-completion-library-path "~/personal/notes/bibliography/bibtex-pdfs"
		bibtex-completion-notes-path "~/personal/notes/bibliography/helm-bibtex-notes")
  )
