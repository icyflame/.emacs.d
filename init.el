(require 'package)

;; Links to useful Emacs Wiki
;; 1. https://www.emacswiki.org/emacs/EmacsKeyNotation
;; 2. Temporary files: "#*#" and "*~"; Find commands:
;; find ~ -iname "#*#"
;; find ~ -iname "*~"
;; 3. Evil EX commands list: Display a list of ex commands with the mapped functions
;; M-x describe-variable evil-ex-commands RET

;; Scratch buffer usage: Evaluate expression (C-x C-e)
;; The result of the expression is printed to the minibuffer

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
	"f c p" 'copy-buffer
	"f y" 'copy-buffer
	)
  ;; 16. Control text size using Ctrl-Shift-+ and Ctrl-Shift-- like in other
  ;; applications
  ;; We use the characters that are typically on top of the actual characters of
  ;; these keys to force the usage of shift
  (general-create-definer ctrl-keybindings
	:states '(normal visual insert)
	)

  (ctrl-keybindings
	"s-k" 'kill-this-buffer
	"M-k" 'kill-this-buffer
	)

  (ctrl-keybindings
	:keymaps '(org-mode-map)
   "C-c l c" 'org-cycle-list-bullet
   "C-c e" 'org-table-edit-formulas
   "C-c t" 'org-show-todo-tree
   "C-a" 'org-agenda
   "C-+" 'text-scale-increase
   "C-_" 'text-scale-decrease
   )

  (general-evil-define-key '(normal visual) org-agenda-mode-map
	"F" 'org-agenda-later
	"B" 'org-agenda-earlier
	)

  ;; Elfeed Search's default mappings work are for Insert mode only
  (general-evil-define-key '(normal visual) elfeed-search-mode-map
	"RET" 'elfeed-search-show-entry
	"u" 'elfeed-search-tag-all-unread
	"r" 'elfeed-search-untag-all-unread
	"C-r" 'elfeed-update
	)

  (general-evil-define-key '(normal) elfeed-show-mode-map
	"F" 'elfeed-show-next
	"P" 'elfeed-show-prev
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
	"s c" 'kill-all-comments
	)

  (leader-def-mode
	:states '(normal visual)
	:keymaps '(go-mode-map)
	"g t" 'go-test-current-test
	"g f" 'go-test-current-file
	"g p" 'go-test-current-project
	)

  (leader-def-mode
	:states '(normal visual)
	:keymaps '(magit-mode-map)
	"g p" 'go-test-current-project

	;; magit
	"b c" 'magit-branch-create
	"b s" 'magit-branch-spinoff

	"s w" 'magit-stash-worktree

	"c o" 'magit-checkout

	"c c" 'magit-commit-create
	"c a" 'magit-commit-amend

	"f p" 'magit-fetch-all-prune
	"p p" 'magit-push-current-to-pushremote
	"m m" 'magit-merge
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

(require-package 'helm-rg)

;; 17. Get helm-projectile and bind to Ctrl-P
(require-package 'helm-projectile)
(use-package helm-projectile
  :config
  (general-nmap
	"C-p" 'helm-projectile
	"C-b" 'helm-projectile-switch-to-buffer
	"M-p" 'helm-projectile-switch-project)
  (evil-ex-define-cmd "Ag" 'helm-projectile-rg)
  (evil-ex-define-cmd "Rg" 'helm-projectile-rg))

;; 18. Org mode settings
;;; Set the done time for a TODO item when moving it to DONE
(setq org-log-done 'time)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'flyspell-mode)
(setq org-agenda-files '("~/work/notes/Monthly/Current.org" "~/personal/notes/Current.org"))

(add-hook 'org-mode-hook (lambda () (setq org-odt-preferred-output-format '"docx")))

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
  (add-hook 'go-mode-hook #'company-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode)
  (setq company-minimum-prefix-length 3))

;; 34. Protobuf mode
(require-package 'protobuf-mode)

;; 35. Yasnippets
(require-package 'yasnippet)
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  )

(load '"~/.emacs.d/machine-specific/org-roam.el")
(load '"~/.emacs.d/machine-specific/org-ref.el")

;; 36. Org capture templates
(defun create-notes-file ()
  "Create an org file in ~/notes/."
  (interactive)
  (let ((name (read-string "Filename: ")))
	(expand-file-name (format "%s-%s.org"
							  (format-time-string "%Y-%m-%d") name) "~/personal/notes/japanese")))
(setq org-capture-templates
	  '(
		("j" "Explaining a Japanese news article" plain
		 (file create-notes-file)
		 (file "~/personal/notes/japanese/template.org")
		 :unnarrowed t)
		))

;; 37. Function to kill all comments
(defun line-length ()
  "Get the length of the current line"
  (- (line-end-position) (line-beginning-position)))

(defun kill-comment-and-line ()
  "Remove the first comment on this line;
If this line is now empty, delete the line;
Move to the next line;
Return value: t when a line was killed; nil when the function simply moved to the next line"
  (interactive)
  ;; Get line length before deletion
  (setq line-length-before (line-length))
  ;; Kill first comment and move to next line
  (kill-comment 1)
  ;; Move to the original line
  (forward-line -1)
  ;; Get line length after deletion
  (setq line-length-after (line-length))
  (defun kill-and-return ()
	(kill-whole-line)
	t)
  (defun forward-and-return ()
	(forward-line)
	nil)
  (if (and (equal line-length-after 0) (not (equal line-length-before 0)))
	  (kill-and-return)
	(forward-and-return)
	)
  )

(defun kill-all-comments ()
  "Remove all comments from the active buffer"
  (interactive)
  (end-of-buffer)
  (setq last-line (line-number-at-pos))
  (beginning-of-buffer)
  (setq counter 0)
  (while (and (not (equal (line-number-at-pos) last-line)) (eq t (< counter 100)))
	(when (eq t (kill-comment-and-line))
	  (setq last-line (1- last-line)))
	(1+ counter)
	)
  ;; while loop ends when buffer is on last line; if last line is a comment, we need to delete it
  ;; too
  (kill-comment-and-line))

;; 38. Function to copy the entire buffer
(defun copy-buffer ()
  "Copy the complete buffer to the system clipboard"
  (interactive)
  (kill-new (filter-buffer-substring (point-min) (point-max)))
  nil)

(require-package 'ox-hugo)

;; 40. Elfeed configuration
(require-package 'elfeed)
(use-package elfeed
  :config
  ;; Somewhere in your .emacs file
  (setq elfeed-feeds
		'(
		  ;; News
		  ("https://fivethirtyeight.com/all/feed" fivethirtyeight us-politics)
		  ("https://rss.nytimes.com/services/xml/rss/nyt/Politics.xml" nytimes us-politics)
		  ("https://rss.nytimes.com/services/xml/rss/nyt/AsiaPacific.xml" nytimes asia)
		  ("https://www.vox.com/rss/index.xml" vox)
		  ("https://rss.politico.com/politics-news.xml" politico us-politics)
		  ("https://feeds.feedburner.com/ndtvnews-top-stories" ndtv india)
		  ))
  )

(require-package 'ob-go)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((go . t)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)))

(defun insert-current-time ()
  "Insert the current time string into the active buffer at point"
  (interactive)
  (insert-string (current-time-string)))

(defun insert-date ()
  "Insert the current date string into the active buffer at point"
  (interactive)
  (insert-string (format-time-string '"%F")))

(defun insert-time ()
  "Insert the current time string into the active buffer at point"
  (interactive)
  (insert-string (format-time-string '"%R")))
