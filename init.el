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

;;; similar to require-package but using package-install-file instead
(defun require-package-file (package file)
  "Install a package from the given location"
  (if (package-installed-p package)
	  t
	(package-install-file file)))

(package-initialize)

(require-package 'init-loader)

;; https://github.com/hanabokuro/dot-files
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
  (evil-ex-define-cmd "q" 'kill-this-buffer))

(defun kannan/ask-user-approval (prompt)
  "A function to ask the user for approval"
  (setq answer (read-char (concat prompt " " "(y/n): ")))
  (string-equal "y" (string answer)))

(defun kannan/magit/merge-upstream-into-current ()
  "Merge the upstream for this branch into this branch"
  (interactive)
  (magit-merge-plain (magit-get-upstream-branch)))

(defun kannan/magit/push-safe-to-current ()
  "Push safely to the upstream branch of the current branch. Ask user before pushing to master"
  (interactive)
  (if (or (not (string-equal "master" (magit-get-current-branch)))
		  (eq t (kannan/ask-user-approval "Push to upstream on master?")))
	  (call-interactively #'magit-push-current-to-pushremote)))

(defun kannan/magit/checkout-previous-branch ()
  "Checkout the previous branch"
  (interactive)
  (magit-checkout (magit-get-previous-branch)))

(defun kannan/magit/checkout-default-branch ()
  "Checkout the default branch"
  (interactive)
  (magit-checkout '"master"))

(defun kannan/magit/rebase-previous-branch ()
  "Rebase current branch on the previous branch"
  (interactive)
  (magit-rebase-branch (magit-get-previous-branch) ()))

(defun kannan/buffer/switch-to-scratch-buffer ()
  "Switch to scratch buffer in the current buffer. Usefule when trying to focus on a single buffer
and empty out everything else around it"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; 12. Install general package
(require-package 'general)
(use-package general
  :config
  ;; 13. Add key mappings for common actions using general
  (general-evil-setup)
  (general-nmap
	"DEL" 'evil-ex-nohighlight
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
	"C-h" 'evil-window-left
	"C-j" 'evil-window-down
	"C-k" 'evil-window-up
	"C-l" 'evil-window-right

	"C-+" 'text-scale-increase
	"C-_" 'text-scale-decrease

	"M-k" 'kill-this-buffer
	"M-e" 'kannan/buffer/switch-to-scratch-buffer

	"C-a" 'org-agenda
	"C-x o c" 'org-capture

	"M-g" 'magit-status
	"C-x g" 'magit-status

	"C-x e p" 'eval-print-last-sexp
	"C-x e b" 'eval-buffer

	"M-d" 'preview-plantuml-now

	"C-x e f" 'elfeed
	"C-x n m" 'notmuch)

  (ctrl-keybindings
	:keymaps '(org-mode-map)
   "C-c l c" 'org-cycle-list-bullet
   "C-c e" 'org-table-edit-formulas
   "C-c t" 'org-show-todo-tree
   )

  (ctrl-keybindings
	:keymaps '(magit-mode-map)
	"M-=" 'magit-refresh-all
	)

  (general-evil-define-key '(normal visual) org-agenda-mode-map
	"M-n" 'org-agenda-later
	"M-p" 'org-agenda-earlier
	)

  ;; Elfeed Search's default mappings work are for Insert mode only
  (general-evil-define-key '(normal visual) elfeed-search-mode-map
	"RET" 'elfeed-search-show-entry
	"u" 'elfeed-search-tag-all-unread
	"r" 'elfeed-search-untag-all-unread
	"f" 'elfeed-search-set-filter
	"M-=" 'elfeed-update
	)

  ;; Elfeed Search's default mappings work are for Insert mode only
  (ctrl-keybindings
	:keymaps '(biblio-selection-mode-map)
	"RET" 'biblio--selection-insert-quit
	)

  (general-evil-define-key '(normal) elfeed-show-mode-map
	"M-n" 'elfeed-show-next
	"M-p" 'elfeed-show-prev
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

	;;; common
	"w" 'evil-write
	"s c" 'kill-all-comments

	;;; magit

	"b c" 'magit-branch-create
	"b s" 'magit-branch-spinoff
	"b d" 'magit-branch-delete
	"b r" 'magit-branch-rename

	"c P" 'magit-cherry-pick
	"c a" 'magit-cherry-apply

	"c o" 'magit-checkout
	"c p" 'kannan/magit/checkout-previous-branch
	"c d" 'kannan/magit/checkout-default-branch

	"c c" 'magit-commit-create
	"c a" 'magit-commit-amend

	"c p" 'magit-cherry-pick

	"f p" 'magit-fetch-all-prune

	"m m" 'magit-merge
	"m u" 'kannan/magit/merge-upstream-into-current

	"p p" 'kannan/magit/push-safe-to-current

	"r b" 'magit-rebase
	"r p" 'kannan/magit/rebase-previous-branch

	"r v c" 'magit-revert-and-commit
	"r v n" 'magit-revert-no-commit

	"s h" 'magit-show-commit

	"s w" 'magit-stash-worktree
	"s b" 'magit-stash-both
	"s p" 'magit-stash-pop
	"s a" 'magit-stash-apply
	"s d" 'magit-stash-drop)

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
	)
  )

;; 5. Set the color scheme to solarized dark
(require-package 'solarized-theme)
;; (use-package solarized-theme
;;   :init
;;   (load-theme 'solarized-dark t)
;;   )

;; 5.1 Set the color scheme to Nord (trial period)
(require-package 'nord-theme)
(use-package nord-theme
  :init
  (load-theme 'nord t))

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
(setq org-agenda-files '(
						 ;; files at work
						 "~/work/notes/TODO.org"
						 "~/work/notes/Current.org"
						 ;; files at home
						 "~/personal/notes/TODO.org"
						 "~/personal/notes/Current.org"
						 "~/personal/notes/Current.org"))

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

;; 32. Magit
(require-package 'magit)

;; Language server protocol client
(require-package 'lsp-mode)
;; 33. Comp(lete) any(thing)
(require-package 'company)
;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :hook ((go-mode emacs-lisp-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 3))

;; 34. Protobuf mode
(require-package 'protobuf-mode)

;; 35. Yasnippets
(require-package 'yasnippet)
(use-package yasnippet
  :hook ((org-mode go-mode) . #'yas-minor-mode)
  :config
  (yas-reload-all))

;; 36. Org capture templates
(defun create-notes-file ()
  "Create an org file in ~/notes/."
  (interactive)
  (let ((name (read-string "Filename: ")))
	(expand-file-name (format "%s-%s.org"
							  (format-time-string "%Y-%m-%d") name) "~/personal/notes/japanese")))
(defun create-blog-file ()
  "Create an org file in ~/blog/."
  (interactive)
  (let ((name (read-string "Filename: ")))
	(expand-file-name (format "%s-%s.org"
							  (format-time-string "%Y-%m-%d") name) "~/personal/blog/posts-org")))

(defun is-work-computer ()
  "Return t or nil depending on whether this is a work computer or not"
  (let ((home-computers '("home-thinkpad")))
	(not (seq-contains-p home-computers (system-name)))))

(if (not (is-work-computer))
	((lambda ()
	  (load '"~/.emacs.d/machine-specific/org-roam.el")
	  (load '"~/.emacs.d/machine-specific/org-ref.el"))))

(setq default-todo-file-for-computer (if (is-work-computer) '"~/work/notes/TODO.org" '"~/personal/notes/TODO.org"))

(setq org-capture-templates
	  '(("b" "Blog post" plain
		 (file create-blog-file)
		 (file "~/personal/blog/posts-org/template.org")
		 :prepend t
		 :jump-to-captured t
		 :unnarrowed t)

		("t" "Todo" entry (file+headline default-todo-file-for-computer "Tasks")
		 "* TODO %?\n  %i\n  %a")

		("r" "Add a recommendation to the recommendations list" checkitem
		 (file "~/personal/notes/RecommendationsList.org")
		 "- [ ] %^{Title}
  - *Date added to this list:* %T
  - *Source:* %^{Source}
  - *Author:* %^{Author (if known)}
  - *Note:* %?")

		("j" "Explaining a Japanese news article" plain
		 (file create-notes-file)
		 (file "~/personal/notes/japanese/template.org")
		 :jump-to-captured t
		 :unnarrowed t)))

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
		  ("https://fivethirtyeight.com/all/feed" news fivethirtyeight us-politics)
		  ("https://rss.nytimes.com/services/xml/rss/nyt/Politics.xml" news nytimes us-politics)
		  ("https://rss.nytimes.com/services/xml/rss/nyt/AsiaPacific.xml" news nytimes asia)
		  ("https://www.vox.com/rss/index.xml" news vox)
		  ("https://rss.politico.com/politics-news.xml" news politico us-politics)
		  ("https://feeds.feedburner.com/ndtvnews-top-stories" news ndtv asia india)

		  ("https://www.bloomberg.com/opinion/authors/AQwaMsNcwy0/matthew-g-yglesias.rss" blogs bloomberg)
		  ("https://www.bloomberg.com/opinion/authors/ARbTQlRLRjE/matthew-s-levine.rss" blogs bloomberg)

		  ("https://www.theatlantic.com/feed/author/zeynep-tufekci/" blogs)
		  ("https://zachholman.com/atom.xml" blogs tech)
		  ("https://kazeburo.hatenablog.com/feed" blogs tech)
		  ("https://blog.jessfraz.com/index.xml" blogs tech)
		  ("https://daniel.haxx.se/blog/feed/" blogs tech)
		  ("https://pluralistic.net/feed/" blogs tech links)

		  ("https://ppsreejith.net/index.xml" blogs friends)
		  ("https://code.ghostwriternr.me/feed.xml" blogs friends)
		  ("https://notes.ppsreejith.net/index.xml" blogs friends)
		  ("https://www.xypnox.com/blag/atom.xml" blogs friends)
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
  (insert-string (format-time-string '"%F %H:%M:%S %Z")))

(defun insert-date ()
  "Insert the current date string into the active buffer at point"
  (interactive)
  (insert-string (format-time-string '"%F")))

(defun insert-time ()
  "Insert the current time string into the active buffer at point"
  (interactive)
  (insert-string (format-time-string '"%R:%S")))

(require-package-file 'awesome-tab "~/.emacs.d/lisp/awesome-tab")
(use-package awesome-tab
  :ensure t
  :config
  (awesome-tab-mode t))

;; Elisp functions to unwrap text. Useful when to copying Org markup into text input fields on
;; browsers which will not be formatted before display
(defun unwrap-all (file)
  "Unwrap all the paragraphs in the given file and write to (basename).unwrapped.(extension|).

This function will leave the buffer with the unwrapped text open. The user can switch to that buffer
after the function runs if they want to look at the contents.

E.g. 1. /.../test-file.ext => /.../test-file.unwrapped.ext
E.g. 2. /.../test-file => /.../test-file.unwrapped

This function will handle list items properly (i.e. separate list items will not be unwrapped into
each other). Works well when the file has Org markup with plain paragraphs and nested lists, with
titles.

Note: This will not work if the file has Org tables
"
  (setq output-file-name (concat file ".unwrapped"))
  (unless (eq nil (string-match "\\." file))
	(setq comps (reverse (split-string file "\\.")))
	(setq extension (pop comps))
	(setq comps (reverse comps))
	(setq output-file-name (concat (string-join comps ".") ".unwrapped" "." extension)))

  ;; Open the requested file in a buffer
  (setq old-buffer (find-file-noselect file))
  ;; Split and prepare new buffer
  (setq new-buffer (find-file-noselect output-file-name))
  ;; Insert old buffer into new buffer and edit the new buffer
  (with-current-buffer new-buffer
	(erase-buffer)
	(insert-buffer old-buffer)
	;; Prepare tracker variable to keep track if previous line allows indentation or not
	(setq d nil)
	;; Iterate over the complete buffer, starting at the beginning
	(while (not (eq (point-at-eol) (point-max)))
	  (beginning-of-line)
	  (setq is-line-empty (looking-at "^$"))

	  (if is-line-empty (setq d nil)
		(setq start-char (char-after))
		(setq is-header-line (eq start-char "*"))
		(setq is-list-start-line (org-list-at-regexp-after-bullet-p '""))
		(if (and
			 (not is-header-line)
			 (not is-list-start-line)
			 (not is-line-empty)
			 (eq d t))
			(delete-indentation))
		(if (not is-header-line) (setq d t)))
	  (forward-line 1))
	;; Write to the output file and leave the buffer open for the user
	(write-file output-file-name))

  (message "Unwrapped and written to %s" output-file-name))

(defun unwrap-current ()
  "Unwrap all paragraphs in the current file and write to (basename).unwrapped.(extension)"
  (interactive)
  (unwrap-all (buffer-file-name)))

(require-package 'web-mode)

(require-package 'notmuch)
(use-package notmuch
  :config
  (general-nmap
	"t" 'notmuch-tree-tag))

(defun get-conversion-rate (from to)
  "Get the conversion rate between any two currencies using a free exchange rate provider

Current implementation uses ratesapi.io. API documentation: https://ratesapi.io/documentation/

This function will return a floating point value"
  (setq base-cmd "curl 'https://api.ratesapi.io/api/latest?base=%s&symbols=%s' -s | jq '.rates.%s'")
  (setq actual-cmd (format base-cmd from to to))
  (message actual-cmd)
  (string-to-number (shell-command-to-string actual-cmd)))

(defun convert-currencies ()
  "Convert currencies is a function which will ask a series of questions and convert amounts
from one currency to another"
  (interactive)
  (let ((from-amount (string-to-number (read-string "From amount? ")))
		(from (read-string "From currency? "))
		(to (read-string "To currency? ")))
	(setq to-amount (* from-amount (get-conversion-rate from to)))
	(message "%0.2f %s = %0.2f %s" from-amount from to-amount to)))

(require-package 'atomic-chrome)
(atomic-chrome-start-server)

(require-package 'org-journal)
(use-package org-journal
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/personal/notes/journal/"
        org-journal-date-format "%A, %d %B %Y"))

(require-package 'nov)
(use-package nov
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (setq nov-text-width 80)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
