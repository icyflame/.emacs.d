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

(defun is-work-computer ()
  "Return t or nil depending on whether this is a work computer or not"
  (let ((home-computers '("home-thinkpad")))
	(not (seq-contains-p home-computers (system-name)))))

(defun is-personal-computer ()
  "Return t or nil depending on whether this is a personal computer or not"
  (not (is-work-computer)))

(setq notes-directory (if (is-work-computer) '"~/work/notes/" '"~/personal/notes/"))
(defun notes-directory-file (filename)
  "Return the path to filename when placed inside the notes-directory"
  (concat notes-directory filename))

(setq default-todo-file-for-computer (notes-directory-file '"TODO.org"))

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

(defun kannan/magit/delete-branch (branch)
  (magit-run-git "branch" "-d" branch))

(defun kannan/magit/delete-all-merged-branches ()
  "Delete all branches that have been merged into the current branch"
  (interactive)
  (let ((current-branch (magit-get-current-branch)))
	(if (or (string-equal "master" current-branch)
			(eq t (kannan/ask-user-approval "Delete merged branches, even though we are not on master?")))
		(let ((branches-to-delete (delete current-branch (magit-list-merged-branches))))
		  (mapc #'kannan/magit/delete-branch branches-to-delete)
		  (message "Deleted all branches: %s" (string-join branches-to-delete ", "))))))

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

	"C-+" 'increase-emacs-font-size
	"C-_" 'decrease-emacs-font-size

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
	"C-c e" 'org-table-edit-formulas

	"C-c l c" 'kannan/org/copy-link-to-clipboard
	"C-c l r" 'kannan/org/replace-link-from-clipboard
	"C-c l s" 'kannan/org/show-link
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
	"a s" 'kannan/convert-region-to-ascii
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

  ;; 23. SQL format highlighted region
  (leader-def-mode
	:states '(normal visual)
	:keymaps '(sql-mode-map)
	"g q" 'kannan/sql-pretty-print
	"g s" 'kannan/sql-single-line
	)
  )

;; 5.2 Set the color scheme to Tomorrow Night - Bright (trial)
(require 'color-theme-tomorrow)
(color-theme-tomorrow--define-theme night)
(enable-theme 'tomorrow-night)

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
  (evil-ex-define-cmd "P" 'helm-projectile-switch-project)
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
(require-package 'lsp-ui)
(use-package lsp-ui)

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
	"g i" 'lsp-treemacs-implementations
	"g r" 'lsp-treemacs-references
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

(load '"~/.emacs.d/machine-specific/org-roam.el")
(if (not (is-work-computer))
	((lambda ()
	  (load '"~/.emacs.d/machine-specific/org-ref.el"))))

(if (and
	 (is-work-computer)
	 (x-list-fonts "Menlo 14")) (set-frame-font "Menlo 14" nil t))

(setq org-capture-templates '())
(add-to-list 'org-capture-templates
			 '("j" "Explaining a Japanese news article" plain
			   (file create-notes-file)
			   (file (lambda () (notes-directory-file '"japanese/template.org")))
			   :jump-to-captured t
			   :unnarrowed t))

(if (is-personal-computer)
	(add-to-list 'org-capture-templates
				 '("b" "Blog post" plain
				   (file create-blog-file)
				   (file "~/personal/blog/posts-org/template.org")
				   :prepend t
				   :jump-to-captured t
				   :unnarrowed t)))

(add-to-list 'org-capture-templates
			 '("r" "Add a recommendation to the recommendations list" checkitem
			   (file (lambda () (notes-directory-file '"RecommendationsList.org")))
			   "- [ ] %^{Title}
- *Date added to this list:* %T
- *Source:* %^{Source}
- *Author:* %^{Author (if known)}
- *Link:* %^{Link (if known)}
- *Tags:* %^{Tags (if required)}
- *Note:* %?"))

(add-to-list 'org-capture-templates
			 '("t" "Todo" entry (file+headline default-todo-file-for-computer "Tasks")
			   "* TODO %?\n  %i\n  %a"))

(add-to-list 'org-capture-templates
			 '("w" "Wordle" entry (file "~/work/notes/org-roam/2022-01-21-wordle_turn_history.org")
			   "* %<%Y-%m-%d>

%x

"
			   :prepend t
			   :no-save t
			   :jump-to-captured t
			   :unnarrowed t))


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

  (message output-file-name)

  ;; Open the requested file in a buffer
  (setq old-buffer (find-file-noselect file))
  ;; Split and prepare new buffer
  (setq new-buffer (find-file-noselect output-file-name))
  ;; Insert old buffer into new buffer and edit the new buffer
  (with-current-buffer new-buffer
	(erase-buffer)
	(insert-buffer old-buffer)
	;; Prepare tracker variable to keep track if previous line allows indentation or not
	(setq does-previous-line-allow-indentation nil)
	;; Iterate over the complete buffer, starting at the beginning
	(while (not (eq (point-at-eol) (point-max)))
	  (beginning-of-line)
	  (setq is-line-empty (looking-at "^$"))

	  (setq start-char (char-after))
	  (setq is-header-line (eq start-char (string-to-char '"*")))
	  (setq is-list-start-line (org-list-at-regexp-after-bullet-p '""))
	  (setq is-property-definition (looking-at "^#\\+"))
	  (setq is-block-end (looking-at "^#\\+end"))
	  (setq is-block-begin (looking-at "^#\\+begin"))

	  (if is-line-empty
		  (setq does-previous-line-allow-indentation nil)
		(if (and
			 (not (org-in-src-block-p))
			 (not is-property-definition)
			 (not is-block-end)
			 (not is-header-line)
			 (not is-list-start-line)
			 (not is-line-empty)
			 (eq does-previous-line-allow-indentation t))
			(delete-indentation))

		;; Allow indentation on next line if this line is neither a header and nor a block begin
		(setq does-previous-line-allow-indentation (and (not is-block-begin) (not is-header-line))))

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
	"M-a" 'notmuch-show-archive-message-then-next-or-next-thread))

(require-package 'atomic-chrome)
(atomic-chrome-start-server)

(require-package 'org-journal)
(use-package org-journal
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir (notes-directory-file "journal/")
        org-journal-date-format "%A, %d %B %Y"))

(require-package 'nov)
(use-package nov
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  (setq nov-text-width 80)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(defun kannan/golang-download-dependncies ()
  "This function will download dependencies using gomods"
  (interactive)
  (start-process "download-go-dependencies" "*Go mods*" "go" "mod" "vendor"))

(defun afs/org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(require 'simpleclip)
(defun kannan/org/replace-link-from-clipboard ()
    "Replace an Org link with the same description and the link from the clipboard

Adapted from afs/org-replace-link-by-link-description"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
		  (let
			  ((new-link (simpleclip-get-contents)))
			(org-insert-link nil new-link description)
			(message new-link))))))

(defun kannan/org/copy-link-to-clipboard ()
    "Replace an Org link with the same description and the link from the clipboard

Adapted from afs/org-replace-link-by-link-description"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
	  (save-excursion
		(let ((link (org-match-string-no-properties 1)))
		  (simpleclip-set-contents link)
		  (message link)))))

(defun kannan/org/show-link ()
    "Replace an Org link with the same description and the link from the clipboard

Adapted from afs/org-replace-link-by-link-description"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
	  (save-excursion
		(let ((link (org-match-string-no-properties 1)))
		  (message link)))))

;; ============================================
;; Coldnew's Font Size Conf for Org-Table
;; ============================================
;;
;; Use narrow fonts, which ensure that column width is always the same.
;;
;; Source: https://github.com/kuanyui/.emacs.d/blob/11b67bbeb00b254f4504c73c7c8bf6dad2a8e53e/rc/rc-basic.el#L168
;; Answers: https://emacs.stackexchange.com/a/59700, https://emacs.stackexchange.com/a/10474
;; Original blog post: https://coldnew.github.io/d5011be2/
;; 特殊字型設定
(defun get-screen-pixel-density ()
  "Return nil on terminal.
Otherwise, return DPI (1 inch = 2.54 cm)
"
  (let* ((screen0 (car (display-monitor-attributes-list)))
         (mm (alist-get 'mm-size screen0))
         (px (alist-get 'geometry screen0))
         (w-mm (nth 0 mm))
         (w-px (nth 2 px))
         )
    (if (eq w-mm nil)
        nil
      (* 25.4 (/ w-px (float w-mm)))
      )))

(when (window-system)
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Consolas-9"))
  (if (eq system-type 'windows-nt)
      (setq emacs-cjk-font "Consolas"
            emacs-english-font "Consolas"))

  (defvar emacs-english-font "DejaVu Sans Mono" "The font name of English.")
  (defvar emacs-cjk-font "Noto Sans CJK JP" "The font name for CJK.")
  ;;; for test
  ;; (find-font (font-spec :name "LiHei Pro"))
  ;; (font-family-list)

  (defvar emacs-font-size-pair '(17 . 20)
    "Default font size pair for (english . chinese)")

  ;; Auto adjust font-size for Hi-res screen
  (let ((dpi (get-screen-pixel-density)))
    (setq emacs-font-size-pair
          (cond
           ((eq dpi nil) (error "This should not be executed under terminal."))
           ((> dpi 150) '(24 . 28))
           (t '(17 . 20))
           )))

  (defvar emacs-font-size-pair-list
    '(( 5 .  6) (9 . 10) (10 . 12)(12 . 14)
      (13 . 16) (15 . 18) (17 . 20) (19 . 22)
      (20 . 24) (21 . 26) (24 . 28) (26 . 32)
      (28 . 34) (30 . 36) (34 . 40) (36 . 44))
    "This list is used to store matching (english . chinese) font-size.")

  (defun font-exist-p (fontname)
    "Test if this font is exist or not."
    (if (or (not fontname) (string= fontname ""))
        nil
      (if (not (x-list-fonts fontname)) nil t)))

  (defun set-font (english chinese size-pair)
    "Setup emacs English and Chinese font on x window-system."

    (if (font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

    (if (font-exist-p chinese)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family chinese :size (cdr size-pair))))))

  ;; Setup font size based on emacs-font-size-pair
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

  (defun emacs-step-font-size (step)
    "Increase/Decrease emacs's font size."
    (let ((scale-steps emacs-font-size-pair-list))
      (if (< step 0) (setq scale-steps (reverse scale-steps)))
      (setq emacs-font-size-pair
            (or (cadr (member emacs-font-size-pair scale-steps))
                emacs-font-size-pair))
      (when emacs-font-size-pair
        (message "emacs font size set to %.1f" (car emacs-font-size-pair))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

  (defun increase-emacs-font-size ()
    "Decrease emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size 1))

  (defun decrease-emacs-font-size ()
    "Increase emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size -1))
  )

(defun kannan/convert-region-to-ascii (beg end &optional arg)
  "Convert the highlighted region to ASCII through transliteration offered by iconv.

iconv should be present on the host.

I don't know what the third argument to this function is, but not having the third argument
causes the function to throw an error when this function is executed from visual mode.
"
  (interactive "r\nP")
  (shell-command-on-region beg end '"iconv --to ascii//translit" nil t)
  (message "where I %d you %d" end beg))

(require-package 'olivetti)

(defun kannan/sql-pretty-print ()
  "Pretty print the SQL query in the current buffer

pg_format is a binary that can be built using Perl 5. The code for this binary is
available here: https://github.com/darold/pgFormatter
"
  (interactive)
  (shell-command-on-region (point-min) (point-max) '"pg_format -f 2 -" nil t))

(defun kannan/sql-single-line ()
  "Convert the SQL query in this buffer into a single line query

This is required for some MySQL command line clients which don't support multiline
SQL queries.
"
  (interactive)
  (delete-indentation nil (point-min) (point-max)))

(init-loader-load "~/.emacs.d/local-confs")

;; Japanese language input using Mozc
;; On Ubuntu: apt-get install emacs-mozc
;; This should put mozc.el inside /usr/share/emacs/site-lisp
;; And it should also put mozc_emacs_helper in the $PATH (maybe /usr/bin)
;; Link to the mozc.el file which is under ~/.emacs.d/lisp:
;;     https://github.com/google/mozc/blob/master/src/unix/emacs/mozc.el

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :hook (eshell-mode-hook)
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;; Japanese language input using Mozc
;;
;; Step 1: Install mozc server
;;     On Ubuntu: apt-get install emacs-mozc
;;
;;     This should put mozc.el inside /usr/share/emacs/site-lisp
;;     and it should also put ~mozc_emacs_helper~ in the $PATH (maybe /usr/bin)
;;
;; Step 2: Download the mozc.el file which makes Emacs a client of the Mozc server
;;
;;     Put this file inside ~/.emacs.d/lisp.
;;
;;     Link to the mozc.el file:
;;         https://github.com/google/mozc/blob/master/src/unix/emacs/mozc.el
;;
;; Switch input methods using C-\. When in the "Insert" mode, Japanese can be added to the
;; buffer. When in other modes, Emacs keybindings will continue to work as usual.
(require 'mozc)
(setq default-input-method "japanese-mozc")
