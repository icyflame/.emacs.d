;; 0. Disable audible bell and all related sounds that could come from Emacs. Turn this off
;; immediately after Emacs starts. Even if we can't get any package, the audible bell shouldn't
;; sound.
(setq ring-bell-function (lambda () ()))

;; Links to useful Emacs Wiki
;; 1. https://www.emacswiki.org/emacs/EmacsKeyNotation
;; 2. Temporary files: "#*#" and "*~"; Find commands:
;; find ~ -iname "#*#"
;; find ~ -iname "*~"
;; 3. Evil EX commands list: Display a list of ex commands with the mapped functions
;; M-x describe-variable evil-ex-commands RET
;; 4. Org table cell references: https://orgmode.org/manual/References.html
;;    @ROW$COLUMN
;;    @1..2$3 OR @1$1..@2$4
;;    @1..>$4 => Column 4, rows 1 to the last row
;;    @>$2=vsum(@-II..@-1$2) => Column 2 of last row is the sum of all the rows from
;;                              the first row under previous HLINE to the last but one
;;                              row

;; Scratch buffer usage: Evaluate expression (C-x C-e)
;; The result of the expression is printed to the minibuffer

;; car is the first element of a cons cell, and cdr is the second element

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                            ("melpa" . "https://melpa.org/packages/")
                            ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                            ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; TODO: Replace .emacs.d with the variable `user-emacs-directory'. This will make it possible to
;; start emacs using emacs --init-directory in order to test a new configuration
;; https://emacsredux.com/blog/2024/02/23/changing-the-emacs-configuration-directory/

;; 1. Install use-package using package.el. This should be the only package which does not use use-package.
(require 'use-package)

(use-package init-loader
    :ensure t
    :config
    ;; https://github.com/hanabokuro/dot-files
    (setq init-loader-default-regexp "\\(?:^[[:digit:]]\\{1\\}\\).*\\.el\$") ;; default だと *.el~ も対象になってしまう。
    (init-loader-load "~/.emacs.d/imported-confs")
    (init-loader-load "~/.emacs.d/local-confs")
    (init-loader-load "~/.emacs.d/separated-confs"))

(use-package emacs
    :config
    ;; 1. Don't show splash screen at start-up
    (setq inhibit-splash-screen t)

	;; 6. Move everything defined for the customize system to a separate file
	;; TODO: What use does the customize system provide? Should I even be using it?
	(setq custom-file "~/.emacs.d/custom.el")
	(load custom-file)

    ;; 26. Remove trailing whitespace characters from all files
    (add-hook 'before-save-hook 'delete-trailing-whitespace)

    (setq org-capture-templates '())

    (if (boundp 'notes-directory)
        (when (null (string-suffix-p '"/" notes-directory))
            (setq notes-directory (format '"%s/" notes-directory))))

    (if (boundp 'blog-location)
        (when (null (string-suffix-p '"/" blog-location))
            (setq blog-location (format '"%s/" blog-location))))

    (defun notes-directory-file (filename)
        "Return the path to filename when placed inside the notes-directory"
        (concat notes-directory filename))

    (setq default-todo-file-for-computer (notes-directory-file '"TODO.org"))

    (defun kannan/buffer/switch-to-scratch-buffer ()
        "Switch to scratch buffer in the current buffer. Useful when I want to focus on a single buffer
and remove everything else from the screen"
        (interactive)
        (switch-to-buffer "*scratch*"))

    ;; Don't use C-M-s for anything other than "OS-level search"
    (unbind-key '"C-M-s")

	(setq-default fill-column 100)

	;; 8. Don't blink cursor
	(blink-cursor-mode 0)

	;; 20. Set the default width of a tab character
	(setq-default tab-width 4)

    (defun kannan/ask-user-approval (prompt)
        "A function to ask the user for approval"
        (setq answer (read-char (concat prompt " " "(y/n): ")))
        (string-equal "y" (string answer))))

(use-package async
    :defer t
    :ensure t)

;; 3. Evil mode across most of Emacs
(use-package evil
    :ensure t
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
    (evil-ex-define-cmd "q" 'kill-current-buffer))

;; 12. Install general package
(use-package general
    :ensure t
    :config
    ;; 13. Add key mappings for common actions using general
    (general-evil-setup)
    (general-nmap
        "DEL" 'evil-ex-nohighlight
        "f y" 'copy-buffer)

    ;; 16. Control text size using Ctrl-Shift-+ and Ctrl-Shift-- like in other
    ;; applications
    ;; We use the characters that are typically on top of the actual characters of
    ;; these keys to force the usage of shift
    (general-create-definer ctrl-keybindings
        :states '(normal visual insert))

    (ctrl-keybindings
        "C-h" 'evil-window-left
        "C-j" 'evil-window-down
        "C-k" 'evil-window-up
        "C-l" 'evil-window-right

        "C-+" 'increase-emacs-font-size
        "C-_" 'decrease-emacs-font-size

        "M-k" 'kill-current-buffer
        "M-e" 'kannan/buffer/switch-to-scratch-buffer

        "M-b" 'switch-to-buffer
        "C-b" 'projectile-switch-to-buffer
        "C-p" 'projectile-find-file

        "C-s" 'swiper
        "C-g" 'projectile-ripgrep

        "C-a" 'org-agenda
        "C-x o c" 'org-capture

        "C-x s c" 'flyspell-buffer
        "C-x n e" 'flyspell-goto-next-error

        "M-g" 'magit-status
        "C-x g" 'magit-status

        "C-x e p" 'eval-print-last-sexp
        "C-x e b" 'eval-buffer

        "M-d" 'preview-plantuml-now
        "C-c n f" 'org-roam-node-find

        "C-c g l" 'git-link

        "C-x e s" 'eshell
        "C-x i v" 'ivy-locate-replacement-helper

        "C-x n m" 'notmuch
        "C-x n j" 'notmuch-jump-search
        "C-x n s" 'notmuch-search)

    (ctrl-keybindings
        :keymaps '(org-mode-map)
        "TAB" 'org-cycle
        "C-c e" 'org-table-edit-formulas

        "C-c l c l" 'kannan/org/copy-link-to-clipboard
        "C-c l c d" 'kannan/org/copy-description-to-clipboard
        "C-c l r" 'kannan/org/replace-link-from-clipboard
        "C-c l d" 'afs/delete-link-at-point
        "C-c l s" 'kannan/org/show-link
        "C-c n i" 'org-roam-node-insert
        "C-c n l" 'org-roam-buffer-toggle)

    (ctrl-keybindings
        :keymaps '(markdown-mode-map)
        "C-c C-x C-v" 'markdown-toggle-inline-images)

    (ctrl-keybindings
        :keymaps '(magit-mode-map)
        "M-=" 'magit-refresh-all)

    (general-evil-define-key '(normal visual insert) org-agenda-mode-map
        "M-n" 'org-agenda-later
        "M-p" 'org-agenda-earlier
        "M-." 'kannan/org-agenda/schedule-today
        "M-+" 'kannan/org-agenda/schedule-tomorrow)

    (ctrl-keybindings
        :keymaps '(biblio-selection-mode-map)
        "RET" 'biblio--selection-insert-quit)

    ;; 24. Keybindings that use the leader key functionality in normal and visual mode
    (general-create-definer leader-def-mode
        :prefix ",")

    (leader-def-mode
        :states '(normal)
        "c SPC" 'comment-line)

    (leader-def-mode
        :states '(visual)
        "c SPC" 'comment-or-uncomment-region
        "c s" 'comment-or-uncomment-region
        "a s" 'kannan/convert-region-to-ascii)

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
        "p r" 'magit-process-buffer
        "p l" 'magit-pull-from-upstream

        "r b" 'magit-rebase
        "r p" 'kannan/magit/rebase-previous-branch

        "r v c" 'magit-revert-and-commit
        "r v n" 'magit-revert-no-commit

        "s h" 'magit-show-commit

        ;; stash the unstaged changes only
        "s w" 'magit-stash-worktree
        ;; stash the staged changes only
        "s i" 'magit-stash-index
        ;; stash both staged and unstaged changes
        "s b" 'magit-stash-both

        "s p" 'magit-stash-pop
        "s a" 'magit-stash-apply
        "s d" 'magit-stash-drop)

    (leader-def-mode
        :states '(normal visual)
        :keymaps '(go-mode-map)
        "g t" 'go-test-current-test
        "g f" 'go-test-current-file
        "g p" 'go-test-current-project)

    (leader-def-mode
        :states '(normal visual)
        :keymaps '(magit-mode-map)
        "g p" 'go-test-current-project)

    ;; 23. SQL format highlighted region
    (leader-def-mode
        :states '(normal visual)
        :keymaps '(sql-mode-map)
        "g q" 'kannan/sql-pretty-print
        "g s" 'kannan/sql-single-line)

    (leader-def-mode
        :states '(normal visual)
        :keymaps '(perl-mode-map)
        "d c" 'cperl-perldoc-at-point))

;; 5.2 Set the color scheme to Tomorrow Night - Bright
(use-package color-theme-tomorrow
  :load-path "lisp/"
  :config
  (color-theme-tomorrow--define-theme night)
  (color-theme-tomorrow--define-theme night-bright)
  (color-theme-tomorrow--define-theme day)
  (enable-theme 'tomorrow-night-bright))

;; 7. Go mode settings
(use-package go-mode
    :ensure t
    :defer t
    :config
    ;; Set up before-save hooks to format buffer and add/delete imports.
    ;; Make sure you don't have other gofmt/goimports hooks enabled.
    (defun lsp-go-install-save-hooks ()
        (add-hook 'before-save-hook #'lsp-format-buffer t t)
        (add-hook 'before-save-hook #'lsp-organize-imports t t))

    (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package gotest
    :ensure t
    :defer t)

;; 9. Use Ivy instead of helm because it is fast
(use-package ivy
    :ensure t
    :hook
    (after-init . ivy-mode))

;; 9.1. Use Ivy-prescient to ensure that the sorting and filtering is done based on the history of
;; command usage.
(use-package ivy-prescient
    :ensure t
    :config
    ;; persist the weights of various functions between Emacs sessions
    ;; the history is saved at ~/.emacs.d/var/prescient-save.el
    (setq prescient-persist-mode t)
    ;; character folding prevents the use of "long" regular expressions which are usually generated by packages like ivy
    ;; https://github.com/radian-software/prescient.el/issues/113#issuecomment-924853224
    ;; https://github.com/radian-software/prescient.el/issues/71#issuecomment-703646491
    (setq prescient-use-char-folding nil)
    :hook
    (after-init . prescient-persist-mode)
    (after-init . ivy-prescient-mode))

;; 9.2
(use-package swiper
    :ensure t
    :config
    (copy-face 'region 'swiper-line-face))

(use-package ripgrep
    :defer t
    :ensure t)

(use-package rg
    :defer t
    :ensure t)

(use-package projectile
    :defer t
    :ensure t
    :hook
    (after-init . projectile-mode)
    :config
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ivy)
    (setq projectile-sort-order 'modification-time)
    ;; This workaround is required if fd's version is below 8.3.0
    ;; This is because the option "--strip-cwd-prefix" is used by projectile.
    ;; See comment from the author of projectile:
    ;; https://github.com/bbatsov/projectile/issues/1788#issuecomment-1484024829
    ;; Related work-around which I tried earlier:
    ;; https://github.com/juergenhoetzel/projectile/commit/383b3bf47d34ca60c24cd73ea9c335936d0b70be
    (setq projectile-git-use-fd nil))

;; 11. Install markdown mode
(use-package markdown-mode
    :defer t
    :ensure t
    :mode
    (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . gfm-mode)
        ("\\.notes\\'" . gfm-mode)
        ("\\.markdown\\'" . gfm-mode))
    :init
    (add-hook 'markdown-mode-hook #'auto-fill-mode)
    (setq markdown-command "multimarkdown")
    (setq markdown-open-command "firefox"))

(use-package lsp-ui
    :defer t
    :ensure t)

;; https://github.com/leoliu/ggtags
(use-package ggtags
    :defer t
    :ensure t
    :config
    (general-nmap
        :keymaps '(c-mode-map)
        "g t" 'ggtags-find-definition
        "g d" 'ggtags-find-tag-dwim)
    (add-hook 'c-mode-common-hook
        (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)))))

(use-package lsp-mode
    :defer t
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :config
    (setq lsp-enable-file-watchers nil)
    (general-nmap
        :keymaps '(go-mode-map)
        "g t" 'lsp-goto-type-definition
        "g d" 'lsp-find-definition
        "g i" 'lsp-find-implementation
        "g r" 'lsp-find-references
        "C-]" 'lsp-find-definition
        )
    (lsp-register-custom-settings
        '(("gopls.completeUnimported" t t)
             ("gopls.staticcheck" t t))))

(use-package xref
    :defer t
    :config
    (general-nmap
        :keymaps '(emacs-lisp-mode-map)
        "g d" 'xref-find-definitions
        "g r" 'xref-find-references))

;; 18. Org mode settings
;;; Set the done time for a TODO item when moving it to DONE
(use-package org
  :config
  (setq org-log-done 'time)

  (setq org-todo-keywords
		'((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)

  (setq org-directory notes-directory)

  (if (not (boundp 'org-agenda-files))
      (message '"ERROR: Variable org-agenda-files is not available. Bind it in local-confs/10_local.el"))

  (add-hook 'org-mode-hook (lambda () (setq org-odt-preferred-output-format '"docx")))

  ;; Load org-roam and org-ref
  (load '"~/.emacs.d/machine-specific/org-roam.el")
  (when (and (boundp 'load-org-ref)
			 (not (null load-org-ref)))
	(load '"~/.emacs.d/machine-specific/org-ref.el"))

  ;; 36. Org capture templates
  ;; Documentation: https://orgmode.org/manual/Capture-templates.html
  (if (not (boundp 'local-competitive-programming-note-file))
      (message '"ERROR: Variable local-competitive-programming-note-file is not available. Bind it in local-confs/10_local.el")
    (add-to-list 'org-capture-templates
				 '("l" "Captures related to competitive programming problem solving"))
    (add-to-list 'org-capture-templates
				 '("lc" "Org entry for solving a new programming problem" entry
				   (file (lambda () (notes-directory-file local-competitive-programming-note-file)))
				   "* [%^{Level|UNKNOWN|EASY|MEDIUM|HARD}] %^{Link to Competitive Programming Problem}"
				   :clock-in t
				   :jump-to-captured t
				   :unnarrowed t))

    (defun create-competitive-programming-file ()
      "A function which will read the link to a programming problem and return a stub"
      (let ((link (read-string "Link to programming problem: ")))
        (expand-file-name (format "%s.go" (car (last (split-string (string-trim-right link "/") "/"))))
						  "~/go_workspace/src/github.com/icyflame/leetcode/")))

    (add-to-list 'org-capture-templates
				 '("lg" "Go code for solving a programming problem" plain
				   (file create-competitive-programming-file)
				   "package main

func main() {
}

%?"
				   :jump-to-captured t)))

  (defun create-blog-file ()
    "Create an org file in ~/blog/."
    (interactive)
    (let ((name (read-string "Filename: "))
          (blog-directory (concat blog-location '"posts-org")))
      (expand-file-name (format "%s-%s.org"
                                (format-time-string "%Y-%m-%d") name)
						blog-directory)))

  (if (and (boundp 'local/load-blog-capture-template)
           (boundp 'blog-location)
           local/load-blog-capture-template)
      (add-to-list 'org-capture-templates
				   '("b" "Blog post" plain
					 (file create-blog-file)
					 ;; I could not move this into a variable despite trying various things.
					 (file "~/code/blog/posts-org/template.org")
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


	(defun afs/delete-link-at-point ()
		"Replace an org link by its description or if empty its address

	Source function name: afs/org-replace-link-by-link-description
	"
		(interactive)
		(if (org-in-regexp org-link-bracket-re 1)
			(save-excursion
				(let ((remove (list (match-beginning 0) (match-end 0)))
						(description
							(if (match-end 2)
								(org-match-string-no-properties 2)
								(org-match-string-no-properties 1))))
					(apply 'delete-region remove)
					(insert description)
					(message '"Removed link and inserted title instead")))))

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
					(message '"Copied: %s" link)))))

	(defun kannan/org/copy-description-to-clipboard ()
		"Replace an Org link with the same description and the link from the clipboard

	Adapted from afs/org-replace-link-by-link-description"
		(interactive)
		(if (org-in-regexp org-link-bracket-re 1)
			(save-excursion
				(let ((description (org-match-string-no-properties 2)))
					(simpleclip-set-contents description)
					(message '"Copied: %s" description)))))

	(defun kannan/org/show-link ()
		"Replace an Org link with the same description and the link from the clipboard

	Adapted from afs/org-replace-link-by-link-description"
		(interactive)
		(if (org-in-regexp org-link-bracket-re 1)
			(save-excursion
				(let ((link (org-match-string-no-properties 1)))
					(message link)))))

	(defun kannan/org/paste-as-quote ()
		"Paste the content that is the system clipboard as a quote block in Org mode."
		(interactive)
		(org-insert-structure-template '"quote")
		(insert '"\n")
		(previous-line)
		(insert (simpleclip-get-contents))))

;; 19. Install editorconfig
(use-package editorconfig
    :defer t
    :ensure t
    :config
    (editorconfig-mode 1))

;; 22. Use vendored git-link
(use-package git-link
  :init
  (if (not (file-directory-p "~/.emacs.d/lisp/git-link/"))
	  (message '"ERROR: Vendored library `git-link` does not exist. Run `git submodule init` and `git submodule update --recursive` to get it."))

  :if (file-directory-p '"~/.emacs.d/lisp/git-link/")
  :load-path "lisp/git-link/"

  :defer t)

;; 25. Yaml Mode
(use-package yaml-mode
    :defer t
    :ensure t
    :mode
    ("\\.yml\\'" . yaml-mode)
    ("\\.yaml\\'" . yaml-mode)
    ("\\.gotmpl\\'" . yaml-mode))

(use-package powerline
    :ensure t
    :config

	(defun kannan/show-word-count-in-modeline ()
	  "An interactive function which shows the word count of the current buffer in the modeline."
	  (interactive)
	  (setq powerline-display-word-count 't))

	(defun kannan/hide-word-count-in-modeline ()
      "An interactive function which hides the word count of the current buffer from the modeline."
      (interactive)
      (setq powerline-display-word-count 'nil))


	(defun powerline-theme-personal ()
	  "Setup a mode-line with major, evil, and minor modes centered."
	  (interactive)
	  (setq-default mode-line-format
					'("%e"
					  (:eval
					   (let* ((active (powerline-selected-window-active))
							  (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
							  (mode-line (if active 'mode-line 'mode-line-inactive))
							  (face0 (if active 'powerline-active0 'powerline-inactive0))
							  (face1 (if active 'powerline-active1 'powerline-inactive1))
							  (face2 (if active 'powerline-active2 'powerline-inactive2))
							  (separator-left (intern (format "powerline-%s-%s"
															  (powerline-current-separator)
															  (car powerline-default-separator-dir))))
							  (separator-right (intern (format "powerline-%s-%s"
															   (powerline-current-separator)
															   (cdr powerline-default-separator-dir))))
							  (lhs (list (powerline-raw "%*" face0 'l)
										 (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
										 (powerline-raw " " face0)
										 (funcall separator-left face0 face1)
										 (powerline-narrow face1 'l)

										 (powerline-raw global-mode-string face1 'r)
										 (powerline-raw "%4l" face1 'r)
										 (powerline-raw ":" face1)
										 (powerline-raw "%3c" face1 'r)
										 (funcall separator-right face1 face0)
										 (powerline-raw " " face0)
										 (powerline-raw "%6p" face0 'r)

										 (powerline-vc face1)))
							  (rhs (list ()))
							  (center (append (list (powerline-major-mode face2 'l)
													(powerline-process face2)
													(powerline-raw " " face2))
											  (if evil-mode
												  (list (funcall separator-right face2 face1)
														(powerline-raw evil-mode-line-tag face1 'l)
														(powerline-raw " " face1)))
											  (if powerline-display-word-count
												  (list (powerline-wc face1)))
											  )))

						 (concat (powerline-render lhs)
								 (powerline-fill-center face1 (/ (powerline-width center) 2.0))
								 (powerline-render center)
								 (powerline-fill face1 (powerline-width rhs))
								 (powerline-render rhs)))))))

    ;; 27. Include powerline
    (defpowerline powerline-wc
				  (format " %d words" (count-words (point-min) (point-max))))
    (setq-default powerline-display-word-count 'nil)

    (powerline-theme-personal))

;; 29. JSON mode
(use-package json-mode
    :defer t
    :ensure t)

;; 32. Magit
(use-package magit
    :defer t
    :ensure t
    :config
    (general-nmap
        :keymaps '(magit-mode-map)
        "s" 'magit-stage
        "u" 'magit-unstage)

    (defun kannan/magit/merge-upstream-into-current ()
        "Merge the upstream for this branch into this branch"
        (interactive)
        (magit-merge-plain (magit-get-upstream-branch)))

    (defun kannan/magit/delete-branch (branch)
        (magit-run-git "branch" "-d" branch))

    (defvar protected-branches
        '("master" "main")
        "Pushing to any of these branches will require an additional confirmation.")

    (defun kannan/magit/is-protected-branch (current-branch)
        "Return nil or t depending on whether the given branch is a `protected' branch.

Usually, branches such as `master' and `main' are considered protected branches."
        (if (null (member current-branch protected-branches)) nil t))

    (defun kannan/magit/delete-all-merged-branches ()
        "Delete all branches that have been merged into the current branch"
        (interactive)
        (let ((current-branch (magit-get-current-branch)))
            (if (or (kannan/magit/is-protected-branch current-branch)
                    (eq t (kannan/ask-user-approval "Delete merged branches, even though we are not on master?")))
                (let ((branches-to-delete (delete current-branch (magit-list-merged-branches))))
                    (mapc #'kannan/magit/delete-branch branches-to-delete)
                    (message "Deleted all branches: %s" (string-join branches-to-delete ", "))))))

    (defun kannan/magit/push-safe-to-current ()
        "Push safely to the upstream branch of the current branch. Ask user before pushing to master"
        (interactive)
        (let ((current-branch (magit-get-current-branch)))
            (if (or (not (kannan/magit/is-protected-branch current-branch))
                    (eq t (kannan/ask-user-approval (format "Push to upstream on protected branch `%s'?" current-branch))))
                (call-interactively #'magit-push-current-to-pushremote))))

    (defun kannan/magit/checkout-previous-branch ()
        "Checkout the previous branch"
        (interactive)
        (magit-checkout (magit-get-previous-branch)))

    (defun kannan/magit/first-protected-branch ()
        "Return the first branch in the protected-branches list that exists in this repository.

This is to support both older repositories that use `master' as the default branch, and newer ones that use `main' as the default branch"
        (seq-find #'magit-local-branch-p protected-branches))

    (defun kannan/magit/checkout-default-branch ()
        "Checkout the default branch"
        (interactive)
        (let ((default-branch (kannan/magit/first-protected-branch)))
            (magit-checkout default-branch)))

    (defun kannan/magit/rebase-previous-branch ()
        "Rebase current branch on the previous branch"
        (interactive)
        (magit-rebase-branch (magit-get-previous-branch) ())))

;; 33. Comp(lete) any(thing)
;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
    :defer t
    :ensure t
    :hook ((go-mode emacs-lisp-mode) . company-mode)
    :config
    (setq company-minimum-prefix-length 3))

;; 34. Protobuf mode
(use-package protobuf-mode
    :defer t
    :ensure t)

;; 35. Yasnippets
(use-package yasnippet
    :defer t
    :ensure t
    ;; :hook ((org-mode go-mode perl-mode) . #'yas-minor-mode)
    :config
    (add-hook 'go-mode-hook #'yas-minor-mode)
    (add-hook 'org-mode-hook #'yas-minor-mode)
    (add-hook 'perl-mode-hook #'yas-minor-mode)
    (add-hook 'c++-mode-hook #'yas-minor-mode)
    (yas-reload-all))

(use-package ox-hugo
    :defer t
    :ensure t)

(use-package ob-go
    :defer t
    :ensure t
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((go . t)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((perl . t))))

(use-package web-mode
    :defer t
    :ensure t)

(use-package notmuch
    :defer t
    :ensure t
    :config
	(defun kannan/notmuch-show-delete-message-then-next-or-next-thread ()
		"Add the deleted tag to the current message and move to the next message.

	Useful when triaging e-mails for later passes of actually reading the e-mails"
		(interactive)
		(notmuch-show-add-tag '("+deleted"))
		(unless (notmuch-show-next-open-message)
			(notmuch-show-next-thread t)))

	(defun kannan/notmuch-tree-delete-message-then-next-or-next-thread ()
		"Delete the current message highlighted in the thread view

	Useful when viewing a thread with drafts in it which are not duplicates of sent messages"
		(interactive)
		(notmuch-tree-add-tag '("+deleted"))
		(notmuch-tree-next-message))

	(setq-default mml-secure-openpgp-sign-with-sender t)

	;; When archiving a thread, remove both inbox and unread tags.
	(advice-add
		'notmuch-search-archive-thread
		:before
		(lambda (&rest r) (notmuch-search-remove-tag '("-unread")))
		'((name . "notmuch-remove-unread-on-archive")))

	(defun kannan/notmuch/delete-thread ()
		"Delete the current thread by adding a tag to it."
		(interactive)
		(notmuch-search-add-tag '("+deleted"))
		(notmuch-search-next-thread))

	(defun kannan/notmuch/view-html-part ()
		"View the text/html part that the cursor is currently on in a browser"
		(interactive)
		(let ((handle (notmuch-show-current-part-handle))
				(file-name '"/tmp/g.html"))
			(mm-save-part-to-file handle file-name)
			(if (string-equal '"darwin" system-type)
				(browse-url-default-macosx-browser (concat "file://" file-name))
				(browse-url-firefox (concat "file://" file-name)))))

	(defun kannan/notmuch/show-save-all-attachments-to-tmp ()
		"Save all attachments of the currently open e-mail to the ~/Downloads directory

	Ask the user for an optional prefix for all the filenames."
		(interactive)

		(with-current-notmuch-show-message
			(let ((mm-handle (mm-dissect-buffer))
					;; save-directory can be defined by the user or we will use /tmp
					(save-directory (if (boundp 'local/email/downloads-directory)
										local/email/downloads-directory
										(if (boundp 'local/email/temporary-directory)
											local/email/temporary-directory '"/tmp")))
					(filename-prefix
						(read-string '"Enter a prefix for the attachment files: (default: empty) " "" nil "")))
				(notmuch-foreach-mime-part
					(lambda (p)
						(let ((filename (mm-handle-filename p)))
							;; filename is nil when the part is not an attachment
							(if (not (eq filename nil))
								(mm-save-part-to-file p (expand-file-name (concat filename-prefix filename) save-directory)))))
					mm-handle))))

	;; When generating a unique message ID for emails sent from Emacs, replace the ".fsf" prefix with
	;; ".emacs".
	;; :filter-return is cleaner. Guide: https://emacs.stackexchange.com/a/26556
	(advice-add 'message-unique-id :filter-return #'kannan/message-unique-id)
	(defun kannan/message-unique-id (original-return-val)
		(string-replace ".fsf" ".emacs" original-return-val))


    (general-imap
        :keymaps '(notmuch-search-mode-map)
        "d" 'kannan/notmuch/delete-thread)
    (general-nmap
        :keymaps '(notmuch-show-mode-map)
        "o" 'kannan/notmuch/view-html-part
        "d" 'kannan/notmuch-show-delete-message-then-next-or-next-thread
        "M-d" 'kannan/notmuch-show-delete-message-then-next-or-next-thread
        "a" 'notmuch-show-archive-message-then-next-or-next-thread)
    (general-imap
        :keymaps '(notmuch-show-mode-map)
        ". a" 'kannan/notmuch/show-save-all-attachments-to-tmp)
    (general-imap
        :keymaps '(notmuch-tree-mode-map)
        "d" 'kannan/notmuch-tree-delete-message-then-next-or-next-thread)
    (general-nmap
        :keymaps '(notmuch-tree-mode-map)
        "M-d" 'kannan/notmuch-tree-delete-message-then-next-or-next-thread
        "M-a" 'notmuch-show-archive-message-then-next-or-next-thread))

(use-package org-journal
    :defer t
    :ensure t
    :config
    (setq org-journal-dir (notes-directory-file "journal/")
        org-journal-date-format "%F (%a)"))

;; TODO: Everything before this is using use-package.

(defun kannan/golang-download-dependncies ()
    "This function will download dependencies using gomods"
    (interactive)
    (start-process "download-go-dependencies" "*Go mods*" "go" "mod" "vendor"))

(use-package simpleclip
  :load-path "lisp/")

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
    (defvar emacs-english-font "Noto Sans Mono" "The font name of English.")
    (defvar emacs-cjk-font "Noto Sans CJK JP" "The font name for CJK.")
  ;;; for test
    ;; (find-font (font-spec :name "LiHei Pro"))
    ;; (font-family-list)

    (defvar emacs-font-size-pair '(20 . 23)
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
        (interactive) (emacs-step-font-size -1)))

(defun kannan/convert-region-to-ascii (beg end &optional arg)
    "Convert the highlighted region to ASCII through transliteration offered by iconv.

iconv should be present on the host.

I don't know what the third argument to this function is, but not having the third argument
causes the function to throw an error when this function is executed from visual mode.
"
    (interactive "r\nP")
    (shell-command-on-region beg end '"iconv --to ascii//translit" nil t)
    (message "where I %d you %d" end beg))

(use-package olivetti
    :defer t
    :ensure t)

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

(use-package eshell-syntax-highlighting
    :defer t
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
(use-package mozc
  :load-path "lisp/"
  :config
  (setq default-input-method "japanese-mozc"))

(defun kannan/get-strings-without-text-properties (input)
    "From the given list variable `input', get every element which is a string without text properties.

This function is particularly useful when used with the variable where the `ivy-read' stores its history."
  (seq-filter
   (lambda (element)
	 (eq (text-properties-at 0 element) nil))
   input))

(defun kannan/get-strings-matching-pattern (pattern lst)
    "From the given list `lst', return a list containing all strings which contain `pattern'"
  (seq-filter
   (lambda (elt) (string-match pattern elt))
   lst))

;; Inspired by git-grep integration with counsel:
;;   https://oremacs.com/2015/04/19/git-grep-ivy/
(defun make-command-from-reg-comp (comp)
    (format "%s --ignore-case \"%s\"" (executable-find '"rg") comp))

(setq-default ivy-locate-replacement-helper-history '())

(defun ivy-locate-replacement-helper-function (string &optional _pred &rest _u)
    "Grep in the current git repository for STRING."
    ;; string = 'A B' => rg-part = '/usr/local/bin/rg --ignore-case "A" | /usr/local/bin/rg --ignore-case "B"'
    (let ((rg-part (string-join (mapcar #'make-command-from-reg-comp (split-string string " ")) " | ")))
        (append (seq-reduce (lambda (sum elt)
                                (kannan/get-strings-matching-pattern elt sum))
                    (split-string string '" " t)
                    (kannan/get-strings-without-text-properties ivy-locate-replacement-helper-history))

            (split-string
                (shell-command-to-string
                    (format
                        "cat ~/.locate-simple-replacement-index | %s | head"
                        rg-part))
                "\n"
                t))))

(defun ivy-locate-replacement-helper ()
    "Grep for a string in the current git repository."
    (interactive)
    (if (not (file-exists-p '"~/.locate-simple-replacement-index"))
        (message "Index does not exist. Run 01_create_index.sh script before running this.")
        (let ((default-directory (locate-dominating-file
                                     default-directory ".git"))
                 (val (ivy-read "pattern: " 'ivy-locate-replacement-helper-function
                          :dynamic-collection t
                          :history 'ivy-locate-replacement-helper-history
                          )))
            (find-file val)
            (goto-char (point-min)))))

;; *Problems:*
;;
;; Now, the function works as required. But it keeps searching on every single keypress. That is not
;; performant enough. Instead, we have to use C-m, C-j, and RET in conjunction, so that C-j will update
;; the list of suggestions, and RET will actually enter that file; while typing a single keypress will
;; not do anything except update the search pattern.
;;
;; With 2.4 million file names in the index, the simple replacement works without lag. But with more
;; files, this will probably change and we will hit performance problems.
;;
;; Explanation: https://oremacs.com/2019/06/27/ivy-directory-improvements/

(setq org-html-postamble 'nil)
(setq org-html-head-include-default-style 'nil)

(if (not (boundp 'local-clippings-file))
    (message '"ERROR: Variable local-clipping-file is not available. Bind it in local-confs/10_local.el")
    (add-to-list 'org-capture-templates
        '("c" "Clip whatever is in the clipboard"))
    (add-to-list 'org-capture-templates
        '("cb" "Something copied from the Firefox Browser" entry
             (file+datetree (lambda () (notes-directory-file local-clippings-file)))
             "* Clipping from Firefox at %<%R>

#+begin_quote
%c
#+end_quote"
             :empty-lines-before 1
             :unnarrowed t)))

(if (boundp 'org-latex-classes)
    (add-to-list 'org-latex-classes '("siddharthkannanresume" "\\documentclass[]{siddharthkannanresume}"
                                         ("\\section{%s}" . "\\section*{%s}")
                                         ("\\subsection{%s}" . "\\subsection*{%s}")
                                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package php-mode
    :defer t
    :ensure t)

(require 'benchmark)

(use-package lua-mode
    :defer t
    :ensure t
    :config
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode)))

(use-package jinja2-mode
  :load-path "lisp/")

;; Show a list of TODO headlines which don't have a schedule or a deadline
;; https://emacs.stackexchange.com/a/16561/31572
(setq org-agenda-custom-commands
      '(("u" . "Unscheduled TODO")
        ("ut" "Unscheduled TODOs"
         ((todo ""
                ((org-agenda-overriding-header "\nUnscheduled TODO")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp)))))
         nil
         nil)))

;; Default Org export options
(setq org-export-with-author 'nil)
(setq org-export-with-toc 'nil)
(setq org-export-with-sub-superscripts 'nil)

(unless (boundp 'enable-treesitter)
    (message '"INFO: enable-treesitter is not set. Treesitter will be disabled.")
    (setq enable-treesitter nil))

(when (and enable-treesitter (treesit-available-p))
    ;; treesit.el is included in Emacs. Language grammars have to be installed separately for each
    ;; of the languages where we want to use tree-sitter support.
    ;; https://arnesonium.com/2023/08/configuring-emacs-29-1-for-golang
    ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
    ;; Run `treesit-install-language-grammar' after initial start-up of Emacs
    (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
             (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

    (use-package yaml-pro
        :defer t
        :ensure t
        :hook (yaml-mode . yaml-pro-ts-mode)
        :config
        ;; Keybindings chosen by Yaml-Pro conflict with those in Org-mode.
        ;; I don't think the default keybindings are too useful anyway, so I will just delete the
        ;; map wholesale from the minor mode map list
        ;;
        ;; https://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
        ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Controlling-Active-Maps.html#Definition-of-minor_002dmode_002dmap_002dalist
        ;;
        (setq minor-mode-alist
            (assq-delete-all 'yaml-pro-ts-mode minor-mode-map-alist)))

    (use-package treesit-fold
        :if (file-directory-p "~/.emacs.d/lisp/treesit-fold/")
        :load-path "~/.emacs.d/lisp/treesit-fold/"))

(defun kannan/org-agenda/schedule-offset (offset)
    "Schedule the Org TODO item at point `offset' days from today. Offset is an integer."
    (let ((now (current-time)))
        (org-agenda-schedule
            (point)
            (format-time-string '"%F" (time-add now (days-to-time offset))))))

(defun kannan/org-agenda/schedule-today ()
    "Schedule the Org TODO item at point to today.

This is an interactive function which will be bound to a keybinding, and will be called when the keybinding is used."
    (interactive)
    (kannan/org-agenda/schedule-offset 0))

(defun kannan/org-agenda/schedule-tomorrow ()
    "Schedule the Org TODO item at point to tomorrow.

This is an interactive function which will be bound to a keybinding, and will be called when the keybinding is used."
    (interactive)
    (kannan/org-agenda/schedule-offset 1))

(defun kannan/org-roam/condense-agenda-files ()
    "Reduce the org-agenda-files list to a list of only those files that contain TODO items.

This requires ripgrep to be installed."
    (interactive)
    (let ((grep-path (executable-find '"rg"))
             (agenda-files-length-before (list-length org-agenda-files))
             (agenda-files-length-after 0))
        (if (or (null grep-path) (null org-roam-directory))
            (message '"ERROR: Prerequisites not met. Ripgrep must be installed in order to condense the `org-agenda-files' list. `org-roam-directory' must not be `nil'")
            (setq org-agenda-files (split-string (shell-command-to-string (format '"%s -w '(TODO|WAITING|DONE|CANCELED)' -l %s" grep-path org-roam-directory)) '"\n"))
            (setq agenda-files-length-after (list-length org-agenda-files))
            (message '"INFO: Condensed `org-agenda-files'. Length change: %d => %d" agenda-files-length-before agenda-files-length-after))))

;; TODO: Reduction of `org-agenda-files' does not work properly yet
;; (advice-add #'org-agenda-list
;;     :before
;;     #'kannan/org-roam/condense-agenda-files)

(use-package ledger-mode
    :defer t
    :ensure t
    :config
    ;; From the documentation: https://github.com/ledger/ledger-mode/blob/master/doc/ledger-mode.texi#L984C1-L986C37
    ;; @item ledger-highlight-xact-under-point
    ;; If non-nil, highlight transaction under point using
    ;; @option{ledger-font-highlight-face}.
    (copy-face 'region 'ledger-font-xact-highlight-face)
    (setq ledger-highlight-xact-under-point t))

(use-package jsonnet-mode
    :ensure t)
