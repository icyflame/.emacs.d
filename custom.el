(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-link-use-commit t)
 '(helm-ag-base-command "rg --no-heading")
 '(helm-ag-success-exit-status '(0 2))
 '(helm-completion-style 'emacs)
 '(lsp-ui-doc-position 'bottom)
 '(mml-secure-key-preferences
   '((OpenPGP
	  (sign)
	  (encrypt
	   ("mail@siddharthkannan.in" "71DB9964695BBCD235B5155305A6BC36250D69FF")))
	 (CMS
	  (sign)
	  (encrypt))))
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key "i")
	 (:name "unread" :query "tag:unread" :key "u")
	 (:name "flagged" :query "tag:flagged" :key "f")
	 (:name "sent" :query "tag:sent" :key "t")
	 (:name "drafts" :query "tag:draft" :key "d")
	 (:name "all mail" :query "*" :key "a")
	 (:name "todo" :query "tag:todo")))
 '(notmuch-search-oldest-first nil)
 '(org-agenda-files
   '("~/work/notes/TODO.org" "~/work/notes/Current.org" "~/personal/notes/Current.org" "~/personal/notes/Current.org"))
 '(org-roam-directory "~/work/notes/org-roam")
 '(package-selected-packages
   '(filenotify-recursive emacsql-sqlite org-roam-id rust-mode olivetti color-theme-tomorrow lsp-treemacs treemacs-projectile treemacs-evil treemacs "treemacs" nov org-journal lsp-ui solarized-theme atomic-chrome nord-theme notmuch awesome-tab web-mode ob-perl ob-go elfeed ox-hugo ox-confluence helm-rg yasnippet protobuf-mode org-roam-doctor gotest org-ref org-roam-bibtex org-roam-completion org-roam company-lsp lsp-mode company company-mode magit ggtags json-mode php-mode helm-ag powerline jsx-mode yaml-mode git-link sqlformat editorconfig helm-projectile plantuml-mode general markdown-mode use-package helm go-mode go-mode\.el gnu-elpa-keyring-update evil))
 '(send-mail-function 'sendmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
