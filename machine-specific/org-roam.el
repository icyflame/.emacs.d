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

* Source



* Related

- "
		   :unnarrowed t
		   )))

  ;; Think about moving these to use general.el later
  :bind (:map org-roam-mode-map
			  (("C-c n l" . org-roam)
			   ("C-c n f" . org-roam-find-file)
			   ("C-c n g" . org-roam-graph))
			  :map org-mode-map
			  (("C-c n i" . org-roam-insert))
			  (("C-c n I" . org-roam-insert-immediate))))
