;; 34. Org Roam
(require-package 'org-roam)
(use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory '"~/work/notes/org-roam")
    :config

    (org-roam-setup)

    (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
        (let ((level (org-roam-node-level node)))
            (concat
                (when (> level 0) (concat (org-roam-node-file-title node) " > "))
                (when (> level 1) (concat (string-join (org-roam-node-olp node) " > ") " > "))
                (org-roam-node-title node))))

    (setq org-roam-node-display-template "${tags:100} ${hierarchy:*}")
    (setq org-roam-capture-templates
        '(("d" "default" plain "%?

* Source

"
              :target (file+head "%<%Y-%m-%d>-${slug}.org" "#+title: ${title}
#+author:
#+created: %T
#+filetags:")
              :unnarrowed t))))

;; To move to v2, first install the latest version of org-roam. And then, run the "org-roam-migrate-wizard" function, which rewrites everything.
;; More about v2: https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2
