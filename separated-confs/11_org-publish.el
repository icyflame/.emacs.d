(setq org-publish-project-alist
    '(
         ("org-test-publish"
             ;; Path to your org files.
             :base-directory "~/sandbox/test-org-publish/source"
             :base-extension "org"

             ;; Path to your Jekyll project.
             :publishing-directory "~/sandbox/test-org-publish/dest"
             :publishing-function org-blackfriday-publish-to-blackfriday
             :headline-levels 4
             :html-extension "md"
             :body-only t ;; Only export section between <body> </body>

             :with-toc nil
             )
         ))
