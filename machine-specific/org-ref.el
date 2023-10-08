(require-package 'ivy-bibtex)
(require-package 'pdf-tools)
(require-package 'org-ref)
(use-package org-ref
    :config
    ;; Enable downloading PDFs / getting bibtex entries using DOI
    (require 'doi-utils)
    (require 'org-ref-arxiv)

    (setq reftex-default-bibliography '("~/notes/bibliography/references.bib"))
    ;; Required for org-ref
    (setq org-ref-bibliography-notes "~/notes/bibliography/notes.org"
        org-ref-default-bibliography '("~/notes/bibliography/references.bib")
        org-ref-pdf-directory "~/notes/bibliography/bibtex-pdfs/")

    ;; Required for helm-bibtex
    (setq bibtex-completion-bibliography "~/notes/bibliography/references.bib"
        bibtex-completion-library-path "~/notes/bibliography/bibtex-pdfs"
        bibtex-completion-notes-path "~/notes/bibliography/ivy-bibtex-notes")
    )
