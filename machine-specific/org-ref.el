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
