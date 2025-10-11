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

(defun insert-current-time ()
    "Insert the current time string into the active buffer at point"
    (interactive)
    (insert-string (format-time-string '"%F %H:%M:%S %Z")))

(defun insert-current-date ()
    "Insert the current date string into the active buffer at point"
    (interactive)
    (insert-string (format-time-string '"%F")))

(defun insert-time ()
    "Insert the current time string into the active buffer at point"
    (interactive)
    (insert-string (format-time-string '"%R:%S")))

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

Note: This will not unwrap text which is not inside an Org subtree. If you have such a file, then
consider adding an Org header at the top of the file.
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
                        (org-in-subtree-not-table-p)
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
