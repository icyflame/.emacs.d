(defun preview-plantuml-now ()
    "This function previews the PlantUML present in the current file.

It uses a PlantUML helper function to render the PlantUML into a PNG file. And then, opens that PNG
file in a new buffer. This new buffer is generated by splitting horizontally from the current window.

Focus is never moved from the PlantUML buffer, so the editing experience can continue uninterrupted
while the file is being rendered."
    (interactive)

    ;; Render plantuml in the current buffer to a PNG file on disk
    (plantuml-render-buffer)

    ;; Prepare the preview buffer next to the plantUML buffer (create if necessary)
    (setq preview-buffer-name "*PlantUML Preview*")

    (setq preview-buffer (get-buffer-create preview-buffer-name))

    (setq image-name (replace-regexp-in-string "puml$" "png" (buffer-file-name)))

    (with-current-buffer preview-buffer
        ;; Revert the buffer to it's original state
        (read-only-mode -1)
        (major-mode-suspend)
        (erase-buffer)
        ;; Insert the image file and prepare display
        (insert-file image-name)
        (image-mode)
        (read-only-mode))

    ;; Prepare the window where the preview buffer will be displayed (split horizontally if required)
    (setq preview-window (get-buffer-window preview-buffer))
    (if (eq preview-window nil) (setq preview-window (split-window-horizontally)))
    (set-window-buffer preview-window preview-buffer))
