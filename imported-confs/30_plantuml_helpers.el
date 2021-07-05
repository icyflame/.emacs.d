;; https://gist.githubusercontent.com/rpl/547521/raw/1f45128251e5677e4f681f6d3c15892cf6589ab2/plantuml_helpers.el
(require 'iimage)
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
(add-to-list 'iimage-mode-image-regex-alist '("@startuml\s+\\(.+\\)" . 1))

;; Rendering plantuml
(defun plantuml-render-buffer ()
  (interactive)
  (if (not (file-exists-p '"/usr/local/bin/plantuml.jar"))
	  (message "Plantuml.jar not found at /usr/local/bin/plantuml.jar")
  (message "PLANTUML Start rendering")
  (shell-command (concat "java -jar /usr/local/bin/plantuml.jar "
                         buffer-file-name))
  (message (concat "PLANTUML Rendered:  " (buffer-name)))))

;; Image reloading
(defun reload-image-at-point ()
  (interactive)
  (message "reloading image at point in the current buffer...")
  (image-refresh (get-text-property (point) 'display)))

;; Image resizing and reloading
(defun resize-image-at-point ()
  (interactive)
  (message "resizing image at point in the current buffer123...")
  (let* ((image-spec (get-text-property (point) 'display))
         (file (cadr (member :file image-spec))))
    (message (concat "resizing image..." file))
    (shell-command (format "convert -resize %d %s %s "
                           (* (window-width (selected-window)) (frame-char-width))
                           file file))
    (reload-image-at-point)))
