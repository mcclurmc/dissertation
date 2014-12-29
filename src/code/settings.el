(defun ocaml-listing-settings ()
  (interactive)
  (let ((bname (buffer-file-name)))
    (cond
     ((string-match "Oxford/Thesis/" bname)
       (set-variable 'fill-column 60))
    )))

(add-hook 'tuareg-mode-hook 'ocaml-listing-settings)
