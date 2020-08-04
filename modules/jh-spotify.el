;;; appearance --- Summary

;;; Commentary:

;;; Code:
(defun modules/spotify--load (config)
  "Configure spotify using CONFIG."
  (add-to-list 'load-path "~/.emacs.d/ext_lisp/spotify.el")
  (require 'spotify)
  (let* ((spotify-auth "~/.emacs.d/.spotify_auth")
         (spotify-data (with-temp-buffer
                         (insert-file-contents spotify-auth)
                         (split-string (buffer-string) "\n" t)))
         (client-id (nth 0 spotify-data))
         (client-secret (nth 1 spotify-data)))
    (setq spotify-transport 'apple)
    (setq spotify-oauth2-client-id client-id)
    (setq spotify-oauth2-client-secret client-secret)))



(provide 'jh-spotify)
;;; jh-spotify.el ends here
