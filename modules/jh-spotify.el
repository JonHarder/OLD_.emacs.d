;;; appearance --- Summary

;;; Commentary:

;;; Code:
(require 'contrib "~/.emacs.d/contrib")

(defun modules/spotify--load (config)
  "Configure spotify using CONFIG."
  (let* ((plugin-installed (file-directory-p "~/.emacs.d/ext_lisp/spotify.el"))
         (should-install (or plugin-installed (y-or-n-p "Spotify package not installed, clone now? "))))
    (when (and (not plugin-installed) should-install)
      (async-shell-command "git clone https://github.com/danielfm/spotify.el ~/.emacs.d/ext_lisp/spotify.el"))
    (when (or plugin-installed should-install)
      (add-to-list 'load-path "~/.emacs.d/ext_lisp/spotify.el")
      (require 'spotify)
      (unless (file-exists-p "~/.emacs.d/.spotify_auth")
        (error "Credential file '~/.emacs.d/.spotify_auth' not found"))
      (let* ((spotify-auth "~/.emacs.d/.spotify_auth")
             (spotify-data (contrib/read-file-to-lines spotify-auth))
             (client-id (nth 0 spotify-data))
             (client-secret (nth 1 spotify-data)))
        (setq spotify-oauth2-client-id client-id)
        (setq spotify-oauth2-client-secret client-secret)
        (setq spotify-transport 'apple)))))



(provide 'jh-spotify)
;;; jh-spotify.el ends here
