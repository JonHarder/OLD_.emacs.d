;;; Code:
(require 'contrib "~/.emacs.d/contrib")

(defmacro with-spotify-auth (client-id client-secret &rest body)
  "Assign spotify client id and secret to CLIENT-ID and CLIENT-SECRET and execute BODY."
  (declare (indent defun))
  (let ((data (gensym)))
    `(let* ((,data (contrib/read-file-to-lines "~/.emacs.d/.spotify_auth"))
            (,client-id (nth 0 ,data))
            (,client-secret (nth 1 ,data)))
       ,@body)))


(use-package consult-spotify
  :config
  (with-spotify-auth client-id client-secret
    (setq espotify-client-id client-id
          espotify-client-secret client-secret)))


(let* ((plugin-installed (file-directory-p "~/.emacs.d/ext_lisp/spotify.el"))
       (should-install (or plugin-installed (y-or-n-p "Spotify package not installed, clone now? "))))
  (when (and (not plugin-installed) should-install)
    (async-shell-command "git clone https://github.com/danielfm/spotify.el ~/.emacs.d/ext_lisp/spotify.el"))
  (when (or plugin-installed should-install)
    (add-to-list 'load-path "~/.emacs.d/ext_lisp/spotify.el")
    (require 'spotify)
    (unless (file-exists-p "~/.emacs.d/.spotify_auth")
      (error "Credential file '~/.emacs.d/.spotify_auth' not found"))
    (with-spotify-auth client-id client-secret
      (setq spotify-oauth2-client-id client-id)
      (setq spotify-oauth2-client-secret client-secret)
      (setq spotify-transport 'apple))))
