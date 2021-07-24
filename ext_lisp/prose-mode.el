;;; prose-mode.el --- Major mode for distraction free writing, allows for setting margins, and provides writing tips. -*- lexical-binding: t -*-

;; Author: 2021	Jon Harder <jonharder6@gmail.com>
;; Keywords: writing prose composition
;; URL: https://github.com/JonHarder/prose-mode

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'writegood-mode)

(defconst prose-version "1.0.0"
  "The release version of `prose-mode'.")

;;;###autoload
(defun prose-version (&optional here)
  "Show the `prose-mode' version in the echo area.
With prefix argument HERE, insert it at point."
  (interactive "P")
  (let* ((prose-mode-dir (ignore-error
                             (file-name-directory (or (locate-library "prose-mode") ""))))
         (version (format "prose-mode version %s (%s)"
                          prose-version
                          prose-mode-dir)))
    (if here
        (insert version)
      (message "%s" version))))


(defvar prose-mode-map
  (make-sparse-keymap)
  "Keymap used in `prose-mode'.")
    

(defun prose-mode--map (fn)
  "Map argument FN over the list of all `prose-mode' buffers."
  (dolist ($buf (buffer-list (current-buffer)))
    (with-current-buffer $buf
      (when (eq major-mode 'prose-mode)
        (funcall fn $buf)))))

;; (defun prose-mode--set-margin-customize (margin-symbol margin-value)
;;   "Set the margin symbol MARGIN-SYMBOL `prose-mode-margin' to MARGIN-VALUE."
;;   (set-default margin-symbol margin-value)
;;   (dolist ($buf (buffer-list (current-buffer)))
;;     (with-current-buffer $buf
;;       (when (eq major-mode 'prose-mode)
;;         (setq left-margin-width margin-value
;;               right-margin-width margin-value)
;;         (set-window-buffer
;;           (get-buffer-window $buf)
;;           $buf)))))

(defun prose-mode--set-margin-customize (margin-symbol margin-value)
  "Set the margin symbol MARGIN-SYMBOL `prose-mode-margin' to MARGIN-VALUE."
  (set-default margin-symbol margin-value)
  (prose-mode--map
   (lambda ($buf)
     (setq left-margin-width margin-value
           right-margin-width margin-value)
     (set-window-buffer
      (get-buffer-window $buf)
      $buf))))

(defun prose-mode--set-line-spacing-customize (sym spacing)
  "Post-customization function to update `prose-mode' buffers with settings, setting symbol SYM to SPACING."
  (set-default sym spacing)
  (prose-mode--map (lambda (_)
                     (setq line-spacing spacing))))

(defcustom prose-mode-hook '(writegood-mode visual-line-mode flyspell-mode)
  "List of functions to run after `prose-mode' is enabled."
  :group 'prose
  :type 'hook
  :options '(flyspell-mode
             visual-line-mode
             variable-pitch-mode
             writegood-mode))

(defcustom prose-mode-margin 12
  "Number of columns the margin should take up."
  :type 'integer
  :group 'prose
  :set 'prose-mode--set-margin-customize)

(defcustom prose-mode-line-spacing 0.0
  "Line spacing for `prose-mode' buffers."
  :group 'prose
  :type 'integer
  :set #'prose-mode--set-line-spacing-customize)


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prose\\'" . prose-mode))
   
;;;###autoload
(defun prose-mode-set-margin (&optional margin)
  "Set the number of columns to MARGIN in all `prose-mode' buffers."
  (interactive "nMargin: ")
  (prose-mode--set-margin-customize 'prose-mode-margin (or margin prose-mode-margin)))

;;;###autoload
(defun prose-mode-set-line-spacing (&optional spacing)
  "Set the line spacing of all `prose-mode' buffers to argument SPACING."
  (interactive "nLine Spacing: ")
  (prose-mode--set-line-spacing-customize 'prose-mode-line-spacing (or spacing prose-mode-line-spacing)))

;;;###autoload
(define-derived-mode prose-mode text-mode "Prose"
  "Major mode for writing ."
  :group 'prose
  :after-hook (progn
                (prose-mode-set-margin)
                (prose-mode-set-line-spacing)))


(provide 'prose-mode)
;;; prose-mode.el ends here
