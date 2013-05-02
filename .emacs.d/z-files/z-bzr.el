(require 'bazaar)

(defvar z-bzr-buffer-name "^\\*Bazaar\\*"
  "A regexp matching the name of a buffer running bzr, as
switched to by \\[z-goto-bzr].")

(defun z-goto-bzr (arg)
  "Switch to the bzr buffer as identified by z-bzr-buffer-name, mangling
windows according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-bzr-buffer-name))

(defun z-bzr-keys ()
  (interactive)
  (z-define-keys ctl-z-map
		 '(
		   ("\C-u"  . bazaar)
		   ("u"     . z-goto-bzr))))

(setq bzr-status-hide-unmodified t)
(z-bzr-keys)
