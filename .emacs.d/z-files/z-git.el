
(require 'git)

(defvar z-git-buffer-name "^\\*git-status\\*"
  "A regexp matching the name of a buffer running git, as
switched to by \\[z-goto-git].")

(defun z-goto-git (arg)
  "Switch to the git buffer as identified by z-git-buffer-name, mangling
windows according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-git-buffer-name))

(defun z-git-keys ()
  (interactive)
  (z-define-keys ctl-z-map
		 '(
		   ("\C-u"  . git-status)
		   ("u"     . z-goto-git))))

(z-git-keys)
