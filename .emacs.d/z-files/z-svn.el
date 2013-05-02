(require 'psvn)

(defvar z-svn-buffer-name "^\\*svn-status\\*"
  "A regexp matching the name of a buffer running svn, as
switched to by \\[z-goto-svn].")

(defun z-goto-svn (arg)
  "Switch to the svn buffer as identified by z-svn-buffer-name, mangling
windows according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-svn-buffer-name))

(defun z-svn-keys ()
  (interactive)
  (z-define-keys ctl-z-map
		 '(
		   ("\C-u"  . svn-status)
		   ("u"     . z-goto-svn))))

(setq svn-status-hide-unmodified t)
(z-svn-keys)
