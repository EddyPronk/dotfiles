;;; ctl-z-map.el --- a global keymap with some useful bindings

;; Copyright (C) 1992, 1993, 1994, 1995

;; Authors: Pieter J. Schoenmakers <tiggr@tiggr.net>
;;          and Michael L.H. Brouwer <michael@thi.nl>

;; This file is part of the z-files.

;; the z-files is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; the z-files is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; INSTALLATION/USAGE:
;;;
1;;;   o  loaded by z-files.el as desired.

;;; User overridable settings.
(defvar z-find-file-shortcut-directory "~/.notepad/"
  "Directory to shortcut search for files which have not been explicitly set.")

(defvar z-find-file-shortcut-files '((?e . "~/.emacs"))
  "Files to shortcut-find from a directory other than
z-find-file-shortcut-directory.  This is a list of (character . filename)
cons cells.  It overrides files found in the z-find-file-shortcut-directory
otherwise.")

(defvar z-inserted-name (concat "--" (capitalize (user-login-name)))
  "*Name to be inserted by \\[z-insert-name].")

(defvar z-inserted-first-name "--Pieter"
  "*Name to be inserted by \\[z-insert-first-name].")

(defvar z-compilation-buffer-name "^\\*compilation"
  "A regexp matching the name of a buffer running a compilation, as
switched to by \\[z-tail-compilation].")

(defvar z-gdb-buffer-name "^\\*gud"
  "A regexp matching the name of a buffer running your favourite debugger,
as switched to by \\[z-goto-gdb].")

(defvar z-zenirc-buffer-name "^\\*zenirc"
  "A regexp matching the name of a buffer running your favourite irc client,
as switched to by \\[z-goto-zenirc].")

(defvar z-grep-buffer-name "^\\*grep"
  "A regexp matching the name of a buffer running a grep, as
switched to by \\[z-tail-grep].")

(defvar z-shell-buffer-name "^\\*shell"
  "A regexp matching the name of a buffer running the shell, as
switched to by \\[z-tail-shell].")

(defvar z-cvs-buffer-name "^\\*cvs\\*"
  "A regexp matching the name of a buffer running cvs, as
switched to by \\[z-goto-cvs].")

(defvar z-binary-little-endian nil
  "Iff T \\[z-display-binary] assumes a little endian represenation.
Otherwise, big endian is assumed (the default).")

;;; Subsidiary functions
(defun z-define-keys (map key-defs)
  "Insert into the MAP the consed key definitions in the KEY-DEFS list."
  (mapcar (lambda (keydef) (define-key map (car keydef) (cdr keydef)))
	  key-defs))

(defun z-mangle-windows (arg buffer)
  "Deciding the window configuration depending on the ARG, display the
BUFFER.  Positive ARG (> 1) means a window different from the current
window will display the buffer.  Negative ARG (< 1) means the buffer will
be the only buffer displayed in current frame.  Otherwise, the buffer is
put in the current window.  If the buffer is already visible, simply
switch to that buffer."
  (let ((window (get-buffer-window buffer)))
    (if window
	(select-window window)
      (if (<= arg 1)
	  (switch-to-buffer buffer)
	(switch-to-buffer-other-window buffer)))
    (if (< arg 1) (delete-other-windows))))

(defun z-match-buffer-string (num)
  "Return string of text matched by last search in a buffer"
  (buffer-substring (match-beginning num) (match-end num)))

(defun z-nearest-buffer (buffer-list &optional source-buffer)
  "Return the buffer from the BUFFER-LIST nearest to the optional
SOURCE-BUFFER, which defaults to the current buffer.  Distance measurement
is based on longest common prefix of the buffers' default directory.  If
possible, the buffer returned is not the SOURCE-BUFFER."
  (save-excursion
    (or source-buffer (setq source-buffer (current-buffer)))
    (set-buffer source-buffer)
    (if (cdr buffer-list)
	(let ((dir (expand-file-name default-directory)))
	  (setq buffer-list (sort buffer-list
	   (lambda (a b)
	     (let ((ma (progn
			 (set-buffer a)
			 (try-completion
			  "" (list (list dir)
				   (list (expand-file-name
					  default-directory))))))
		   (mb (progn
			 (set-buffer b)
			 (try-completion
			  "" (list (list dir)
				   (list (expand-file-name
					  default-directory)))))))
	       (> (length ma) (length mb))))))
	  (if (eq source-buffer (car buffer-list))
	      (car (cdr buffer-list)) (car buffer-list)))
      (car buffer-list))))

(defun z-purge-nil (list)
  "Return the LIST, with all NIL elements purged.  This is mainly here to
avoid using mapcan."
  (if (null list)
      list
    (while (and list (null (car list)))
      (setq list (cdr list)))
    (let ((l list))
      (while (and l (cdr l))
	(if (null (car (cdr l)))
	    (setcdr l (cdr (cdr l)))
	  (setq l (cdr l)))))
    list))

(defun z-switch-to-buffer (prefix regexp &optional tail command)
  "Mangle windows according to the PREFIX arg, after having switched to
the nearest buffer mathing the REGEXP.  Goto the last point if the
optional TAIL if non-nil.  If a buffer can not be found or if there is
only one buffer which is already current, execute the optional COMMAND."
  (let ((bs (z-purge-nil (mapcar
			  (lambda (b)
			    (if (string-match regexp (buffer-name b)) b nil))
			  (buffer-list)))))
    (if (null bs)
	(if command
	    (eval (list command))
	  (error "No buffer matching %s" regexp))
      (if (cdr bs)
	  (z-mangle-windows arg (z-nearest-buffer bs))
	(let ((already (and command (eq (car bs) (current-buffer)))))
	  (z-mangle-windows arg (car bs))
	  (if already (eval (list command)))))
      (if tail (goto-char (point-max))))))

;;; Shortcut file finds.
(defun z-find-file-shortcut (arg)
  "Shortcut find file from character read from keyboard.  Prefix arg
determines window mangling."
  (interactive "p")
  (let* ((ch (read-char))
	 (file (assoc ch z-find-file-shortcut-files)))
    (if file (setq file (cdr file))
	(let ((file-list (directory-files z-find-file-shortcut-directory t
			  (concat "^" (regexp-quote (char-to-string ch)) ".*")
			  t)))
	  (while (consp file-list)
	    (if (not (backup-file-name-p (car file-list)))
		(if file
		    (error "No unique shortcut for character `%c'" ch)
		  (setq file (car file-list))))
	    (setq file-list (cdr file-list)))))
    (if file
	(z-mangle-windows arg (find-file-noselect file))
      (error "No shortcut for character `%c'" ch))))

;;; Insertions.
(defun z-insert-header-into-mail-buffer (header-field)
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator)
    (let ((search-bound (match-beginning 0)))
      (goto-char (point-min))
      (if (not (search-forward header-field search-bound t))
          (progn
            ;; Insert the field from the previous line not to mess with
            ;; highlighting.
            (goto-char (1- search-bound))
            (insert ?\n header-field))))))

(defun z-insert-bcc ()
  "Insert a `bcc' header field to the user in a mail being composed."
  (interactive)
  (z-insert-header-into-mail-buffer (concat "BCC: " (user-login-name))))

(defun z-insert-date (long-form)
  "Insert the current date.  With a prefix argument, insert the full date,
including the time (excluding the timezone)."
  (interactive "P")
  (let ((time (current-time-string)))
    (insert
     (or (and long-form time)
	 (progn
	   (string-match " [0-9][0-9]:[0-9][0-9]:[0-9][0-9][^0-9]*" time)
	   (concat (substring time 0 (1+ (match-beginning 0)))
		   (substring time (match-end 0))))))))

(defun z-insert-name ()
  "Insert the value of z-inserted-name at point, adjust preceding spaces,
as needed."
  (interactive)
  (if (not (looking-at "^"))
      (progn (just-one-space) (insert ? )))
  (insert z-inserted-name))

(defun z-insert-first-name ()
  "Insert the value of z-inserted-first-name at point, adjust preceding
spaces, as needed."
  (interactive)
  (if (not (looking-at "^"))
      (progn (just-one-space) (insert ? )))
  (insert z-inserted-first-name))

(defun z-insert-mail-address ()
  "Insert the value of user-mail-address at point."
  (interactive)
  (insert user-mail-address))

(defun z-insert-remark (pfix)
  "Insert a new remark in the current buffer.  With prefix arg,
include user's email address"
  (interactive "P")
  (goto-char (point-max))
  (let ((start (re-search-backward "^\\( *\\)\\([0-9]+\\)\\( *\\)\\["
				   (point-min) t)))
    (goto-char (point-max))
    (if start
	(insert ?\n (z-match-buffer-string 1)
		(int-to-string (1+ (string-to-int (z-match-buffer-string 2))))
		(z-match-buffer-string 3) "[")
      (insert "  1 ["))
    (z-insert-date t)
    (and pfix (insert ", " user-mail-address))
    (insert  "] ")))

;;; Display binary information
(defun z-display-binary (num-bytes)
  "Display, in the minibuffer, the interpretation of the bytes at point.
The numeric prefix arg defines the number of bytes to interpret."
  (interactive "p")
  (let ((value 0) (i 0) (sub (buffer-substring (point) (+ (point) num-bytes))))
      (while (< i num-bytes)
	(setq value (+ (* 256 value) (elt sub (if z-binary-little-endian
						  (- num-bytes 1 i) i))))
	(setq i (1+ i)))
      (message "(%d) %d byte%s: %d (0x%08x)" (point) num-bytes
	       (if (= num-bytes 1) "" "s") value value)))

;;; Various
(defun z-compilation-buffer-name (mode)
  (string-match "\\([^/]*\\)/$" default-directory)
  (concat "*" (downcase mode) "*"
          (substring default-directory (match-beginning 1) (match-end 1))
           "*"))

(setq compilation-buffer-name-function 'z-compilation-buffer-name)

(defun z-fill-this-paragraph (prefix)
  "Set (and leave) the fill-prefix to the whitespace starting this line
and invoke fill-paragraph."
  (interactive "P")
  (save-excursion
    (beginning-of-line nil)
    (looking-at "[ \t]*")
    (setq fill-prefix (buffer-substring (match-beginning 0) (match-end 0))))
  (fill-paragraph prefix))

(defun z-kill-buffer-lines (keep)
  "Kill all lines from the current line (minus KEEP lines) to the
start of the buffer."
  (interactive "P")
  (save-excursion
    (forward-line (if keep (- 0 keep) 0))
    (delete-region (point-min) (point))))

(defun z-kill-this-buffer ()
  "Kill the current buffer.  No questions asked."
  (interactive)
  (kill-buffer (current-buffer)))

(defun switch-to-other-buffer ()
  "Switch to another buffer.  No questions asked."
  (interactive)
  (switch-to-buffer nil))

(defun switch-to-other-buffer-other-window ()
  "Switch to another buffer in the other window.  No questions asked."
  (interactive)
  (switch-to-buffer-other-window nil))

(defun z-goto-cvs (arg)
  "Switch to the cvs buffer as identified by z-cvs-buffer-name, mangling
windows according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-cvs-buffer-name))

(defun z-goto-gdb (arg)
  "Switch to the debugger as identified by z-gdb-buffer-name, mangling windows
according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-gdb-buffer-name))

(defun z-goto-zenirc (arg)
  "Switch to the irc buffer by z-zenirc-buffer-name, mangling windows
according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-zenirc-buffer-name))

(defun z-tail-compilation (arg)
  "Switch to and tail the compilation buffer as identified by
z-compilation-buffer-name, mangling windows according to the numeric
prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-compilation-buffer-name t))

(defun z-tail-grep (arg)
  "Switch to and tail the grep buffer as identified by
z-grep-buffer-name, mangling windows according to the numeric
prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-grep-buffer-name t))

(defun z-tail-shell (arg)
  "Switch to the shell, mangling windows according to the numeric prefix ARG."
  (interactive "p")
  (z-switch-to-buffer arg z-shell-buffer-name t 'shell))

(defun z-insert-day ()
  "Insert the day in the format yyww.d"
  (interactive)
  (let* ((s (calendar-iso-from-absolute
	     (calendar-absolute-from-gregorian (calendar-current-date))))
	 (zs (if (> (car s) 9) "" "0"))
	 (y (% (caddr s) 100))
	 (zy (if (> y 9) "" "0")))
    (insert zy (number-to-string y)
	    zs (number-to-string (car s))
	    "." (number-to-string (cadr s)))))

;;; Create and activate the maps.
(defvar ctl-z-map (make-keymap) "Keymap for C-z commands.")
(defvar ctl-z-i-map (make-sparse-keymap "Insert")
  "Keymap for C-z i commands.")
(defvar ctl-z-t-map (make-sparse-keymap "TAGS")
  "Keymap for C-z t (TAGS) commands.")
(fset 'Control-Z-prefix ctl-z-map)
(fset 'Control-Z-I-prefix ctl-z-i-map)
(fset 'Control-Z-T-prefix ctl-z-t-map)
(define-key global-map "\C-z" 'Control-Z-prefix)

;;; Fill them
(z-define-keys ctl-z-map
	       '(
		 ("\C-c" . compile)
		 ("\C-r" . recompile)
		 ("c"    . z-tail-compilation)

		 ("\C-d" . gdb)
		 ("d"    . z-goto-gdb)

		 ("\C-e" . grep)
		 ("e"    . z-tail-grep)

		 ("\C-s" . z-tail-shell)
		 ("s"    . z-tail-shell)

		 ("\C-u"  . cvs-update)
		 ("u"     . z-goto-cvs)

		 ("\C-l" . goto-line)
		 ("l"    . what-line)

		 ("\C-b" . bury-buffer)
		 ("\C-k" . kill-compilation)
		 ("\C-n" . next-error)
		 ("\C-p" . previous-error)
		 ("\C-q" . z-fill-this-paragraph)
		 ("q" . sql-send-region)
		 ("\C-z" . z-display-binary)

		 ("%" . query-replace-regexp)
		 ("b" . switch-to-other-buffer)
		 ("f" . z-find-file-shortcut)
		 ("g" . goto-char)
		 ("i" . Control-Z-I-prefix)
		 ("j" . z-kill-buffer-lines)
		 ("k" . z-kill-this-buffer)
		 ("m" . manual-entry)
		 ("o" . switch-to-other-buffer-other-window)
                 ("t" . Control-Z-T-prefix)
                 ("z" . z-goto-zenirc)))

(z-define-keys ctl-z-i-map
	       '(("b" . z-insert-bcc)
		 ("c" . z-insert-day)
		 ("d" . z-insert-date)
		 ("j" . z-insert-first-name)
		 ("m" . z-insert-mail-address)
		 ("n" . z-insert-name)
		 ("r" . z-insert-remark)))

(z-define-keys ctl-z-t-map
	       '(("s" . tags-search)
		 ("%" . tags-query-replace)
		 ("a" . tags-apropos)
		 ("t" . select-tags-table)
		 ("v" . visit-tags-table)
		 ("l" . list-tags)))

;; Set some other goodies if on NeXTSTEP.
(if (and (boundp 'window-system) (eq window-system 'ns))
    (require 'z-ns-map))

;;; Done.
(provide 'ctl-z-map)
