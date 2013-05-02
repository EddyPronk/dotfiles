;;; z-adjustments.el --- Mangle other Emacs stuff to remove unwanted features.

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
;;;   o  loaded by z-files-startup as desired.

(defvar z-modes-killed '(makefile-mode)
  "List of modes which are definitely undesirable and which should be killed
from the auto-mode-alist.")

(defun z-kill-modes (modes)
  "Remove the MODES (a list of symbols) from the auto-mode-alist."
  (setq auto-mode-alist
	(z-purge-nil (mapcar (lambda (elt)
			       (if (memq (let ((e (cdr elt)))
					   (if (consp e) (car e) e)) modes)
				   nil elt)) auto-mode-alist))))

;;; And now for some action!
(z-kill-modes z-modes-killed)
(setq-default z-mode-line-format
 '("--%1*%1+--%b--" (-3 . "%p") "--" global-mode-string "--"
   "%[(" mode-name minor-mode-alist "%n" mode-line-process ")%]--" "-%-"))

;;; Make sure we `understand' TOM sources.
(if (not (assoc "\\.t$" auto-mode-alist))
    (setq auto-mode-alist (cons '("\\.t$" . objc-mode) auto-mode-alist)))

;;; This is useful when in gud mode.
(defun set-directory (directory &optional buffer)
  "Set the DIRECTORY of the BUFFER, which defaults to the current buffer."
  (interactive "DDefault directory (for this buffer): ")
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (setq default-directory directory)))

;;; Done.
(provide 'z-adjustments)
