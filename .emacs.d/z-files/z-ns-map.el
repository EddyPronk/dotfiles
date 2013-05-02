;;; z-ns-map.el --- Hack some useful nextstep bindings.

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
;;;   o  loaded by ctl-z-map if applicable.

(defun z-hide-emacs ()
  "Hide emacs.  This is a command."
  (interactive)
  (hide-emacs t))

(defun z-scroll-lines-down (arg)
  "Scroll down prefix lines."
  (interactive "p")
  (scroll-down arg))

(defun z-scroll-lines-up (arg)
  "Scroll up prefix lines."
  (interactive "p")
  (scroll-up arg))

(setq ns-use-open-panel nil
      ns-use-yes-no-panel t)

(z-define-keys global-map
	       '(([?\s-k] . z-kill-buffer-lines)
		 ([s-escape] . next-error)
		 ([M-s-escape] . previous-error)
		 ([M-up] . z-scroll-lines-down)
		 ([M-down] . z-scroll-lines-up)
		 ([S-M-up] . scroll-down)
		 ([S-M-down] . scroll-up)
		 ([?\s-o] . find-file)
		 ([ns-open-file] . ns-find-file)
		 ([ns-open-temp-file] . ns-find-file)
		 ([?\s- ] . blink-matching-open)))

(z-define-keys ctl-z-map
	       '(("\C-z" . z-hide-emacs)))

;;; Done.
(provide 'z-ns-map)
;; hoi
