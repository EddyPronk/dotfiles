;;; z-files.el --- initialize use of the z-files

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
;;;   o  load this from your .emacs after having set z-files-directory
;;;	 to point to the actual directory containing the z-files.

;; Directory containing the z-files.  Set this to be the actual directory
;; containing the z-files before loading z-files-startup or executing the
;; rest of this file (depending on how you load this).
(defvar z-files-directory "/usr/gnu/lib/emacs/site-lisp/z-files")

;; Extend the load path, protecting against multiple evaluations.
(or (member z-files-directory load-path)
    (setq load-path (cons z-files-directory load-path)))

(require 'ctl-z-map)
(require 'z-adjustments)

(provide 'z-files)
