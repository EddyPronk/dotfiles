;; z-files

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(push "~/.emacs.d" load-path)
(push "~/.emacs.d/z-files" load-path)
(load-library "z-files")

;; disable startup message
(setq inhibit-startup-message t)
(tool-bar-mode nil)
(menu-bar-mode nil)
(menu-bar-no-scroll-bar)
;(menu-bar-right-scroll-bar)
(setq x-select-enable-clipboard t)
(setq kill-whole-line 0)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(setq default-tab-width 4)
;(setq split-height-threshold 0)
(setq split-width-threshold nil)

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;<f1>			help-command
;<f2>			2C-command
;<f3>			kmacro-start-macro-or-insert-counter
;<f4>			kmacro-end-or-call-macro
(global-set-key [f5] 'eval-last-sexp)
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'hwn-compile)
(global-set-key [f8] 'ido-find-file)
(global-set-key [f9] 'ido-switch-buffer)
;(global-set-key [f10] 'other-window)
;(global-set-key [f11] 'z-tail-shell)
(global-set-key [f12] 'save-buffer)
(global-set-key [f13] 'undo)
(global-set-key [S-f13] 'pwd)

(global-set-key [?\C-,] 'previous-error)
(global-set-key [?\C-.] 'next-error)
(global-set-key [?\C-=] 'hwn-compile)
;(global-set-key [?\C-=] 'eval-buffer)

(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-S-iso-lefttab] 'previous-buffer)

;;
;; Make all "yes or no" prompts show "y or n" instead
;;
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on the parenthesis highlight mode.

(show-paren-mode t)

;(require 'psvn)
;;(add-to-list 'vc-handled-backends 'SVN)
;(defconst svn-status-default-diff-arguments '("--diff-cmd" "diff" "-x" "-wbBu"))

(require 'compile)
(defun hwn-compile ()
  "save all, kill or start compile"
  (interactive)
  (save-some-buffers t)
;;  (switch-to-buffer-other-window "*compilation*build*")
  (if compilation-in-progress (kill-compilation) (recompile)))

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;;
;; C++ stuff
;;

(require 'cc-mode)
(push
 '("sutter" ;; Ellemtel without indenting namespaces
   (c-basic-offset . 4)
   (tab-width . 4)
   (indent-tabs-mode t)
   (c-comment-only-line-offset . 0)
   (c-hanging-braces-alist . ((substatement-open before after)))
   (c-offsets-alist . ((topmost-intro        . 0)
		       (substatement         . +)
		       (substatement-open    . 0)
		       (case-label           . +)
		       (access-label         . -)
		       (inclass              . +)
		       (inline-open          . 0)
		       (innamespace . 0))))
 c-style-alist)

;(setq auto-mode-alist
;	(append (list
;		'("\\.h" "\\.c$" . c++-mode)) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;(load-library "mantara-style")

(add-hook 'c++-mode-hook
	  '(lambda ()
	     (if (not (equal c-indentation-style 
			     (default-value 'c-indentation-style)))
(c-set-style "sutter"))))
	;	 (c-set-style "mantara")
;;	       (tab-width 4)
;;	       (c-set-style c-indentation-style))))

(require 'sql)

(defun sql-create-buffer (profile)
  "derived from sql-product-interactive in sql.el which is"
  "covered by GNU General Public License version 3."

  (setq sql-user     (cdr (assoc 'user profile))
		sql-password (cdr (assoc 'password profile))
		sql-server   (cdr (assoc 'server profile))
		sql-database (cdr (assoc 'database profile)))
  (setq product 'postgres) ;;(cdr (assoc 'product profile)))
  (when (sql-product-feature :sqli-connect product)
    (if (comint-check-proc "*SQL*")
	(pop-to-buffer "*SQL*")
      ;; Connect to database.
      (message "Login...")
      (funcall (sql-product-feature :sqli-connect product))
      ;; Set SQLi mode.
      (setq sql-interactive-product product)
      (setq sql-buffer (current-buffer))
      (sql-interactive-mode)
      ;; All done.
      (message "Login...done")
      ;(pop-to-buffer sql-buffer)
	  )))

;(setq sga-profile '(
;	  (product  . "postgres")
;	  (user     . "")
;	  (password . "")
;	  (server   . "zeus4.research.phg.com.au")
;	  (database . "sga")))

;(sql-create-buffer sga-profile)

; select description from stocks where ric = 'BHP.AX';


(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
       (set-frame-parameter nil 'fullscreen
                            (if (equal 'fullboth current-value)
                                (if (boundp 'old-fullscreen) old-fullscreen nil)
                                (progn (setq old-fullscreen current-value)
                                       'fullboth)))))

(global-set-key [f11] 'toggle-fullscreen)
(global-set-key (kbd "C-c r") 'revert-buffer)

;;
;; Ediff setup
;;

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-grab-mouse nil)
(setq ediff-diff-options "-d")
;; =======================================================================================
;(require 'column-marker)
;(column-marker-1 80)

;; ;; Highlight column 80 in foo mode.
;(add-hook 'c++-mode-hook (lambda () (interactive) (column-marker-1 80)))
(put 'erase-buffer 'disabled nil)

; Not sure what emacs -rv does, but I think its pretty close.
(set-background-color "black")
(set-face-background 'default "black")
(set-face-background 'region "black")
(set-face-foreground 'default "white")
(set-face-foreground 'region "gray60")
(set-foreground-color "white")
(set-cursor-color "red")

;(load-library "robot-mode.el")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (string-inflection multiple-cursors magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(customize-set-variable 'search-whitespace-regexp nil)
