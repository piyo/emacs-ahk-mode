;;; ahk-mode.el --- major mode for editing AutoHotKey scripts for X/GNU Emacs

;; Copyright (C) 2005 Robert Widhopf-Fenk

;; Author:   Robert Widhopf-Fenk
;; Keywords: AutoHotKey
;; X-URL:    http://www.robf.de/Hacking/elisp
;; arch-tag: 1ae180cb-002e-4656-bd9e-a209acd4a3d4
;; Version:  $Id$


;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Commentary:
;;
;; AutoHotKey: Automation, Hotkeys and Scripting for Windows at
;; http://www.autohotkey.com/ is a cool tool to make daily life
;; with Windows easier or even fun!
;;
;; This is a X/GNU Emacs mode for editing AutoHotKey scripts.
;;
;; Place this file somewhere in your load-path, byte-compile it and add the
;; following line to your ~/.xemacs/init.el:
;;   (require 'ahk-mode)
;; 
;; When opening a script file you will get:
;; - syntax highlighting
;; - completion of commands and variables (bound to tab)
;; - indention (bound to tab)
;; - electric braces (typing { will also insert })
;;
;; Please send bug-reports or feature suggestions to hackATrobfDOTde.

;;; History:
;;
;; is stored in my arch repository ... 

(eval-when-compile
  (require 'cl))

;;; Code:
(defgroup ahk-mode nil
  "A mode for AutoHotKey"
  :group 'languages)

(defcustom ahk-mode-hook nil
  "Hook run by `ahk-mode'."
  :type 'hook
  :group 'ahk-mode)

(defcustom ahk-indetion 2
  "The indetion level."
  :type 'integer
  :group 'ahk-mode)

(defcustom ahk-syntax-directory nil
  "The indetion level."
  :type 'directory 
  :group 'ahk-mode)

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.ahk$"  . ahk-mode))

(defvar ahk-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; these are also allowed in variable names
    (modify-syntax-entry ?#  "w" syntax-table)
    (modify-syntax-entry ?_  "w" syntax-table)
    (modify-syntax-entry ?@  "w" syntax-table)
    (modify-syntax-entry ?$  "w" syntax-table)
    (modify-syntax-entry ??  "w" syntax-table)
    (modify-syntax-entry ?[  "w" syntax-table)
    (modify-syntax-entry ?]  "w" syntax-table)
    syntax-table)
  "Syntax table used in `ahk-mode' buffers.")

(defvar ahk-mode-abbrev-table
  (let ((a (make-abbrev-table)))
    a)
  "Abbreviation table used in `ahk-mode' buffers.")

(defvar ahk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [tab] 'ahk-indent-line-and-complete)
    (define-key map "{" 'ahk-electric-brace)
    (define-key map "}" 'ahk-electric-brace)
    (define-key map [return] 'ahk-electric-return)
    map)
  "Keymap used in `ahk-mode' buffers.")

(defvar ahk-Commands-list nil
  "A list of ahk commands and parameters.
Will be initialized by `ahk-init'")

(defvar ahk-Keys-list nil
  "A list of ahk key names.
Will be initialized by `ahk-init'")

(defvar ahk-Keywords-list nil
  "A list of ahks keywords.
Will be initialized by `ahk-init'")

(defvar ahk-Variables-list nil
  "A list of ahks variables.
Will be initialized by `ahk-init'")

(defvar ahk-mode-font-lock-keywords nil
  "Syntax highlighting for `ahk-mode'.
Will be initialized by `ahk-init'")

;(easy-menu-define ahk-menu ahk-mode-map "AHK Mode Commands"
;		  (cons "AHK" ("0.1" ahk-mode-menu ahk)))

(defun ahk-init ()
  "Initializes ahk-mode variables.
An AHK installation provides a subdirectory \"Extras/Editors/Syntax\"
containing a list of keywords, variables, commands and keys.

This directory must be specified in the variable `ahk-syntax-directory'."
  (interactive)

  (message "Initializing ahk-mode variables ...")
  (when (null ahk-syntax-directory)
    (customize-save-variable
     'ahk-syntax-directory
     (read-directory-name "Please give the AHK-Syntax directory: "))
    (custom-save-all))

  (save-excursion
    (set-buffer (get-buffer-create " *ahk-mode-temp*"))
  
    ;; read commands 
    (erase-buffer)
    (insert-file-contents (expand-file-name "Commands.txt"
                                            ahk-syntax-directory))
    (setq ahk-Commands-list nil)
    (goto-char 0)
    (while (not (eobp))
      (if (not (looking-at "\\(#?[A-Za-z]+\\)\\([^]*\\)?"))
          nil                           ; (error "Unknown file syntax.")
        (setq ahk-Commands-list (cons (list
                                       (match-string 1)
                                       (match-string 2))
                                      ahk-Commands-list)))
      (forward-line 1))
    
    ;; read keys
    (erase-buffer)
    (insert-file-contents (expand-file-name "Keys.txt"
                                            ahk-syntax-directory))
    (setq ahk-Keys-list nil)
    (goto-char 0)
    (while (not (eobp))
      (if (not (looking-at "\\([^;][^\n ]+\\)?"))
          nil;; (error "Unknown file syntax of Keys.txt.")
        (setq ahk-Keys-list (cons (match-string 1) ahk-Keys-list)))
      (forward-line 1))
    
    ;; read keywords  
    (erase-buffer)
    (insert-file-contents (expand-file-name "Keywords.txt"
                                            ahk-syntax-directory))
    (setq ahk-Keywords-list nil)
    (goto-char 0)
    (while (not (eobp))
      (if (not (looking-at "\\([^;][^\n ]+\\)?"))
          nil;; (error "Unknown file syntax of Keywords.txt.")
        (setq ahk-Keywords-list (cons (match-string 1) ahk-Keywords-list)))
      (forward-line 1))
    ;; read variables
    (erase-buffer)
    (insert-file-contents (expand-file-name "Variables.txt"
                                            ahk-syntax-directory))
    (setq ahk-Variables-list nil)
    (goto-char 0)
    (while (not (eobp))
      (if (not (looking-at "\\([^;][^\n ]+\\)?"))
          nil;; (error "Unknown file syntax of Variables.txt.")
        (setq ahk-Variables-list (cons (match-string 1) ahk-Variables-list)))
      (forward-line 1))
  
    (setq ahk-mode-font-lock-keywords 
          (list
           '("\\s-*;.*$" .
             font-lock-comment-face)
           '("^\\([^ \t\n:]+\\):" .
             (1 font-lock-builtin-face))
           '("[^, %\"]*%[^% ]+%" .
             font-lock-variable-name-face)
           ;; I get an error when using regexp-opt instead of simply
           ;; concatenating the keywords and I do not understand why ;-(
           ;; (warning/warning) Error caught in `font-lock-pre-idle-hook': (invalid-regexp Invalid preceding regular expression)
           (cons (mapconcat 'identity (mapcar 'car ahk-Commands-list) "\\|")
                 'font-lock-function-name-face)
           (cons (mapconcat 'identity ahk-Keywords-list "\\|")
                 'font-lock-keyword-face)
           (cons (mapconcat 'identity ahk-Keys-list "\\|")
                 'font-lock-constant-face)
           (cons (mapconcat 'identity ahk-Variables-list "\\|")
                 'font-lock-variable-name-face)
           )))
  
  (message "Initializing ahk-mode variables done."))

;;;###autoload
(defun ahk-mode ()
  "Major mode for editing AutoHotKey Scripts.

The hook `ahk-mode-hook' is run at mode initialization.

Key bindings:
\\{ahk-mode-map}"
  (interactive)
  (if (null ahk-Commands-list)
      (ahk-init))
  (kill-all-local-variables)
  (set-syntax-table ahk-mode-syntax-table)
  (setq major-mode 'ahk-mode
	mode-name "AHK"
	local-abbrev-table ahk-mode-abbrev-table
	abbrev-mode t
        indent-region-function 'ahk-indent-region)
  (put 'ahk-mode 'font-lock-defaults '(ahk-mode-font-lock-keywords t))
  (put 'ahk-mode 'font-lock-keywords-case-fold-search t)

  (when (not (featurep 'xemacs))
    (setq font-lock-defaults '(ahk-mode-font-lock-keywords))
    (setq font-lock-keywords-case-fold-search t))
  
  (use-local-map ahk-mode-map)
;  (easy-menu-add ahk-menu)
  (setq comment-start ";")
  (font-lock-mode 1)
  (force-mode-line-update)
  (run-hooks 'ahk-mode-hook))

(defun ahk-indent-line ()
  "Indent the current line."
  (interactive)

  (let ((indent 0)
        (case-fold-search t))
    ;; do a backward search to determin the indention level
    (save-excursion
      (beginning-of-line)
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (if (looking-at "^[^: ]+:")
          (setq indent 2)
        (if (looking-at "^\\([ \t]*\\){")
            (setq indent (+ (length (match-string 1)) 2))
          (if (looking-at "^\\([ \t]*\\)")
              (setq indent (+ (length (match-string 1))))))))
    ;; check for special tokens
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^\\([ \t]+\\)\\}")
          (setq indent (- indent 2))
        (if (or (looking-at "^[ \t]*[^: \t\n]*::")
                (and (looking-at "^\\([ \t]*\\)\\(Return\\|Exit\\)")
                     (or (<= (length (match-string 1)) 2)
                         (= indent 2)))
                (looking-at "^;;;"))
        (setq indent 0))))
    (let ((p (point-marker)))
      (beginning-of-line)
      (if (looking-at "^[ \t]+")
          (replace-match ""))
      (indent-to indent)
      (goto-char p)
      (set-marker p nil)
      (if (bolp)
          (goto-char (+ (point) indent))))))

(defun ahk-indent-region (start end)
  "Indent lines in region START to END."
  (interactive "r")
  (goto-char end)
  (setq end (point-marker))
  (goto-char start)
  (while (<= (point) end)
    (beginning-of-line)
    (ahk-indent-line)
    (forward-line 1))
  (set-marker end nil))

(defvar ahk-completion-list nil)

(defun ahk-complete ()
  "Indent current line when at the beginning or complete current command."
  (interactive)

  (if (null ahk-completion-list)
      ;; built completion list
      (setq ahk-completion-list
            (mapcar (lambda (c) (list c))
                    (append (mapcar 'car ahk-Commands-list)
                            ahk-Keywords-list
                            ahk-Variables-list
                            ahk-Keys-list))))
  (if (looking-at "\\w+")
      (goto-char (match-end 0)))
  
  (let ((end (point)))
    (if (and (or (save-excursion (re-search-backward "\\<\\w+"))
                 (looking-at "\\<\\w+"))
             (= (match-end 0) end))
        (let ((start (match-beginning 0))
              (prefix (match-string 0))
              (completion-ignore-case t)
              completions)
          (setq completions (all-completions prefix ahk-completion-list))
          (if (eq completions nil)
              nil;(error "Unknown command prefix <%s>!" prefix)
            (if (> (length completions) 1)
                (setq completions
                      (completing-read "Complete command: "
                                       (mapcar (lambda (c) (list c))
                                               completions)
                                       nil t prefix)))
            (if (stringp completions)
                ;; this is a trick to upcase "If" and other prefixes
                (let ((c (try-completion completions ahk-completion-list)))
                  (if (stringp c)
                      (setq completions c))))
            
            (delete-region start end)
            (if (listp completions) (setq completions (car completions)))
            (insert completions)
            (let ((help (assoc completions ahk-Commands-list)))
              (if help (message "%s" (mapconcat 'identity help ""))))
            )))))

(defun ahk-indent-line-and-complete ()
  "Combines indetion and completion."
  (interactive)
  (ahk-indent-line)
  (ahk-complete))

(defun ahk-electric-brace (arg)
  "Insert character ARG and correct line's indentation."
  (interactive "p")
  (if (save-excursion
        (skip-chars-backward " \t")
        (bolp))
      nil
    (ahk-indent-line)
    (newline))
  (self-insert-command arg)
  (ahk-indent-line)
  (newline)
  (ahk-indent-line)

  (let ((event  last-input-event))
    (setq event (if (featurep 'xemacs)
		    (event-to-character (aref event 0))
		  (setq event (if (stringp event) (aref event 0) event))))

    (when (equal event ?{)
      (newline)
      (ahk-indent-line)
      (insert ?})
      (ahk-indent-line)
      (forward-line -1)
      (ahk-indent-line))))

(defun ahk-electric-return ()
  "Insert newline and indent."
  (interactive)
  (ahk-indent-line)
  (newline)
  (ahk-indent-line))

(provide 'ahk-mode)

;;; ahk-mode.el ends here
