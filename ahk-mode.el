;;; ahk-mode.el --- AutoHotKey code editing commands for XEmacs

;; Copyright (C) Robert Widhopf-Fenk

;; Author: Robert Widhopf-Fenk
;; Homepage: http://www.robf.de/Hacking/elisp
;; Keywords: AutoHotKey
;; arch-tag: 1ae180cb-002e-4656-bd9e-a209acd4a3d4
;; Version:  $Id$

;; This mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:
;;
;; AutoHotKey: Automation, Hotkeys and Scripting for Windows at
;; http://www.autohotkey.com/ is a cool tool to make daily life
;; with Windows easier or even fun!
;;
;; This is a XEmacs mode for editing AutoHotKey scripts.
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
;; 0.1.2: first public release

(eval-when-compile
  (require 'cl))

;;; Code:
(defgroup ahk-mode nil
  "An mode for AutoHotKey"
  :group 'languages)

(defcustom ahk-mode-hook nil
  "*Hook run by `ahk-mode'."
  :type 'hook
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
    (define-key map [(control c) (control c)] 'comment-region)
    (define-key map [tab] 'ahk-indent-line-and-complete)
    (define-key map [{] 'ahk-electric-brace)
    (define-key map [}] 'ahk-electric-brace)
    (define-key map [return] 'ahk-electric-return)
    map)
  "Keymap used in `ahk-mode' buffers.")

(defvar ahk-keyword-list
  '("AutoTrim" "BlockInput" "Break" "ClipWait" "Continue" "Control"
    "ControlClick" "ControlFocus" "ControlGet" "ControlGetFocus"
    "ControlGetPos" "ControlGetText" "ControlMove" "ControlSend"
    "ControlSendRaw" "ControlSetText" "CoordMode" "DetectHiddenText"
    "DetectHiddenWindows" "Drive" "DriveGet" "DriveSpaceFree" "Edit" "Else"
    "EnvAdd" "EnvDiv" "EnvMult" "EnvSet" "EnvSub" "EnvUpdate" "Exit" "ExitApp"
    "FileAppend" "FileCopy" "FileCopyDir" "FileCreateDir" "FileCreateShortcut"
    "FileDelete" "FileInstall" "FileGetAttrib" "FileGetShortcut" "FileGetSize"
    "FileGetTime" "FileGetVersion" "FileMove" "FileMoveDir" "FileRead"
    "FileReadLine" "FileRecycle" "FileRecycleEmpty" "FileRemoveDir"
    "FileSelectFile" "FileSelectFolder" "FileSetAttrib" "FileSetTime"
    "FormatTime" "GetKeyState" "Gosub" "Goto" "GroupActivate" "GroupAdd"
    "GroupClose" "GroupDeactivate" "GUI" "GuiControl" "GuiControlGet"
    "HideAutoItWin" "Hotkey" "If" "IfEqual" "IfNotEqual" "IfExist"
    "IfNotExist" "IfGreater" "IfGreaterOrEqual" "IfInString" "IfNotInString"
    "IfLess" "IfLessOrEqual" "IfMsgBox" "IfWinActive" "IfWinNotActive"
    "IfWinExist" "IfWinNotExist" "ImageSearch" "IniDelete" "IniRead"
    "IniWrite" "Input" "InputBox" "KeyHistory" "KeyWait" "LeftClick"
    "LeftClickDrag" "ListHotkeys" "ListLines" "ListVars" "Loop" "Menu"
    "MouseClick" "MouseClickDrag" "MouseGetPos" "MouseMove" "MsgBox" "OnExit"
    "OutputDebug" "Pause" "PixelGetColor" "PixelSearch" "PostMessage"
    "Process" "Progress" "Random" "RegDelete" "RegRead" "RegWrite" "Reload"
    "Repeat" "EndRepeat" "Return" "RightClick" "RightClickDrag" "Run" "RunAs"
    "RunWait" "Send" "SendRaw" "SendMessage" "SetBatchLines"
    "SetCapslockState" "SetControlDelay" "SetDefaultMouseSpeed" "SetFormat"
    "SetKeyDelay" "SetMouseDelay" "SetNumlockState" "SetScrollLockState"
    "SetStoreCapslockMode" "SetTimer" "SetTitleMatchMode" "SetWinDelay"
    "SetWorkingDir" "Shutdown" "Sleep" "Sort" "SoundBeep" "SoundGet"
    "SoundGetWaveVolume" "SoundPlay" "SoundSet" "SoundSetWaveVolume"
    "SplashImage" "SplashTextOn" "SplashTextOff" "SplitPath"
    "StatusBarGetText" "StatusBarWait" "StringCaseSense" "StringGetPos"
    "StringLeft" "StringLen" "StringLower" "StringMid" "StringReplace"
    "StringRight" "StringSplit" "StringTrimLeft" "StringTrimRight"
    "StringUpper" "Suspend" "SysGet" "Thread" "ToolTip" "Transform" "TrayTip"
    "URLDownloadToFile" "WinActivate" "WinActivateBottom" "WinClose"
    "WinGetActiveStats" "WinGetActiveTitle" "WinGetClass" "WinGet" "WinGetPos"
    "WinGetText" "WinGetTitle" "WinHide" "WinKill" "WinMaximize"
    "WinMenuSelectItem" "WinMinimize" "WinMinimizeAll" "WinMinimizeAllUndo"
    "WinMove" "WinRestore" "WinSet" "WinSetTitle" "WinShow" "WinWait"
    "WinWaitActive" "WinWaitClose" "WinWaitNotActive")
  "A list of ahk commands used for syntax highlighting and completion.")

(defvar ahk-directive-list
  '("#AllowSameLineComments" "#CommentFlag" "#ErrorStdOut" "#EscapeChar"
    "#HotkeyInterval" "#HotkeyModifierTimeout" "#Hotstring" "#Include"
    "#InstallKeybdHook" "#InstallMouseHook" "#KeyHistory"
    "#MaxHotkeysPerInterval" "#MaxMem" "#MaxThreads" "#MaxThreadsBuffer"
    "#MaxThreadsPerHotkey" "#NoTrayIcon" "#Persistent" "#SingleInstance"
    "#UseHook" "#WinActivateForce" )
  "A list of ahk directives.")

(defvar ahk-internal-variable-list
  '("A_ScriptFullPath" "A_AhkVersion" "A_IsCompiled" "A_ExitReason" "A_YYYY"
    "A_MM" "A_DD" "A_MMMM" "A_MMM" "A_DDDD" "A_DDD" "A_WDay" "A_YDay"
    "A_YWeek" "A_Hour" "A_Min" "A_Sec" "A_Now" "A_NowUTC" "A_TickCount"
    "A_IsSuspended" "A_BatchLines" "A_TitleMatchMode" "A_TitleMatchModeSpeed"
    "A_DetectHiddenWindows" "A_DetectHiddenText" "A_AutoTrim"
    "A_StringCaseSense" "A_FormatInteger" "A_FormatFloat" "A_KeyDelay"
    "A_WinDelay" "A_ControlDelay" "A_MouseDelay" "A_DefaultMouseSpeed"
    "A_IconHidden" "A_IconTip" "A_IconFile" "A_IconNumber" "A_TimeIdle"
    "A_TimeIdlePhysical" "A_Gui" "A_GuiControl" "A_GuiWidth" "A_GuiHeight"
    "A_GuiControlEvent" "A_ThisMenuItem" "A_ThisMenu" "A_ThisMenuItemPos"
    "A_ThisHotkey" "A_PriorHotkey" "A_TimeSinceThisHotkey"
    "A_TimeSincePriorHotkey" "A_EndChar" "A_OSType" "A_OSVersion" "A_Language"
    "A_ComputerName" "A_UserName" "A_WinDir" "A_ProgramFiles" "A_Desktop"
    "A_DesktopCommon" "A_StartMenu" "A_StartMenuCommon" "A_Programs"
    "A_ProgramsCommon" "A_Startup" "A_StartupCommon" "A_MyDocuments"
    "A_IsAdmin" "A_ScreenWidth" "A_ScreenHeight" "A_IPAddress1" "A_Cursor"
    "A_CaretX" "A_CaretY" "A_Index" "A_LoopFileName" "A_LoopRegName"
    "A_LoopReadLine" "A_LoopField"

    "ErrorLevel" "Clipboard")
  "A list of ahks internal variables.")

(defvar ahk-mode-font-lock-keywords
  (list
   '("\\s-*;.*$" .  font-lock-comment-face)
   '("^\\([^ \t\n:]+\\):" . (1 font-lock-builtin-face))
   '("[^, %\"]*%[^% ]+%" . font-lock-variable-name-face)
   (cons (concat "^" (regexp-opt ahk-directive-list)) 'font-lock-preprocessor-face)
   (cons (regexp-opt ahk-keyword-list) 'font-lock-function-name-face)
   )
  "Syntax highlighting for `ahk-mode'.")

;(easy-menu-define ahk-menu ahk-mode-map "AHK Mode Commands"
;		  (cons "AHK" ("0.1" ahk-mode-menu ahk)))

;;;###autoload
(defun ahk-mode ()
  "Major mode for editing AutoHotKey Scripts.

The hook `ahk-mode-hook' is run at mode initialization.

Key bindings:
\\{ahk-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table ahk-mode-syntax-table)
  (setq major-mode 'ahk-mode
	mode-name "AHK"
	local-abbrev-table ahk-mode-abbrev-table
	abbrev-mode t
        indent-region-function 'ahk-indent-region)
  (put 'ahk-mode 'font-lock-defaults '(ahk-mode-font-lock-keywords t))
  (put 'ahk-mode 'font-lock-keywords-case-fold-search t)

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
        (if (or (looking-at "^[ \t]*[^: \t\n]*:")
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
                    (append ahk-keyword-list
                            ahk-directive-list
                            ahk-internal-variable-list
                            ahk-completion-list))))
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
              (error "Unknown command prefix <%s>!" prefix)
            (if (> (length completions) 1)
                (setq completions
                      (completing-read "Complete command: "
                                       (mapcar (lambda (c) (list c))
                                               completions)
                                       nil t prefix))))
          (if (stringp completions)
              ;; this is a trick to upcase "If" and other prefixes
              (setq completions (try-completion completions ahk-completion-list)))

          (delete-region start end)
          (if (listp completions) (setq completions (car completions)))
          (insert completions)))))

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
  (when (equal (event-to-character last-input-event) ?{)
    (newline)
    (ahk-indent-line)
    (insert ?})
    (ahk-indent-line)
    (forward-line -1)
    (ahk-indent-line)))

(defun ahk-electric-return ()
  "Insert newline and indent."
  (interactive)
  (ahk-indent-line)
  (newline)
  (ahk-indent-line))

(provide 'ahk-mode)

;;; ahk-mode.el ends here
