;;; pyregexp.el --- a Python regexp/replace command for emacs with interactive feedback

;; Copyright (C) 2012 Marko Bencun

;; Author : benma <mbencun@gmail.com>
;; URL : https://github.com/benma/pyregexp/
;; Version : 0.1
;; Keywords : regexp, replace, python

;; This file is part of pyregexp.

;; pyregexp is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; pyregexp is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with pyregexp.  If not, see <http://www.gnu.org/licenses/>.

;;; INTRODUCTION
;;

;; What's this?
;;
;; It is a command for emacs, enabling you to use Python regular expressions and either a Python string or a Python expression for doing replacements.
;; While constructing the regexp in the minibuffer, you get live visual feedback for the matches, including group matches.
;; While constructing the replacement in the minibuffer, you get live visual feedback for the replacements.

;; Where does pyregexp come from?
;;
;; I was not happy with the way I used emacs' replace-regexp before. Constructing the regular expression is error prone and emacs' regular expressions are limited
;; (for example, no lookaheads, named groups, etc.).
;; Using re-builder to interactively build regular expressions was a step into the right direction, but manually copying over the regexp
;; to to the minibuffer is cumbersome.
;; Using the idea of interactive of of re-builder, this package makes it possible to use just the minibuffer to construct (with live visual feedback) the regexp and replacement,
;; using Python's regular expressions and, optionally, Python expressions for the replacement.
;;
;; So a thanks to Detlev Zundel for his re-builder.

;;; Usage
;;
;; Add the following code to your init file. Of course you can select
;; your own key.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "which-folder-pyregexp-file-in/")
;; (require 'pyregexp)
;; (define-key global-map (kbd "C-c r") 'pyregexp-replace)
;; ----------------------------------------------------------
;; to customize, use: M-x customize-group [RET] pyregexp.
;;
;; Execute C-h f "pyregexp-replace" to read more and see examples.

;; Code goes here

(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for (loop ...) macro

(eval-when-compile 
  (require 'cl))

(defface pyregexp-match-0
  '((((class color) (background light))
     :background "lightblue")
    (((class color) (background dark))
     :background "steelblue4")
    (t
     :inverse-video t))
  "First face for displaying a whole match."
  :group 'pyregexp)

(defface pyregexp-match-1
  '((((class color) (background light))
     :background "cadetblue")
    (((class color) (background dark))
     :background "slateblue4")
    (t
     :inverse-video t))
  "Second face for displaying a whole match."
  :group 'pyregexp)

(defface pyregexp-group-0
  '((((class color) (background light))
     :background "aquamarine")
    (((class color) (background dark))
     :background "blue3")
    (t
     :inverse-video t))
  "First face for displaying a matching group."
  :group 'pyregexp)

(defface pyregexp-group-1
  '((((class color) (background light))
     :background "springgreen")
    (((class color) (background dark))
     :background "chartreuse4")
    (t
     :inverse-video t))
  "Second face for displaying a matching group."
  :group 'pyregexp)

(defface pyregexp-group-2
  '((((min-colors 88) (class color) (background light))
     :background "yellow1")
    (((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "sienna4")
    (t
     :inverse-video t))
  "Third face for displaying a matching group."
  :group 'pyregexp)

(defcustom pyregexp-show-feedback-whole-buffer t
  "Show visual feedback in whole buffer. If nil, show only in visible area."
  :group 'pyregexp)

(defcustom pyregexp-command-prefix (format "python2 %s" pyregexp-filename)
  "Command used to genereate highlighting information."
  :group 'pyregexp)

;; private variables below

(defconst pyregexp-match-faces '(pyregexp-match-0 pyregexp-match-1)
  "Faces in list for convenience")

(defconst pyregexp-group-faces '(pyregexp-group-0 pyregexp-group-1 pyregexp-group-2)
  "Faces in list for convenience")

(defconst pyregexp-filename (expand-file-name "pyregexp.py" (file-name-directory load-file-name))
  "Path to pyregexp.py")

(defconst pyregexp-in-minibuffer nil
  "Is pyregexp currently being used?")

(defvar pyregexp-last-minibuffer-contents nil
  "Keeping track of minibuffer changes")

(defvar pyregexp-target-buffer-start nil
  "Starting position in target buffer.")

(defvar pyregexp-target-buffer-end nil
  "Ending position in target buffer.")

(defvar pyregexp-target-buffer-end-feedback nil
  "Ending position in target buffer during feedback is window-end, ending position of visible area.")

(defvar pyregexp-regexp-string nil
  "Entered regexp.")

(defvar pyregexp-replace-string nil
  "Entered replacement.")

(defvar pyregexp-use-expression nil
  "Use expression instead of string in replacement.")

(defvar pyregexp-target-buffer nil
  "Buffer to which pyregexp is applied to.")

(defvar pyregexp-overlays (make-hash-table :test 'equal)
  "Overlays used in target buffer.")

(defvar pyregexp-visible-overlays (list)
  "Overlays currently visible.")

(defun pyregexp-get-overlay (i j)
  "i: match index, j: submatch index"
  (let (overlay)
    (setq overlay (gethash (list i j) pyregexp-overlays))
    (unless overlay ;; create new one if overlay does not exist yet
      (progn 
	(setq overlay (make-overlay 0 0))
	(if (= 0 j)
	    (overlay-put overlay 'face (nth (mod i (length pyregexp-match-faces)) pyregexp-match-faces))
	  (overlay-put overlay 'face (nth (mod j (length pyregexp-group-faces)) pyregexp-group-faces)))
	(overlay-put overlay 'priority (if (= j 0) 0 1))
	(overlay-put overlay 'pyregexp-ij (list i j))
	(when (= j 0)
	  (overlay-put overlay 'intangible t))
	(puthash (list i j) overlay pyregexp-overlays)
	))
    overlay))

(defun pyregexp-delete-overlays ()
  "Delete all visible overlays."
  (mapc (lambda (overlay)
	     (delete-overlay overlay)) 
	   pyregexp-visible-overlays)
  (setq pyregexp-visible-overlays (list)))

(defun pyregexp-delete-overlay-displays ()
  "Delete the display of all visible overlays. Call before pyregexp-delete-overlays."
  (mapc (lambda (overlay)
	  (multiple-value-bind (i j) (overlay-get overlay 'pyregexp-ij)
	    (when (= 0 j)
	      (overlay-put overlay 'display nil)
	      (overlay-put overlay 'priority 0))))
	pyregexp-visible-overlays))

(defun pyregexp-update ()
  (when (and pyregexp-in-minibuffer (minibufferp))
    ;; delete overlay displays when we are in minibuffer and do any action (like moving the point).
    (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
      (pyregexp-delete-overlay-displays))
    ;; do something when minibuffer contents changes
    (unless (string= pyregexp-last-minibuffer-contents (minibuffer-contents))
      (setq pyregexp-last-minibuffer-contents (minibuffer-contents))
      ;; minibuffer contents has changed, update visual feedback.
      ;; not using after-change-hook because this hook applies to the whole minibuffer, including minibuffer-messages
      ;; that disappear after a while.
      (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	     (pyregexp-regexp-feedback))
	    ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	     (pyregexp-do-replace t)
	     )))))

(add-hook 'post-command-hook 'pyregexp-update)

(defun pyregexp-command (command)
  (let ((stdout-buffer (generate-new-buffer (generate-new-buffer-name " *pyregex stdout*")))
	(output)
	(exit-code))
    (with-current-buffer pyregexp-target-buffer
      (setq exit-code (call-process-region
		       pyregexp-target-buffer-start
		       pyregexp-target-buffer-end
		       shell-file-name
		       nil ;; don't delete region
		       stdout-buffer
		       nil ;; don't redisplay buffer
		       shell-command-switch
		       command
		       )))
    (with-current-buffer stdout-buffer
      (setq output (buffer-string))
      (kill-buffer))
    (list output exit-code)))

(defun pyregexp-not-last-line () 
  "Output of external script ends in one line of message and one empty line.
Return t if current line is not the line with the message."
  (save-excursion (= 0 (forward-line 2))))

(defun pyregexp-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun pyregexp-parse (s callback &optional msg)
  "Parse string s with positions of matches adn groups as returned by external script. For each position, callback is called with arguments (i j begin end),
i being the match and j the group index and begin/end being the span of the match.
"
  (let (message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (let ((offset pyregexp-target-buffer-start))
	(loop while (and (pyregexp-not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	      for i from 0 do
	      (loop while (re-search-forward "\\([0-9]+\\) \\([0-9]+\\)" (line-end-position) t) ;; loop integer pairs in line
		    for j from 0 do
		    (let ((begin (+ offset (string-to-number (match-string 1))))
			  (end (+ offset (string-to-number (match-string 2)))))
		      (funcall callback i j begin end)))
	      (forward-line 1)))
      (setq message-line (pyregexp-unescape (pyregexp-current-line) t)))
    (when msg
      (unless (string= "" message-line) (funcall msg message-line)))))

(defun pyregexp-run-command (args success)
  (multiple-value-bind (output exit-code) (pyregexp-command args)
    (cond ((equal exit-code 0) 
	   (funcall success output))
	  ((equal exit-code 1)
	   (message "script failed:%s\n" output)))))

(defun pyregexp-regexp-feedback ()
  (pyregexp-delete-overlays)
  (when (minibuffer-contents)
    (pyregexp-run-command 
     (format "%s matches %s" pyregexp-command-prefix (shell-quote-argument (minibuffer-contents)))
     '(lambda (output)
	(pyregexp-parse 
	 output 
	 '(lambda (i j begin end) 
	    (when (<= end pyregexp-target-buffer-end-feedback)
	      (let ((overlay (pyregexp-get-overlay i j)))
		(move-overlay overlay begin end pyregexp-target-buffer)
		(setq pyregexp-visible-overlays (cons overlay pyregexp-visible-overlays)))))
	 'minibuffer-message)))))

(defun pyregexp-unescape (s &optional newline)
  "Replacement strings returned by external script have escaped newlines and backslashes (so that there can be one replacement per line). Unescape to get back original.
Escaped newlines are only unescaped if newline is not nil."
  (when newline
    (setq s (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") s)))
  (replace-regexp-in-string (regexp-quote "\\\\") (regexp-quote "\\") s))

(defun pyregexp-format-replace-feedback (original replacement)
  (format "%s => %s" original replacement))

(defun pyregexp-do-replace (&optional feedback)
  "If feedback is not nil, visual feedback for matches will be given. Otherwise, matches will be replaced."
  (if feedback (pyregexp-delete-overlay-displays)
    (progn 
      (pyregexp-delete-overlay-displays)
      (pyregexp-delete-overlays)))
  (let ((replace-string (if feedback (minibuffer-contents) pyregexp-replace-string)))
    (pyregexp-run-command 
     (format "%s replace %s %s %s %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (if feedback "--feedback" "") (shell-quote-argument pyregexp-regexp-string) (shell-quote-argument replace-string))
     '(lambda (output)
	(let ((replacements (list)) ;; store replacements (lines of output) in list
	      (message-line)) ;; store message line (last non-empty line of output)
	  (with-temp-buffer
	    (insert output)
	    (goto-char (point-min))
	    (while (and (pyregexp-not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	      (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) " (line-end-position) t)
	      (let ((replacement (buffer-substring-no-properties (point) (line-end-position)))
		    (begin (+ pyregexp-target-buffer-start (string-to-number (match-string 1))))
		    (end (+ pyregexp-target-buffer-start (string-to-number (match-string 2)))))
		(setq replacements (cons (list replacement begin end) replacements)))
	      (forward-line 1))
	    (setq message-line (pyregexp-unescape (pyregexp-current-line) t))
	    (if feedback
		;; visual feedback for matches
		(loop for i from 0 for replacement-info in (nreverse replacements) do 
		      (multiple-value-bind (replacement begin end) replacement-info
			(when (<= end pyregexp-target-buffer-end-feedback)
			  (let* ((overlay (pyregexp-get-overlay i 0))
				 (empty-match (equal (overlay-start overlay) (overlay-end overlay))))
			    (unless empty-match
			      (let ((original (with-current-buffer pyregexp-target-buffer (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))
				(overlay-put overlay 'display (pyregexp-format-replace-feedback original (pyregexp-unescape replacement)))
				(overlay-put overlay 'priority 2)
				;; (run-at-time "1 sec" nil (lambda (overlay) (overlay-put overlay 'display nil)) overlay)
				))))))
	      (loop for replacement-info in replacements do 
		    (multiple-value-bind (replacement begin end) replacement-info
		      ;; replace match
		      (let ((replacement (pyregexp-unescape replacement t)))
			(with-current-buffer pyregexp-target-buffer
			  (save-excursion
			    (kill-region begin end)
			    (goto-char begin)
			    (insert replacement))))))))
	  (unless (string= "" message-line)
	    (minibuffer-message message-line)))))))

(defun pyregexp-replace (&optional prefix)
  "Regexp-replace with interactive feedback, using Python regular expressions. 
With prefix arg, the replacement string is a Python expression. The Python expression has access to the following variables:
- i: the index of the match
- m: the match object
- \\0, \\1, ...: captured groups (those are aliases for m.group(0), m.group(1), ...).

Example 1: 
regexp: abcd(.)(.)
replace: abc\\2\\1

Example 2: capitalize every word (use prefix arg to use a Python expression)
regexp: \\b\\w
replace: \\0.upper()

Example 3: enumerate all words and put them on new lines (use prefix arg to use a Python expression)
regexp: \\w+
replace: \"\\n{}. {}\".format(i+1, \\0)
"
  (interactive "p")
  (setq pyregexp-target-buffer (current-buffer))
  (setq pyregexp-target-buffer-start (if (and transient-mark-mode mark-active) 
					 (region-beginning)
				       (point)))
  (setq pyregexp-target-buffer-end (if (and transient-mark-mode mark-active) 
				       (region-end)
				     (point-max)))

  (setq pyregexp-target-buffer-end-feedback (if (and transient-mark-mode mark-active) 
						(region-end)
					      (if pyregexp-show-feedback-whole-buffer
						  (window-end nil t)
						(point-max))))

  
  (unwind-protect 
      (progn 
	(setq pyregexp-use-expression (/= 1 prefix))
	(setq pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	(setq pyregexp-last-minibuffer-contents "")
	(setq pyregexp-regexp-string (read-from-minibuffer "Regexp? " ""))
	(setq pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	(setq pyregexp-last-minibuffer-contents "")
	(setq pyregexp-replace-string (read-from-minibuffer (if pyregexp-use-expression "Replace (expression)? " "Replace? ") ""))
	(setq pyregexp-in-minibuffer nil)
	(pyregexp-delete-overlay-displays)
	(pyregexp-delete-overlays)
	;; do replacement
	(pyregexp-do-replace))
    (progn ;; execute on finish
      (setq pyregexp-in-minibuffer nil)
      (pyregexp-delete-overlay-displays)
      (pyregexp-delete-overlays)
      )))

(provide 'pyregexp)

;;; pyregexp.el ends here
