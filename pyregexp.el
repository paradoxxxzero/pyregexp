;;; pyregexp.el --- a Python regexp/replace command for emacs with interactive feedback

;; Copyright (C) 2012 Marko Bencun

;; Author : Marko Bencun <mbencun@gmail.com>
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
;; It is a command for emacs which enables you to use Python regular expressions and either a Python string or a Python expression for doing replacements.
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

;;; Installation
;;
;; Add the following code to your init file. Of course you can select
;; your own key.
;; ----------------------------------------------------------
;; (add-to-list 'load-path "which-folder-pyregexp-file-in/")
;; (require 'pyregexp)
;; (define-key global-map (kbd "C-c r") 'pyregexp-replace)
;; ----------------------------------------------------------
;; To customize, use: M-x customize-group [RET] pyregexp.
;; You can specify how to invoke the Python interpreter by modifying the pyregexp-command-prefix variable. The default is "python ...".
;;
;; Execute C-h f "pyregexp-replace" to read more and see examples.

;; Code goes here

(unless (fboundp 'make-overlay)
  (require 'overlay))

;; cl is used for (loop ...) macro
(eval-when-compile 
  (require 'cl))

;;; faces

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

;;; variables

(defconst pyregexp-filename (expand-file-name "pyregexp.py" (file-name-directory load-file-name))
  "Path to pyregexp.py")

(defcustom pyregexp-command-prefix (format "python %s" pyregexp-filename)
  "External script to compute the replacements."
  :group 'pyregexp)

(defcustom pyregexp-minibuffer-help-regexp
  "C-c ?: help, C-c i: toggle case, C-c m: toggle multiline match of ^ and $, C-c s: toggle dot matches newline, C-c a: toggle show all"
  "Help string shown in minibuffer when entering regexp."
  :group 'pyregexp)

(defcustom pyregexp-minibuffer-help-replace
  "C-c ?: help, C-c C-c: toggle expression, C-c m: show matches/groups, C-c a: toggle show all"
  "Help string shown in minibuffer when entering replacement."
  :group 'pyregexp)

(defcustom pyregexp-default-feedback-limit 50
  "Limit number of matches shown in visual feedback. 
If nil, don't limit the number of matches shown in visual feedback."
  :group 'pyregexp)
(defcustom pyregexp-default-regexp-modifiers '(M)
  "Modifiers that are applied by default. All modifiers are: '(I M S U).
See also: http://docs.python.org/library/re.html#re.I"
  :group 'pyregexp)

;;; private variables

(defconst pyregexp-match-faces '(pyregexp-match-0 pyregexp-match-1)
  "Faces in list for convenience")

(defconst pyregexp-group-faces '(pyregexp-group-0 pyregexp-group-1 pyregexp-group-2)
  "Faces in list for convenience")

(defconst pyregexp-overlay-priority 1001
  "Starting priority of pyregexp overlays.")

(defvar pyregexp-in-minibuffer nil
  "Is pyregexp currently being used?")

(defvar pyregexp-last-minibuffer-contents nil
  "Keeping track of minibuffer changes")

(defvar pyregexp-target-buffer-start nil
  "Starting position in target buffer.")

(defvar pyregexp-target-buffer-end nil
  "Ending position in target buffer.")

(defvar pyregexp-regexp-string nil
  "Entered regexp.")

(defvar pyregexp-replace-string nil
  "Entered replacement.")

(defvar pyregexp-use-expression nil
  "Use expression instead of string in replacement.")

(defvar pyregexp-feedback-limit nil
  "Feedback limit currently in use.")

(defvar pyregexp-target-buffer nil
  "Buffer to which pyregexp is applied to.")

(defvar pyregexp-overlays (make-hash-table :test 'equal)
  "Overlays used in target buffer.")

(defvar pyregexp-visible-overlays (list)
  "Overlays currently visible.")

;; modifiers IMSU (see http://docs.python.org/library/re.html#re.I)
(defvar pyregexp-regexp-modifiers '()
  "Modifiers in use.")


;; (make-variable-buffer-local 'pyregexp-in-minibuffer)
;; (make-variable-buffer-local 'pyregexp-last-minibuffer-contents)
;; (make-variable-buffer-local 'pyregexp-target-buffer-start)
;; (make-variable-buffer-local 'pyregexp-target-buffer-end)
;; (make-variable-buffer-local 'pyregexp-regexp-string)
;; (make-variable-buffer-local 'pyregexp-replace-string)
;; (make-variable-buffer-local 'pyregexp-use-expression)
;; (make-variable-buffer-local 'pyregexp-feedback-limit)
;; (make-variable-buffer-local 'pyregexp-target-buffer)
;; (make-variable-buffer-local 'pyregexp-overlays)
;; (make-variable-buffer-local 'pyregexp-visible-overlays)
;; (make-variable-buffer-local 'pyregexp-modifiers)

;;; keymap

(defvar pyregexp-minibuffer-regexp-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (pyregexp-minibuffer-help)))

    ;; C-i is also <tab>. http://stackoverflow.com/questions/1792326/how-do-i-bind-a-command-to-c-i-without-changing-tab
    (setq map (delq '(kp-tab . [9]) map))
    ;;(keyboard-translate ?\C-i ?\H-i)
    (define-key map (kbd "C-c i") (lambda () (interactive) (pyregexp-toggle-regexp-modifier 'I)))
    (define-key map (kbd "C-c m") (lambda () (interactive) (pyregexp-toggle-regexp-modifier 'M)))
    (define-key map (kbd "C-c s") (lambda () (interactive) (pyregexp-toggle-regexp-modifier 'S)))
    (define-key map (kbd "C-c u") (lambda () (interactive) (pyregexp-toggle-regexp-modifier 'U)))

    (define-key map (kbd "C-c a") 'pyregexp-toggle-limit)
    map)
  "Keymap used while using pyregexp,")

(defvar pyregexp-minibuffer-replace-keymap 
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "C-c ?") (lambda () (interactive) (pyregexp-minibuffer-help)))
    (define-key map (kbd "C-c C-c") (lambda () (interactive) 
				      (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
					(setq pyregexp-use-expression (not pyregexp-use-expression))
					(pyregexp-update-minibuffer-prompt)
					(pyregexp-do-replace-feedback))))
    (define-key map (kbd "C-c m") (lambda ()
				    (interactive)
				    (when (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
				      (pyregexp-delete-overlay-displays)
				      ;; wait for any input to redisplay replacements
				      (sit-for 100000000 t)
				      (pyregexp-do-replace-feedback))))

    (define-key map (kbd "C-c a") 'pyregexp-toggle-limit)
    map)
  "Keymap used while using pyregexp,")


;;; helper functions

(defun pyregexp-toggle-limit ()
  "Toggle the limit of overlays shown (default limit / no limit)"
  (interactive)
  (if pyregexp-feedback-limit
      (setq pyregexp-feedback-limit nil)
    (setq pyregexp-feedback-limit pyregexp-default-feedback-limit))
  (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	 (pyregexp-regexp-feedback))
	((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	 (pyregexp-regexp-feedback t) ;; update overlays
	 (pyregexp-do-replace-feedback))))

(defun pyregexp-toggle-regexp-modifier(modifier)
  (setq pyregexp-regexp-modifiers 
	(if (memq modifier pyregexp-regexp-modifiers)
	    (delq modifier pyregexp-regexp-modifiers)
	  (cons modifier pyregexp-regexp-modifiers)))
  (pyregexp-update-minibuffer-prompt)
  (pyregexp-regexp-feedback))

(defun pyregexp-get-regexp-string ()
  (concat (pyregexp-get-regexp-modifiers-prefix) 
	  (if (equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp) 
	      (minibuffer-contents-no-properties) 
	    pyregexp-regexp-string)))

(defun pyregexp-get-regexp-modifiers-prefix ()
  "Construct (?imsu) prefix based on selected modifiers."
  (let (modifiers-sorted)
    (mapc (lambda (m)
	    (when (memq m pyregexp-regexp-modifiers)
	      (setq modifiers-sorted (cons m modifiers-sorted)))
	    ) '(I M S U) ;; sort according to this order
	  )
    (let ((s (mapconcat 
	      'identity 
	      (delq nil 
		    (mapcar (lambda (m)
			      (cond ((equal m 'I) "i")
				    ((equal m 'M) "m")
				    ((equal m 'S) "s")
				    ((equal m 'U) "u")
				    (t nil)))
			    (nreverse modifiers-sorted)) )
	      "")))
      (if (string= "" s) "" (format "(?%s)" s)))))


;;; minibuffer functions

(defun pyregexp-minibuffer-set-prompt (prompt)
  "Updates minibuffer prompt. Call when minibuffer is active."
  (let ((inhibit-read-only t)) 
    (put-text-property (point-min) (minibuffer-prompt-end) 'display prompt)))

(defun pyregexp-update-minibuffer-prompt ()
  (when (and pyregexp-in-minibuffer (minibufferp))
    (pyregexp-minibuffer-set-prompt
     (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	    (format "Regexp: %s" (pyregexp-get-regexp-modifiers-prefix)))
	   ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	    (concat
	     "Replace"
	     (when pyregexp-use-expression " (using expression)")
	     (format " (%s)" (pyregexp-get-regexp-string))
	     ": "))))))

(defun pyregexp-minibuffer-message (message)
  "Minibuffer message without timeout"
  (let ((minibuffer-message-timeout nil))
    (minibuffer-message message)))

(defun pyregexp-minibuffer-help ()
  (cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	 (when pyregexp-minibuffer-help-regexp
	   (pyregexp-minibuffer-message pyregexp-minibuffer-help-regexp)))
	((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	 (when pyregexp-minibuffer-help-replace
	   (pyregexp-minibuffer-message pyregexp-minibuffer-help-replace)))))

;;; overlay functions

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
	(overlay-put overlay 'priority (+ pyregexp-overlay-priority (if (= j 0) 0 1)))
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
	      (overlay-put overlay 'priority pyregexp-overlay-priority))))
	pyregexp-visible-overlays))

;;; hooks

(defun pyregexp-update (beg end len)
  (when (and pyregexp-in-minibuffer (minibufferp))
    ;; minibuffer-up temporarily deletes minibuffer contents before inserting new one.
    ;; don't do anything then as the messages shown my pyregexp are irritating while browsing the history.
    (unless (and (string= "" (minibuffer-contents-no-properties))
		 (equal last-command 'previous-history-element))
      ;; do something when minibuffer contents changes
      (unless (string= pyregexp-last-minibuffer-contents (minibuffer-contents-no-properties))
	(setq pyregexp-last-minibuffer-contents (minibuffer-contents-no-properties))
	;; minibuffer contents has changed, update visual feedback.
	;; not using after-change-hook because this hook applies to the whole minibuffer, including minibuffer-messages
	;; that disappear after a while.
	(cond ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	       (pyregexp-regexp-feedback))
	      ((equal pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	       (pyregexp-do-replace-feedback)))))))
(add-hook 'after-change-functions 'pyregexp-update)

(defun pyregexp-minibuffer-setup ()
  "Setup prompt and help when entering minibuffer."
  (when pyregexp-in-minibuffer
    (progn
      (pyregexp-update-minibuffer-prompt)
      (pyregexp-minibuffer-help))))
(add-hook 'minibuffer-setup-hook 'pyregexp-minibuffer-setup)

;;; shell command / parsing functions

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

(defun pyregexp-run-command (args success)
  (multiple-value-bind (output exit-code) (pyregexp-command args)
    (cond ((equal exit-code 0) 
	   (funcall success output))
	  ((equal exit-code 1)
	   (message "script failed:%s\n" output)))))

(defun pyregexp-not-last-line () 
  "Output of external script ends in one line of message and one empty line.
Return t if current line is not the line with the message."
  (save-excursion (= 0 (forward-line 2))))

(defun pyregexp-current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun pyregexp-unescape (s &optional newline)
  "Replacement strings returned by external script have escaped newlines and backslashes (so that there can be one replacement per line). Unescape to get back original.
Escaped newlines are only unescaped if newline is not nil."
  (when newline
    (setq s (replace-regexp-in-string (regexp-quote "\\n") (regexp-quote "\n") s)))
  (replace-regexp-in-string (regexp-quote "\\\\") (regexp-quote "\\") s))
(defun pyregexp-parse-matches (s callback)
  "Parse string s with positions of matches and groups as returned by external script. For each position, callback is called with arguments (i j begin end),
i being the match and j the group index and begin/end being the span of the match.
The message line is returned.
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
    message-line))

(defun pyregexp-parse-replace (s)
  "Parse string s with positions of matches and replacements as returned by external script.
Returns a list, in reverse order, of (replacement begin end i) (i = index of match = index of corresponding overlay)
and the message line."
  (let ((replacements (list)) ;; store replacements (lines of output) in list
	message-line) ;; store message line (last non-empty line of output)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (loop while (and (pyregexp-not-last-line) (/= (line-beginning-position) (line-end-position))) ;; loop until empty line is reached
	    for i from 0 do 
	    (re-search-forward "\\([0-9]+\\) \\([0-9]+\\) " (line-end-position) t)
	    (let ((replacement (buffer-substring-no-properties (point) (line-end-position)))
		  (begin (+ pyregexp-target-buffer-start (string-to-number (match-string 1))))
		  (end (+ pyregexp-target-buffer-start (string-to-number (match-string 2)))))
	      (setq replacements (cons (list replacement begin end i) replacements)))
	    (forward-line 1))
      (setq message-line (pyregexp-unescape (pyregexp-current-line) t)))
    (list replacements message-line)))

;;; helper functions

(defun pyregexp-target-window ()
  (if pyregexp-target-buffer
      (get-buffer-window pyregexp-target-buffer)
    nil))

(defun pyregexp-compose-messages(&rest msgs)
  (mapconcat 'identity (delq nil (mapcar (lambda (msg) (if (or (not msg) (string= "" msg)) nil msg)) msgs)) " - "))

;;; show feedback functions

(defun pyregexp-regexp-feedback (&optional inhibit-message)
  "Show visual feedback for matches."
  (pyregexp-delete-overlays)
  (let ((limit-reached nil) 
	message-line)
    (setq message-line 
	  (pyregexp-run-command 
	   (format "%s matches --regexp %s %s" pyregexp-command-prefix (shell-quote-argument (pyregexp-get-regexp-string)) (when pyregexp-feedback-limit (format "--feedback-limit %s" pyregexp-feedback-limit)))
	   '(lambda (output)
	      (pyregexp-parse-matches
	       output 
	       '(lambda (i j begin end) 
		  (when (= 0 i) ;; first match: if invisible, make it visible.
		    (with-selected-window (pyregexp-target-window)
		      (if (>= begin (window-end nil t))
			  (goto-char begin))))
		  (let ((overlay (pyregexp-get-overlay i j)))
		    (move-overlay overlay begin end pyregexp-target-buffer)
		    (setq pyregexp-visible-overlays (cons overlay pyregexp-visible-overlays)))
		  ;; mark if we have reached the specified feedback limit	  
		  (when (and pyregexp-feedback-limit (= pyregexp-feedback-limit (+ i 1)) )
		    (setq limit-reached t)))))))
    (unless inhibit-message
      (let ((msg (pyregexp-compose-messages message-line (when limit-reached (format "%s matches shown, hit C-c a to show all" pyregexp-default-feedback-limit)))))
	(unless (string= "" msg)
	  (pyregexp-minibuffer-message msg))))))

(defun pyregexp-format-replace-feedback (original replacement)
  (format "%s => %s" original replacement))

(defun pyregexp-do-replace-feedback ()
  "Show visual feedback for replacements."
  (pyregexp-delete-overlay-displays)
  (let ((replace-string (minibuffer-contents-no-properties)))
    (pyregexp-run-command 
     (format "%s replace %s --feedback %s --regexp %s --replace %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (when pyregexp-feedback-limit (format "--feedback-limit %s" pyregexp-feedback-limit)) (shell-quote-argument (pyregexp-get-regexp-string)) (shell-quote-argument replace-string))
     '(lambda (output)
	(multiple-value-bind (replacements message-line) (pyregexp-parse-replace output)
	  ;; visual feedback for matches
	  (loop for replacement-info in replacements do 
		(multiple-value-bind (replacement begin end i) replacement-info
		  (let* ((overlay (pyregexp-get-overlay i 0))
			 (empty-match (equal (overlay-start overlay) (overlay-end overlay))))
		    (unless empty-match
		      (let ((original (with-current-buffer pyregexp-target-buffer (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))))
			(overlay-put overlay 'display (pyregexp-format-replace-feedback original (pyregexp-unescape replacement)))
			(overlay-put overlay 'priority (+ pyregexp-overlay-priority 2)))))))
	  
	  (unless (string= "" message-line)
	    (pyregexp-minibuffer-message message-line)))))))

;;; replace / pyregexp-replace

(defun pyregexp-do-replace ()
  "Replace matches."
  (pyregexp-delete-overlay-displays)
  (pyregexp-delete-overlays)
  (let ((replace-string pyregexp-replace-string))
    (pyregexp-run-command 
     (format "%s replace %s --regexp %s --replace %s" pyregexp-command-prefix (if pyregexp-use-expression "--eval" "") (shell-quote-argument (pyregexp-get-regexp-string)) (shell-quote-argument replace-string))
     '(lambda (output)
	(multiple-value-bind (replacements message-line) (pyregexp-parse-replace output)
	  ;; replace in target buffer
	  (loop for replacement-info in replacements do 
		(multiple-value-bind (replacement begin end i) replacement-info
		  ;; replace match
		  (let ((replacement (pyregexp-unescape replacement t)))
		    (with-current-buffer pyregexp-target-buffer
		      (save-excursion
			;; first insert, then delete
			;; this ensures that if we had an active region before, the replaced match is still part of the region
			(goto-char begin)
			(insert replacement)
			(delete-char (- end begin)))))))
	(unless (string= "" message-line)
	  (pyregexp-minibuffer-message message-line)))))))

(defun pyregexp-interactive-get-args ()
  "Get interactive args for the pyregexp-replace function."
  (unwind-protect 
      (progn
	(when pyregexp-in-minibuffer (error "pyregexp already in use."))
	(setq pyregexp-target-buffer (current-buffer))
	(setq pyregexp-use-expression current-prefix-arg)
	(setq pyregexp-target-buffer-start (if (and transient-mark-mode mark-active) 
					       (region-beginning)
					     (point)))
	(setq pyregexp-target-buffer-end (if (and transient-mark-mode mark-active) 
					     (region-end)
					   (point-max)))

	(setq pyregexp-feedback-limit pyregexp-default-feedback-limit)
	(setq pyregexp-feedback-limit-reached nil)
	(setq pyregexp-regexp-modifiers pyregexp-default-regexp-modifiers)

	(save-excursion
	  ;; deactivate mark so that we can see our faces instead of region-face.
	  (deactivate-mark)
	  (progn 
	    (setq pyregexp-in-minibuffer 'pyregexp-minibuffer-regexp)
	    (setq pyregexp-last-minibuffer-contents "")
	    (setq pyregexp-regexp-string 
		  (read-from-minibuffer 
		   " " ;; prompt will be  set in pyregexp-minibuffer-setup
		   nil pyregexp-minibuffer-regexp-keymap))
	    ;;(setq pyregexp-regexp-string (format "%s%s" (pyregexp-get-regexp-modifiers-prefix) pyregexp-regexp-string))
	    
	    (setq pyregexp-in-minibuffer 'pyregexp-minibuffer-replace)
	    (setq pyregexp-last-minibuffer-contents "")
	    (setq pyregexp-replace-string
		  (read-from-minibuffer 
		   " " ;; prompt will be  set in pyregexp-minibuffer-setup 
		   nil pyregexp-minibuffer-replace-keymap))))
	
	(list 
	 pyregexp-regexp-string 
	 pyregexp-replace-string
	 pyregexp-target-buffer-start
	 pyregexp-target-buffer-end
	 pyregexp-regexp-modifiers
	 pyregexp-use-expression))
    (progn ;; execute on finish
      (setq pyregexp-in-minibuffer nil)
      (pyregexp-delete-overlay-displays)
      (pyregexp-delete-overlays))))

(defun pyregexp-replace (regexp replace start end &optional modifiers use-expression)
  "Regexp-replace with interactive feedback, using Python regular expressions. 
When used interactively with prefix arg, the replacement string is a Python expression. The Python expression has access to the following variables:
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
  (interactive 
   (pyregexp-interactive-get-args))
  (unwind-protect 
      (progn 
	(when pyregexp-in-minibuffer (error "pyregexp already in use."))
	(setq pyregexp-target-buffer (current-buffer))
	(setq pyregexp-target-buffer-start start)
	(setq pyregexp-target-buffer-end end)
	(setq pyregexp-modifiers (if modifiers modifiers '()))
	(setq pyregexp-use-expression use-expression)
	(setq pyregexp-regexp-string regexp)
	(setq pyregexp-replace-string replace)
	;; do replacement
	(pyregexp-do-replace))
    ;; execute on finish
    (setq pyregexp-in-minibuffer nil)))

(provide 'pyregexp)

;;; pyregexp.el ends here
