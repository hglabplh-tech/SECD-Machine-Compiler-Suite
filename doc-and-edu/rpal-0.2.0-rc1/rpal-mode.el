;;; rpal-mode.el - Major mode for editing RPAL programs
;; Copyright (C) 2006 Daniel Franke
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(defvar rpal-mode-hook nil)
(defvar rpal-mode-map
  (let ((rpal-mode-map (make-keymap)))
    (define-key rpal-mode-map "\C-j" 'newline-and-indent)
    rpal-mode-map)
  "Keymap for RPAL major mode")

(add-to-list 'auto-mode-alist '("\\.rpal\\'" . rpal-mode))

(defconst rpal-font-lock-keywords-1
  (list
   '("\\<\\(let\\|w\\(ithin\\|here\\)\\|fn\\|rec\\)" . font-lock-keyword-face)
   '("//.*\\'" . font-lock-comment-face)
   '("'\\(\\\\\\\\\\|\\\\'\\|[^\']\\)*'" . font-lock-string-face))
  "Level 1 highlighting expressions for RPAL mode")

(defconst rpal-font-lock-keywords-2
  (append rpal-font-lock-keywords-1
	  (list
	   '("\\<\\(aug\\|or\\|n\\(ot\\|e\\)\\|g\\(r\\|e\\)\\|l\\(e\\|s\\)\\|eq\\)" .
	     font-lock-keyword-face)))
  "Level 2 highlighting expressions for RPAL mode")

(defconst rpal-font-lock-keywords-3
  (append rpal-font-lock-keywords-2
	  (list
	   '("\\<\\(Print\\|Isinteger\\|Istruthvalue\\|Isstring\\|Istuple\\|Isfunction\\|Isdummy\\|Stem\\|Stern\\|Conc\\|Order\\|ItoS\\|Null\\)\\>" .
	     font-lock-builtin-face)))
  "Level 3 highlighting expressions for RPAL mode")

(defvar rpal-font-lock-keywords rpal-font-lock-keywords-3
  "Default highlighting expressions for RPAL mode")

(defun rpal-indent-line ()
  "Indent current line as RPAL code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((parens 'nil)
	  (paren-count 0)
	  (lets 'nil)
	  (let-count 0)
	  (last-close-paren 'nil)
	  (last-in 'nil))
      ;;Determine what occurs on the previous line
      (save-excursion
	(backward-char)
	(beginning-of-line)
	(while (not (eolp))
	  (cond
	   ((looking-at "(") 
	    (setq parens (cons (+ (current-column) 1) parens))
	    (setq paren-count (+ paren-count 1)))
	   ((looking-at ")")
	    (when parens (setq parens (cdr parens)))
	    (setq paren-count (- paren-count 1))
	    (setq last-close-paren (current-column)))
	   ((looking-at "\\<let")
	    (setq lets (cons (+ (current-column) 2) lets))
	    (setq let-count (+ let-count 1)))
	   ((looking-at "\\<in")
	    (when lets (setq lets (cdr lets)))
	    (setq let-count (- let-count 1))
	    (setq last-in (current-column))))
	  (forward-char)))

      (cond 
       ;;The easy cases: 
       (parens (indent-line-to (if (and lets (> (car lets) (car parens)))
				   (car lets)
				 (car parens))))
       (lets (indent-line-to (car lets)))
       
       ((and (< paren-count 0)
	     (or (not last-in)
		 (> last-close-paren last-in)))
	;;Indent to the previous matching paren
	(indent-line-to
	 (save-excursion
	   (backward-char)
	   (beginning-of-line)
	   (backward-char)
	   (while (and (not (bobp)) (< paren-count 0))
	     (when (not (eolp))
	       (cond ((looking-at "(") 
		      (setq paren-count (+ paren-count 1)))
		     ((looking-at ")")
		      (setq paren-count (- paren-count 1)))))
	     (backward-char))
	   (unless (bobp) (forward-char))
	   (current-column))))
       
       ((< let-count 0)
	;;Indent to previous matching let
	(indent-line-to
	 (save-excursion
	   (backward-char)
	   (beginning-of-line)
	   (backward-char)
	   (while (and (not (bobp)) (< let-count 0))
	     (when (not (eolp))
	       (cond ((looking-at "\\<let")
		      (setq let-count (+ let-count 1)))
		     ((looking-at "\\<in")
		      (setq let-count (- let-count 1)))))
	     (backward-char))
	   (unless (bobp) (forward-char))
	   (current-column))))
       (t (indent-line-to (save-excursion 
			    (backward-char)
			    (beginning-of-line)
			    (current-indentation))))))))
	          
(defvar rpal-mode-syntax-table
  (let ((rpal-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" rpal-mode-syntax-table)
    (modify-syntax-entry ?\n ">" rpal-mode-syntax-table)
    (modify-syntax-entry ?/ ". 12" rpal-mode-syntax-table)
    rpal-mode-syntax-table)
  "Syntax table for RPAL mode")

(defun rpal-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map rpal-mode-map)
  (set-syntax-table rpal-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '(rpal-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'rpal-indent-line)
  (setq major-mode 'rpal-mode)
  (setq mode-name "RPAL")
  (run-hooks 'rpal-mode-hook))

(provide 'rpal-mode)