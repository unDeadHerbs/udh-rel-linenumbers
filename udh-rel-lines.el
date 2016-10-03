;;; udh-line-numbers.el --- Display informative line numbers in the margin  -*- lexical-binding: t -*-

;; Author: Murray Fordyce <undeadherbs@gmail.com>
;; URL:
;; Package-Version:
;; Version: 0.2
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2016, Murray Fordyce
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Code:
(defgroup udh-line-numbers nil
  "Show relative line numbers in the margin at regular intervals."
  :group 'convenience
  :prefix "udh-line-numbers-")

(defvar udh-line-numbers-mod-value 5
  "The line period of relative line numbers")
(make-variable-buffer-local 'udh-line-numbers-mod-value)

(defcustom udh-line-numbers-conditional #'udh-line-numbers-mod
  "The funciton used to decide which lines should be given as a relitive position.
The funciton should take one integer argument: the line's distance, in
lines, from the current line, and return a boolean."
  :type 'function
  :group 'udh-line-numbers)

;;;###autoload
(define-minor-mode udh-line-numbers-mode
  "Display relative line number in the left margin at
regular increments.

Toggle uDH Relative Line Numbers on or off.

With a prefix argument ARG, enable uDH Relative Line Numbers mod if ARG
is positive, and disable if otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if the ERG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (udh-line-numbers--off)
  (when udh-line-numbers-mode
    (udh-line-numbers--on)))

;;;###autoload
(define-globalized-minor-mode global-udh-line-numbers-mode
  udh-line-numbers-mode
  (lambda ()
    (unless (minibufferp)
      (udh-line-numbers-mode))))

;;needs updating
(defun udh-line-numbers--on ()
  "Set up `udh-line-numbers-mode'.")

;;needs updating
(defun udh-line-numbers--off ()
  "Tear down `udh-line-numbers-mode'.")

(defun udh-line-numbers-mod (v)
    (= 0 (mod v udh-line-numbers-mod-value)))

;;needs renaming
(defun rpad (s n)
  (if (>= (length s) n)
      s
    (concat (rpad s (- n 1)) " ")))

(defun udh-line-number-calc-fmt (line curline maxlen)
  (if (funcall udh-line-numbers-conditional (abs (- line curline)))
      (format (format " %%%ds" maxlen) (abs (- line curline)))
    (rpad (number-to-string line) (1+ maxlen))))

(defun udh-linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (let ((curline (line-number-at-pos))
	(maxlen (length (number-to-string (count-lines (point-min) (point-max))))))
    (goto-char (window-start win))
    (let ((line (line-number-at-pos))
	  (limit (window-end win t))
	  (fmt (cond ((stringp linum-format) linum-format)
		     ((eq linum-format 'dynamic)
		      (let ((w (length (number-to-string
					(count-lines (point-min) (point-max))))))
			(concat "%" (number-to-string w) "d")))))
	  (width 0))
      (run-hooks 'linum-before-numbering-hook)
      ;; Create an overlay (or reuse an existing one) for each
      ;; line visible in this window, if necessary.
      (while (and (not (eobp)) (< (point) limit))
	(let* ((str (if fmt
			(propertize (udh-line-number-calc-fmt line curline maxlen) 'face 'linum)
		      (funcall linum-format line)))
	       (visited (catch 'visited
			  (dolist (o (overlays-in (point) (point)))
			    (when (equal-including-properties
				   (overlay-get o 'linum-str) str)
			      (unless (memq o linum-overlays)
				(push o linum-overlays))
			      (setq linum-available (delq o linum-available))
			      (throw 'visited t))))))
	  (setq width (max width (length str)))
	  (unless visited
	    (let ((ov (if (null linum-available)
			  (make-overlay (point) (point))
			(move-overlay (pop linum-available) (point) (point)))))
	      (push ov linum-overlays)
	      (overlay-put ov 'before-string
			   (propertize " " 'display `((margin left-margin) ,str)))
	      (overlay-put ov 'linum-str str))))
	;; Text may contain those nasty intangible properties, but that
	;; shouldn't prevent us from counting those lines.
	(let ((inhibit-point-motion-hooks t))
	  (forward-line))
	(setq line (1+ line)))
      (set-window-margins win width (cdr (window-margins win))))))

(defun udh-linum-update-window-advice (original-function win)
  (if (bound-and-true-p udh-line-numbers-mode)
      (udh-linum-update-window win)
    (funcall original-function win)))

(advice-add 'linum-update-window :around #'udh-linum-update-window-advice)

(provide 'udh-line-numbers)

