;;; relative-line-numbers.el --- Display relative line numbers on the margin  -*- lexical-binding: t -*-

;; Author: Murray Fordyce <undeadherbs@gmail.com>
;; URL: 
;; Package-Version: 
;; Version: 0.1
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

;; So we can use it's dealing with emacs internals.
(require 'relative-line-numbers)

(defgroup udh-relative-line-numbers nil
  "Show relative line numbers in the margin at regular intervals."
  :group 'convenience
  :prefix "udh-relative-line-numbers-")

(defvar udh-relative-line-numbers-mod-value 5
  "The line period of relative line numbers")
(make-variable-buffer-local 'udh-relative-line-numbers-mod-value)

(defcustom udh-relative-line-numbers-conditional #'udh-relative-line-numbers-mod
  "The funciton used to decide which lines should be given as a relitive position.
The funciton should take one integer argument: the line's distance, in
lines, from the current line, and return a boolean."
  :type 'function
  :group 'udh-relative-line-numbers)

;;;###autoload
(define-minor-mode udh-relative-line-numbers-mode
  "Display relative line number in the left margin at
regular increments.

Toggle uDH Relative Line Numbers on or off.

With a prefix argument ARG, enable uDH Relative Line Numbers mod if ARG
is positive, and disable if otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if the ERG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (udh-relative-line-numbers--off)
  (when udh-relative-line-numbers-mode
    (udh-relative-line-numbers--on)))

;;;###autoload
(define-globalized-minor-mode global-udh-relative-line-numbers-mode
  udh-relative-line-numbers-mode
  (lambda ()
    (unless (minibufferp)
      (udh-relative-line-numbers-mode))))

(defun udh-relative-line-numbers--on ()
  "Set up `udh-relative-line-numbers-mode'."
  (relative-line-numbers-mode 1)
  (setq relative-line-numbers-format 'udh-relative-line-numbers-format))

(defun udh-relative-line-numbers--off ()
  "Tear down `udh-relative-line-numbers-mode'."
  (relative-line-numbers-mode 0)
  (setq relative-line-numbers-format 'relative-line-numbers-default-format))

(defun udh-relative-line-numbers-mod (v)
    (= 0 (mod v udh-relative-line-numbers-mod-value)))

(defun udh-relative-line-numbers-format (offset)
  (if (funcall udh-relative-line-numbers-conditional offset)
      (format (format " %%%ds"
		      (length (number-to-string (1+ (count-lines (point-min) (point-max))))))
	      (relative-line-numbers-default-format offset))
    (concat "" (number-to-string (1+ (count-lines (point-min) (point)))) " ")))

(provide 'udh-relative-line-numbers)
