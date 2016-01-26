;;;; string-escaping.lisp

;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2013 Michael J. Forster
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(in-package #:hunchentools)

(defun escape-string-custom (string test escape-write-function)
  "Given a string STRING, return a new string, encoding with the
function denoted by ESCAPE-WRITE-FUNCTION every character for which the
function denoted by TEST returns true.

Use this in place of CL-WHO:ESCAPE-STRING where custom encoding is required."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
	 while char
	 do (if (funcall test char)
		(funcall escape-write-function char out)
		(write-char char out))))))

(defun escape-string-js (string)
  "Given a string STRING, return a new string, escaping all ASCII values
less than 256 with the \xHH format and those greater than or equal to
256 with the \uHHHH format."
  (escape-string-custom string 
                        (complement #'alphanumericp)
                        #'(lambda (char out)
                            (let ((char-code (char-code char)))
                              (format out
                                      (if (< char-code 256)
                                          "\\x~:@(~2,'0x~)"
                                          "\\u~:@(~4,'0x~)")
                                      char-code)))))
