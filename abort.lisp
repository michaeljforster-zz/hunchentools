;;;; abort.lisp

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

(defun abort-with-return-code (return-code)
  "Abort handling of the request as if the handler had returned
RETURN-CODE."
  (setf (hunchentoot:return-code*) return-code)
  (hunchentoot:abort-request-handler))

(defun abort-with-bad-request ()
  "Abort handling of the request as if the handler had returned
HUNCHENTOOT:+HTTP-BAD-REQUEST+."
  (abort-with-return-code hunchentoot:+http-bad-request+))

(defun abort-with-forbidden ()
  "Abort handling of the request as if the handler had returned
HUNCHENTOOT:+HTTP-FORBIDDEN+."
  (abort-with-return-code hunchentoot:+http-forbidden+))

(defun abort-with-not-found ()
  "Abort handling of the request as if the handler had returned
HUNCHENTOOT:+HTTP-NOT-FOUND+."
  (abort-with-return-code hunchentoot:+http-not-found+))

(defun abort-with-internal-server-error ()
  "Abort handling of the request as if the handler had returned
HUNCHENTOOT:+HTTP-INTERNAL-SERVER-ERROR+."
  (abort-with-return-code hunchentoot:+http-internal-server-error+))
