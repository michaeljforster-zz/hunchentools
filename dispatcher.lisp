;;;; dispatcher.lisp

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

(defun create-uri-methods-dispatcher (uri methods handler)
  "Return a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches the string URI and the method of the current request is a
member of the list denoted by METHODS. The dispatch function will
return NIL if there is no match."
  #'(lambda (request)
      (when (and (equal (hunchentoot:script-name request) uri)
                 (member (hunchentoot:request-method* request)
                         (alexandria:ensure-list methods)))
        handler)))

(defun create-prefix-methods-dispatcher (prefix methods handler)
  "Return a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
starts with the string PREFIX and the method of the current request is
a member of the list denoted by METHODS. The dispatch function will
return NIL if there is no match."
  #'(lambda (request)
      (let ((mismatch (mismatch (hunchentoot:script-name request)
				prefix
				:test #'char=))
	    (method (hunchentoot:request-method* request)))
	(and (or (null mismatch)
		 (>= mismatch (length prefix)))
	     (member method (alexandria:ensure-list methods))
	     handler))))

(defun create-regex-methods-dispatcher (regex methods handler)
  "Return a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches the CL-PPCRE regular expression REGEX and the method of the
current request is a member of the list denoted by METHODS. The
dispatch function will return NIL if there is no match."
  (let ((scanner (cl-ppcre:create-scanner regex)))
    #'(lambda (request)
	(let ((method (hunchentoot:request-method* request)))
	  (and (cl-ppcre:scan scanner (hunchentoot:script-name request))
	       (member method (alexandria:ensure-list methods))
	       handler)))))
