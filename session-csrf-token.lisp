;;;; csrf-token.lisp

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

(defvar *the-random-state* (make-random-state t))

;; Borrowed, with rename, from Hunchentoot.
(defun random-string (&optional (n 10) (base 16))
  "Return a random number \(as a string) with base BASE and N digits."
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~VR" base
              (random base *the-random-state*)))))

(defvar *csrf-secret* (random-string 10 36))

(let ((csrf-id-counter 0))
  (defun next-csrf-token ()
    (ironclad:byte-array-to-hex-string 
     (ironclad:digest-sequence 
      :sha256
      (ironclad:ascii-string-to-byte-array
       (format nil "~A~A~A" *csrf-secret* (get-universal-time) (incf csrf-id-counter)))))))

(defun session-csrf-token ()
  "Return the current CSRF token set for the session or a new token if
one has not been set.  If the session does not exist, return NIL."
  (multiple-value-bind (token presentp)
      (hunchentoot:session-value 'csrf-token)
    (if presentp
	token
        (setf (hunchentoot:session-value 'csrf-token) (next-csrf-token)))))

(defun delete-session-csrf-token ()
  "Remove the CSRF token, if any, from the session."
  (hunchentoot:delete-session-value 'csrf-token))

(defun require-session-csrf-token (request-type &optional (name "csrf-token"))
  "Abort handling the request, log a warning message, and remove any
session CSRF token if the request of type denoted by REQUEST-TYPE does
not provide a value for the parameter NAME, if the provided value does
not match the session CSRF token, or if the session CSRF token has not
been set.  Othewise, do nothing. REQUEST-TYPE can be one
of :GET, :POST, :PUT, or :DELETE. NAME defaults to \"csrf-token\"."
      (flet ((forbid (reason)
	       (hunchentoot:log-message* :warning
					 "~A (User-Agent: '~A', IP: '~A')"
					 reason
					 (hunchentoot:user-agent)
					 (hunchentoot:remote-addr*))
               ;; vs. (hunchentoot:remove-session) ERROR!
	       (hunchentoot:delete-session-value 'csrf-token)
	       (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
	       (hunchentoot:abort-request-handler)))
	(let ((csrf-token (ecase request-type
			    ((:get)
			     (hunchentoot:get-parameter name))
			    ((:post :put :delete)
			     (hunchentoot:post-parameter name)))))
	  (multiple-value-bind (token presentp)
	      (hunchentoot:session-value 'csrf-token)
	    (unless presentp
	      (forbid "Missing session CSRF token."))
	    (unless (and (stringp csrf-token) (string/= csrf-token ""))
	      (forbid "Fake CSRF token."))
	    (unless (equal csrf-token token)
	      (forbid "Invalid CSRF token"))))))
