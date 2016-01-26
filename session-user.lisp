;;;; session-user.lisp

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

(defun session-user ()
  "Return the user set for the session. Return NIL if the session does
not exist or if no user has been set.

SETF of SESSION-USER can be used to set a new user for the session. If
the session does not exist then one is created. Signal a correctable
error of type TYPE-ERROR if SETF of SESSION-USER is called with a NIL
value for user."
  (multiple-value-bind (user presentp)
      (hunchentoot:session-value 'user)
    (and presentp user)))

(defun (setf session-user) (new-user)
  (check-type new-user (not null))
  (setf (hunchentoot:session-value 'user) new-user))

(defun delete-session-user ()
  "Remove the user, if any, from the session."
  (hunchentoot:delete-session-value 'user))

(defun require-session-user (&rest redirect-args)
  "Log a warning message and redirect if no user is set for the
session. Otherwise, do nothing. REDIRECT-ARGS are used as keyword
arguments to HUNCHENTOOT:REDIRECT."
  (multiple-value-bind (user presentp)
      (hunchentoot:session-value 'user)
    (declare (ignore user))
    (unless presentp
      (hunchentoot:log-message* :warning
                                "Unauthorised (User-Agent: '~A', IP: '~A')"
                                (hunchentoot:user-agent)
                                (hunchentoot:remote-addr*))
      (apply #'hunchentoot:redirect redirect-args))))
