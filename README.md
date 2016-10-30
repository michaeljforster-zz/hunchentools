# hunchentools

Hunchentools is a utility library for the Hunchentoot web
server. Hunchentools provides functions for creating dispatchers,
aborting handlers, escaping strings, hardening session cookies,
managing session users, and managing session CSRF tokens.

Hunchentools depends on
[Hunchentoot](http://www.weitz.de/hunchentoot/),
[Alexandria](https://common-lisp.net/project/alexandria/),
[CL-PPCRE](http://weitz.de/cl-ppcre/),
and
[Ironclad](http://method-combination.net/lisp/ironclad/).
Hunchentools is being developed with
[SBCL](http://sbcl.org/), [CCL](http://ccl.clozure.com/),
and [LispWorks](http://www.lispworks.com/) on OS X.
Hunchentools is being deployed with SBCL on Linux/AMD64.

### Installation

```lisp
(ql:quickload "hunchentools")
```

### Example

```lisp
(defun render-login-page (message &optional (stream *standard-output*))
  (with-html-page (stream)
    (:div
     (:p (cl-who:esc message))
     (:form :action "/login" :method "post"
            (:input :name "username" :type "text" :value "")
            (:input :name "password" :type "password" :value "")
            (:input :type "submit" :value "Log In")))))

(hunchentoot:define-easy-handler (handle-login :uri "/login")
    ((username :parameter-type 'parse-username :request-type :post)
     (password :parameter-type 'parse-password :request-type :post))
  (hunchentoot:start-session)
  (hunchentools:harden-session-cookie)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (case (hunchentoot:request-method*)
    (:get
     (with-output-to-string (stream)
       (render-login-page "Login" stream)))
    (:post
     (if (or (null username)
             (null password)
             (string/= username "root")
             (string/= password "foobar"))
         (with-output-to-string (stream)
           (render-login-page "Bad username and/or password." stream))
         (progn
           (setf (hunchentools:session-user) username)
           (hunchentoot:redirect "/guess"))))))

(defun render-guess-page (csrf-token message
                          &optional (stream *standard-output*))
  (with-html-page (stream)
    (:div
     (:p (cl-who:esc message))
     (:form :action "/guess" :method "post"
            (:input :type "hidden" :name "csrf-token"
                    :value (hunchentools:escape-string-custom
                            csrf-token
                            (constantly t)
                            #'write-char))
            (:input :type "text" :name "guess" :value "")
            (:input :type "submit" :value "Scan"))
     (:p (:a :href "/logout" "Logout")))))

(hunchentoot:define-easy-handler (handle-guess :uri "/guess")
    ((guess :parameter-type 'parse-guess :request-type :post))
  (hunchentoot:start-session)
  (hunchentools:harden-session-cookie)
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (hunchentools:require-session-user "/logout")
  (case (hunchentoot:request-method*)
    (:get
     (with-output-to-string (stream)
       (render-guess-page (hunchentools:session-csrf-token)
                          "Guess a number."
                          stream)))
    (:post
     (hunchentools:require-session-csrf-token :post)
     (when (null guess)
       (hunchentools:abort-with-bad-request))
     (if (= guess 42)
         (hunchentoot:redirect "/guess")
         (with-output-to-string (stream)
           (render-guess-page (hunchentools:session-csrf-token)
                              "Nope. Guess again."
                              stream))))))

(defun handle-logout ()
  (hunchentoot:start-session)
  (hunchentools:harden-session-cookie)
  (hunchentools:delete-session-user)
  (hunchentoot:redirect "/login"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (push (hunchentools:create-uri-methods-dispatcher
          "/logout"
          :get
          'handle-logout)
        hunchentoot:*dispatch-table*))
```

### License

Hunchentools is distributed under the MIT license. See LICENSE.
