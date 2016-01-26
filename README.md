# hunchentools

Hunchentoot utility library.

Hunchentools depends on
[Hunchentoot](http://www.weitz.de/hunchentoot/),
[Alexandria](https://common-lisp.net/project/alexandria/),
[CL-PPCRE](http://weitz.de/cl-ppcre/), and
[Ironclad](http://method-combination.net/lisp/ironclad/). Hunchentools
is being developed using [SBCL](http://sbcl.org/) on OS X and should
work on other implementations and platforms.

### Installation

Hunchentools is not yet available for download via
[quicklisp](https://www.quicklisp.org/). For now, clone the
repository, tell ASDF where to find the system definition, and load
the system with quicklisp:

```lisp
CL-USER> (ql:quickload "hunchentools")
```

### License

Hunchentools is distributed under the MIT license. See LICENSE.
