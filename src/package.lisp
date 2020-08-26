(defpackage #:arrastra
  (:use #:cl #:uiop)
  (:export
   #:target
   #:target-value
   #:eval-target
   #:run-all-targets

   #:*output-root-directory*
   #:*target-output-directory*

   ;; utils
   #:sources))
