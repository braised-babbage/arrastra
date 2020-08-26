(asdf:defsystem #:arrastra
  :description "A build tool"
  :author "Erik Davis <erik@cadlag.org>"
  :license "MIT"
  :serial t
  :depends-on (#:uiop #:closer-mop #:alexandria)
  :pathname "src/"
  :components ((:file "package")
	       (:file "arrastra")))
