;;;; ovr-abs.asd

(asdf:defsystem #:ovr-abs
  :description "An abstraction of OVR using CEPL"
  :author "Graham Marousek <graham.marousek@gmail.com>"
  :license "BSD 2 clause"
  :depends-on (#:cepl.glop
	       #:3b-ovr
               #:temporal-functions
               #:swank
               #:livesupport
               #:skitter
               #:cepl.devil
#:rtg-math)
  :serial t
  :components ((:file "package-abs")
               (:file "abstraction")))

