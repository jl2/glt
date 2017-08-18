;;;; glt.asd
;;;;
;;;; Copyright (c) 2017 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:glt
  :description "Describe glt here"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:cl-glfw3
               #:cl-opengl
               #:trivial-main-thread
               #:stl)
  :serial t
  :components ((:file "package")
               (:file "glt")))

