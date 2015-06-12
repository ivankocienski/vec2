
(asdf:defsystem #:vec2
  :description "2D vector thing"
  :author "Your Name <your.name@example.com>"
  :license ""
  :serial t
  :depends-on (lispbuilder-sdl cl-opengl)
  :components ((:file "src/vec2")
	       (:file "src/package")
	       (:file "src/demo")))

