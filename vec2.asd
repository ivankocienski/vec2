
(asdf:defsystem #:vec2
  :description "2D vector thing"
  :author "Your Name <your.name@example.com>"
  :license ""
  :serial t
  :components ((:file "src/vec2")))

(asdf:defsystem #:vec2-demo
  :description "Some demo code giving you a taste of vec2"
  :author "Your Name <your.name@example.com>"
  :license ""
  :serial t
  :depends-on (vec2)
  :components ((:file "src/demo/package")
	       (:file "src/demo/main")))

