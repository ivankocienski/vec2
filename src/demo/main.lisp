
(in-package :vec2-demo)

(defparameter *v1* (vec2:new))
(defparameter *v2* (vec2:new-xy 100 200))
(defparameter *v3* (vec2:normal *v2*))

(defun demo ()
  "Entry point for demo"
  
  (format t "v2.x=~a~%" (vec2:x *v2*))

  (format t "v2.magnitude=~a~%" (vec2:magnitude *v2*))

  (setf (vec2:y *v1*) 10)

  (format t "v1.y=~a~%" (vec2:y *v1*)))
