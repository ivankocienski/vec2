
(in-package :cl-user)

(defpackage :vec2
  (:use :cl)
  (:export ;; ...
   :x
   :y
   :new
   :new-xy
   :copy
   :add-v
   :add-xy
   :add-v!
   :add-xy!
   :mul-v
   :mul-s
   :mul-xy
   :mul-v!
   :mul-s!
   :mul-xy!
   :dot
   :magnitude
   :normal
   :normalize!
   :perpendicular
   :==
   ))

(in-package :vec2)

(defparameter *comparison-threshold* 0.01)

(defstruct vec2 x y)

(defmacro x (v) `(vec2::vec2-x ,v))
(defmacro y (v) `(vec2::vec2-y ,v))

(defun new ()
  "Returns a new vec2 object"
  (make-vec2 :x 0 :y 0))

(defun new-xy (x y)
  "Returns new vec2 object with values filled"
  (make-vec2 :x x :y y))

(defun copy (v)
  "Copy existing vector"
  (make-vec2 :x (vec2-x v) :y (vec2-y v)))

(defun add-v (v1 v2)
  "Add the x and y components of two vectors together"
  (make-vec2
   :x (+ (vec2-x v1) (vec2-x v2))
   :y (+ (vec2-y v1) (vec2-y v2))))

(defun add-xy (v x y)
  "Add specified X and Y numbers to a new vec2"
  (make-vec2
   :x (+ (vec2-x v) x)
   :y (+ (vec2-y v) y)))

(defun add-v! (v1 v2)
  "Add vector v2 to v1, modifying and returning v1"
  (incf (vec2-x v1) (vec2-x v2))
  (incf (vec2-y v1) (vec2-y v2))
  v1)

(defun add-xy! (v x y)
  "Add X and Y numbers to V, modifying and returning V"
  (incf (vec2-x v1) x)
  (incf (vec2-y v1) y)
  v)

(defun mul-v (v1 v2)
  "Mulitiply the X and Y of V1 and V2 and return as a new vector"
  (make-vec2
   :x (* (vec2-x v1) (vec2-x v2))
   :y (* (vec2-y v1) (vec2-y v2))))

(defun mul-xy (v x y)
  "Multiply V X and Y by supplied X and Y returning as a new vector"
  (make-vec2
   :x (* (vec2-x v) x)
   :y (* (vec2-y v) y)))

(defun mul-s (v s)
  "Mulitply both X and Y components of V by S and return a new vector"
  (make-vec2
   :x (* (vec2-x v) s)
   :y (* (vec2-y v) s)))

(defun mul-v! (v1 v2)
  "Multiply the components of V1 and V1, modifying and returning V1"
  (setf (vec2-x v1) (* (vec2-x v1) (vec2-x v2)))
  (setf (vec2-y v1) (* (vec2-y v1) (vec2-y v2)))
  v1)

(defun mul-xy! (v x y)
  "Multiply the components of V by X and Y returning V"
  (setf (vec2-x v1) (* (vec2-x v) x))
  (setf (vec2-y v1) (* (vec2-y v) y))
  v)

(defun mul-s! (v s)
  "Multiply both X and Y of V by S and return V"
  (setf (vec2-x v1) (* (vec2-x v) s))
  (setf (vec2-y v1) (* (vec2-y v) s))
  v)
  
(defun invert (v)
  "Copy and return an  inverted V"
  (make-vec2 :x (- (vec2-x v)) :y (- (vec2-y v))))

(defun invert! (v)
  "Modify and return V to be its inverse"
  (setf (vec2-x v) (- (vec2-x v)))
  (setf (vec2-y v) (- (vec2-y v)))
  v)

(defun dot (v1 v2)
  "Dot product of V1 and V2"
  (+
   (* (vec2-x v1) (vec2-x v2))
   (* (vec2-y v1) (vec2-y v1))))

(defun magnitude (v)
  "Scalar magnitude of V"
  (sqrt (dot v v)))

(defun normal (v)
  "Return a new vector that is the normalized version of V"
  (let ((d (magnitude v)))
    (make-vec2
     :x (/ (vec2-x v) d)
     :y (/ (vec2-y v) d))))

(defun normalize! (v)
  "Normalize and return V"
  (let ((d (magnitude v)))
    (setf (vec2-x v) (/ (vec2-x v) d))
    (setf (vec2-y v) (/ (vec2-y v) d))
    v))

(defun perpendicular (v)
  "Return a vector that is the tangent of the input vector"
  (make-vector
   :x (- (vec2-y v))
   :y (vec2-x v)))
    
(defun == (v1 v2)
  "Threshold based comparison of two vectors"
  (and
   (< (abs (- (vec2-x v1) (vec2-x v2))) *comparison-threshold*)
   (< (abs (- (vec2-y v1) (vec2-y v2))) *comparison-threshold*)))
