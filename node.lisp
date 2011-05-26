(in-package :som)

(defclass node ()
  ((x :accessor node-x :initarg :x)
   (y :accessor node-y :initarg :y)
   (weights :accessor node-weights :initform nil)))

(defun make-node (x y num-weights)
  "Create a new node with the specified parameters and random weights."
  (let ((node (make-instance 'node :x x :y y)))
    (dotimes (i num-weights)
      (push (random 1.0) (node-weights node)))
    node))

(defmethod get-node-input-distance ((node node) input-vec)
  "Get the 'distance' between the given node and an input vector."
  (let* ((distance 0))
    (mapcar (lambda (input weight) (incf distance (sq (- input weight))))
            input-vec
            (node-weights node))
    (sqrt distance)))

(defmethod adjust-node-weights ((node node) input-vec learning-rate influence)
  "Adjust the weights in a node, correcting based on the given input vector and
   learning rate/influence values."
  (setf (node-weights node)
        (mapcar (lambda (input weight) (+ weight (* learning-rate influence (- input weight))))
                input-vec
                (node-weights node))))
