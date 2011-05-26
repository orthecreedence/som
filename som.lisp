(in-package :som)

(defclass som ()
  ((nodes :accessor som-nodes :initform (make-hash-table :test #'equal))
   (x :accessor som-x :initarg :x)
   (y :accessor som-y :initarg :y)
   (inputs :accessor som-inputs :initarg :inputs :initform nil)
   (num-iterations :accessor som-num-iterations :initarg :num-iterations)
   (iteration :accessor som-iteration :initform 0)
   (map-radius :accessor som-map-radius :initarg :map-radius)
   (time-const :accessor som-time-const :initarg :time-const)
   (search-radius :accessor som-search-radius)
   (learning-rate :accessor som-learning-rate :initarg :learning-rate)
   (paused :accessor som-paused :initform nil)
   (training :accessor som-training :initform t)))

(defun make-som (x y num-iterations inputs)
  "Make a som and create its child nodes."
  (let* ((map-radius (/ (max x y) 2))
         (inputs-length (length inputs))
         (som (make-instance 'som
                             :x x :y y
                             :inputs inputs
                             :num-iterations num-iterations
                             :map-radius map-radius
                             :time-const (/ num-iterations (log map-radius))
                             :learning-rate *const-start-learning-rate*)))
    (dotimes (y y)
      (dotimes (x x)
        (setf (gethash `(,x ,y) (som-nodes som)) 
              (make-node x y inputs-length))))
    som))

(defmethod find-best-node ((som som) input-vec)
  "Given an input vector, find the node that best matches it and return it."
  (let ((best-node nil)
        (inp-dist 999999))
    (loop for node being the hash-value of (som-nodes som) do
      (let ((dist (get-node-input-distance node input-vec)))
        (when (< dist inp-dist)
          (setf inp-dist dist)
          (setf best-node node))))
    best-node))

(defmethod get-som-node ((som som) x y)
  "Get the node at position x,y"
  (gethash `(,x ,y) (som-nodes som)))

(defmethod som-epoch ((som som))
  "Run a som epoch. Gets the best node for an input vector and adjusts other
   nodes according to that one using neighborhood/distance functions."
  (when (or (som-paused som)
            (not (som-training som)))
    (return-from som-epoch nil))
  (let* ((input-vec (nth (random (length (som-inputs som))) (som-inputs som)))
         (best-node (find-best-node som input-vec))
         (search-radius (* (som-map-radius som) (exp (- (/ (som-iteration som) (som-time-const som))))))
         (search-radius-sq (sq search-radius)))
    (loop for node being the hash-value of (som-nodes som) do
      (let ((dist-sq (+ (sq (- (node-x best-node) (node-x node)))
                        (sq (- (node-y best-node) (node-y node))))))
        (when (< dist-sq search-radius-sq)
          (let ((influence (exp (- (/ dist-sq (* search-radius-sq 2))))))
            (adjust-node-weights node input-vec (som-learning-rate som) influence)))))
    (setf (som-learning-rate som)
          (* (exp (- (/ (som-iteration som) (som-num-iterations som))))
             *const-start-learning-rate*)))
  (incf (som-iteration som))
  (when (<= (som-num-iterations som) (som-iteration som))
    (format t "Done training.~%")
    (setf (som-training som) nil)))


