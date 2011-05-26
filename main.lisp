(let ((packages '(cffi cl-opengl cl-glu lispbuilder-sdl bordeaux-threads)))
  (dolist (pkg packages)
    (ql:quickload pkg)))

(defpackage :som
	(:use :cl :cffi :bordeaux-threads))
(in-package :som)

(load (compile-file "config"))
(load (compile-file "window"))
(load (compile-file "drawer"))
(load (compile-file "node"))
(load (compile-file "som"))

(defun make-som-closure (som)
  "Given a som, return a self-contained object that can run operations on said
   som. This is useful for passing directly into the window handler so it can
   update the som without having direct access to the methods to do so."
  (let ((som som)
        (fn-table (make-hash-table :test #'equal)))
    (setf (gethash 'draw fn-table) (lambda () (draw-som som)))
    (setf (gethash 'quit fn-table) (lambda () (setf (som-training som) nil)))
    (setf (gethash 'pause fn-table)
          (lambda () (setf (som-paused som) (if (som-paused som) nil t))))
    (setf (gethash 'reset fn-table)
          (lambda () (setf som (make-som (som-x som) (som-y som)
                                         (som-num-iterations som)
                                         (som-inputs som)))))
    (lambda (fn &rest args)
      (multiple-value-bind (fn exists) (gethash fn fn-table)
        (if exists
            (apply fn args)
            (format t "sry, the method ~a doesn't exist in this closure~%" fn))))))

(defun run (inputs grid iterations)
  "Run the fuckin thing."
  (let* ((som (make-som (nth 0 grid) (nth 1 grid) iterations inputs))
         (draw-cl (make-som-closure som))
         (window-wrapper (lambda ()
                           (make-window *config-graphics-window-x*
                                        *config-graphics-window-y*
                                        "self organizing map test"
                                        draw-cl))))
    (make-thread window-wrapper :name 'somwindow)
    (loop while (som-training som) do
      (som-epoch som))))

(defparameter *grid* '(20 20))
(defparameter *inputs*
  (let ((grid nil))
    (dotimes (y (nth 1 *grid*))
      (dotimes (x (nth 0 *grid*))
        (setf grid (append grid `((,(/ x (nth 0 *grid*)) ,(/ y (nth 1 *grid*))))))))
    grid))
(run *inputs* *grid* 2000)
