(in-package :som)

;; define an "infinity" value
(defparameter *infinity-f* most-positive-single-float)
(defparameter *neg-infinity-f* most-negative-single-float)

;; graphics config
(defparameter *config-graphics-window-x* 500)
(defparameter *config-graphics-window-y* 500)

(setf *random-state* (make-random-state t))

;; network config
(defparameter *const-start-learning-rate* 0.25)

(defun sq (x)
  "Square a value"
  (* x x))

(defmacro toggle-bool (value)
  "Toggle a boolean value in a setf-able variable/location."
  `(setf ,value (if ,value nil t)))
