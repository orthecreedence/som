(in-package :som)

(defparameter *gl-window* nil)

(defun make-window (window-x window-y window-title draw-cl)
  (sdl:with-init ()
    ;; create the main window
    (setf *gl-window*
          (sdl:window window-x window-y
                      :title-caption window-title
                      :opengl t
                      :opengl-attributes '((:sdl-gl-doublebuffer 1)
                                           (:sdl-gl-red-size 8)
                                           (:sdl-gl-green-size 8)
                                           (:sdl-gl-blue-size 8)
                                           (:sdl-gl-depth-size 16))))
    ;; set up key repeat (so we can hold a key for rapid fire)
    (sdl:enable-key-repeat 200 30)
    ;; who knows - figure out what this does
	  (gl:enable :texture-2d :blend)
	  (gl:blend-func :src-alpha :one-minus-src-alpha)
    ;; set up 2D rendering in opengl
    ;; consider using the vector provided instead of coercing?
	  (let ((vport (coerce (gl:get-integer :viewport) 'list)))
	    (gl:matrix-mode :projection)
	    (gl:push-matrix)
	    (gl:load-identity)
	    (gl:ortho 0 (nth 2 vport) 0 (nth 3 vport) -1 1)   ; here's what does the actual 2D
	    (gl:matrix-mode :modelview)
	    (gl:push-matrix)
	    (gl:load-identity))
    ;; disable depth-testing (not needed in 2d)
	  (gl:disable :depth-test)
	  (gl:clear-color .04 .04 .04 0)
    ;; run the world...this calls our game loop
    (window-run draw-cl)))

(defmethod window-run (draw-cl)
  ;; poll our events
  (sdl:with-events (:poll)
	  (:quit-event () t)
	  (:video-expose-event () (sdl:update-display))
    ;; process keyboard input
	  (:key-down-event (:key key)
      (when (sdl:key= key :sdl-key-p)
        (funcall draw-cl 'pause))
      (when (sdl:key= key :sdl-key-r)
        (funcall draw-cl 'reset))
	  	(when (sdl:key= key :sdl-key-q)
        (funcall draw-cl 'quit)
	  	  (sdl:push-quit-event))
	  	(when (sdl:key= key :sdl-key-escape)
        (funcall draw-cl 'quit)
	  	  (sdl:push-quit-event)))
    ;; when not processing events, step the world
	  (:idle ()
      (funcall draw-cl 'draw))))
