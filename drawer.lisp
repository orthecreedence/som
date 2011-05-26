(in-package :som)

(defun draw-som (som)
  (gl:clear :color-buffer :depth-buffer)
  (loop for node being the hash-value of (som-nodes som) do
    (let ((bottom-node (get-som-node som (node-x node) (- (node-y node) 1)))
          (left-node (get-som-node som (- (node-x node) 1) (node-y node))))
      (draw-node node bottom-node left-node)))
  (sdl:update-display))

(defun draw-node (node bottom-node left-node)
  (gl:push-matrix)
  (let* ((width (/ *config-graphics-window-x* 1));grid-x))
         (height (/ *config-graphics-window-y* 1));grid-y))
         (x (* (nth 0 (node-weights node)) width))
         (y (* (nth 1 (node-weights node)) height))
         (bottom-x (when bottom-node (* (nth 0 (node-weights bottom-node)) width)))
         (bottom-y (when bottom-node (* (nth 1 (node-weights bottom-node)) height)))
         (left-x (when left-node (* (nth 0 (node-weights left-node)) width)))
         (left-y (when left-node (* (nth 1 (node-weights left-node)) height))))
    (gl:translate x y 0)
    (gl:color .1 .1 .2)
    ;; draw box line to node on top
    (when bottom-node
      (gl:begin :lines)
      (gl:vertex 0 0)
      (gl:vertex (- bottom-x x) (- bottom-y y))
      (gl:end))
    ;; draw box line to node on left
    (when left-node
      (gl:begin :lines)
      (gl:vertex 0 0)
      (gl:vertex (- left-x x) (- left-y y))
      (gl:end))
    ;; draw current node
    (gl:color .3 .8 .3)
    (gl:begin :polygon)
    (gl:vertex -1 -1)
    (gl:vertex 1 -1)
    (gl:vertex 1 1)
    (gl:vertex -1 1)
    (gl:end))
  (gl:pop-matrix))
