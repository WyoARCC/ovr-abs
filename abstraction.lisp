(in-package #:ovr-abs)

(defparameter *running* nil)
(defparameter *entities* nil)

;;CEPL code starts here
(eval-when (:load-toplevel :compile-toplevel :execute)
    (defun triangle (vertices color normal)
      (let ((a (values (first vertices)))
	    (b (values (second vertices)))
	    (c (values (third vertices))))
	(list (list (v! a) nil nil)
	      ;; (list (v! b) (v! color) (v! normal))
	      ;; (list (v! c) (v! color) (v!  normal))
	      )
	))
  )

(defparameter *verts (list (list 1 0 0) (list -1 0 0) (list 0 1 0)))
(defparameter *color (list 0 1 0 1))
(defparameter *normal (list 0 0 1))

(defstruct-g pcn
  (vertex :vec3 :accessor vert)
  (color :vec4 :accessor col)
  (normal :vec3 :accessor norm))

(defun-g vertex ((vertex :vec3))
  vertex)

(defun-g color ((color :vec4))
  color)

(defun-g normal ((normal :vec3))
  normal)

(def-g-> render-prog ()
  #'vertex #'color #'normal)

(defclass entity ()
  ((e-stream :initform nil :initarg :e-stream :accessor e-stream)
   (position :initform (v! 0 0 -20) :initarg :pos :accessor pos)
   (rotation :initform (v! 0 0 0) :initarg :rot :accessor rot)
   (scale :initform (v! 1 1 1) :initarg :scale :accessor scale)))

(defun make-entity (&key pos e-stream)
  (make-instance 'entity :pos pos :e-stream e-stream))

(defun update-entity (entity)
  (let ((m2w (reduce #'m4:* (list (m4:translation (pos entity))
				  (m4:rotation-from-euler (rot entity))
				  (m4:scale (scale entity))))))
    (setf (rot entity) (v:+ (rot entity) (v! 0 0 0)))
    (map-g #'render-prog (e-stream entity))))

(defun init (data)
  (let* ((verts (make-gpu-array data
				:element-type 'pcn))
	 (e-stream (make-buffer-stream verts)))
    (setf *entities*
	  (mapcar (lambda (_) (make-entity :pos _ :e-stream e-stream))
		  (list (v! 0 0 0))))))

(defun step-demo ()
  ;; (step-host)
  ;; (update-repl-link)
  ;; (clear)				
  (map nil #'update-entity *entities*)	
  (swap))

;;CEPL code ends here

;;OVR code starts here

(defclass ovr-window ()
  ((window :accessor win :initarg window)
   (hmd :reader hmd :initarg :hmd)
   (world-vao :accessor world-vao)
   (count :accessor world-count)
   (hud-vbo :accessor hud-vbo :initform nil)
   (hud-vao :accessor hud-vao :initform nil)
   (hud-count :accessor hud-count)
   (hud-texture :accessor hud-texture)
   (font :accessor font)))

(defun start (render-data)
  (unwind-protect
       (%ovr::with-ovr ok (:debug nil :timeout-ms 500)
	 (unless ok
	   (format t "Libovr not initialized~%")
	   (return-from start nil))
	 (%ovr::with-hmd (hmd)
	   (unless hmd
	     (format t "HMD not opened~%Error: ~s~%" (%ovrhmd::get-last-error (cffi:null-pointer)))
	     (return-from start nil))
	   (let* ((hmd-info (%ovr::dump-hmd-to-plist hmd))
		  (w (getf (getf hmd-info :resolution) :w))
		  (h (getf (getf hmd-info :resolution) :h))
		  (x (aref (getf hmd-info :window-pos) 0))
		  (y (aref (getf hmd-info :window-pos) 1)))
	     (format t "Got HMD~%")
	     (%ovrhmd::set-enabled-caps hmd '(:low-persistence
					      :dynamic-prediction))
	     (format t "Caps enabled: ~s~%" (%ovrhmd::get-enabled-caps hmd))
	     (%ovrhmd::configure-tracking hmd
					  '(:orientation
					    :mag-yaw-correction
					    :position)
					  nil)
	     (format t "Tracking configured~%")
	     #+linux
	     (when (eq (getf hmd-info :type) :dk2)
	       (format t "Width: ~s~%Height: ~s" w h))
	     (format t "Attempting to open window with resolution ~sx~s at ~s, ~s~%" w h x y)
	     ;;might want to use make-viewport
	     (cepl::init w h "OVR with CEPL" t)
	     (let ((win (make-instance ovr-window)))
	       (setf (slot-value win 'window) cepl.glop::*window*
		     (slot-value win 'hmd) hmd)
	       (%ovr::with-configure-rendering eye-render-desc
		 (hmd
		  :back-buffer-size (list :w w :h h)
		  :distortion-caps '(:time-warp :vignette
				     :srgb :overdrive :hq-distortion
				     #+linux :linux-dev-fullscreen))
		 (format t "Configuring eye textures~%")
		 (let* ((ls (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-left+
							   (getf (elt eye-render-desc
								      %ovr::+eye-left+)
								 :fov)
							   1.0))
			(rs (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-right+
							   (getf (elt eye-render-desc
								      %ovr::+eye-right+)
								 :fov)
							   1.0))
			(padding 16)
			(fbo-w (+ (getf ls :w) (getf rs :w) (* 3 padding)))
			(fbo-h (+ (* 2 padding)
				  (max (getf ls :h) (getf rs :h))))
			(eye-textures
			 (loop for v in (list (list :pos (vector padding padding)
						    :size ls)
					      (list :pos (vector (+ (* 2 padding)
								    (getf ls :w))
								 padding)
						    :size rs))
			    collect
			      `(:render-viewport ,v
						 :texture-size (:2 ,fbo-w :h ,fbo-h)
						 :api
						 :opengl))))
		   (format t "Left size: ~s~%Right size: ~s~%Total size: ~sx~a~%"
			   ls rs fbo-w fbo-h)
		   (glop:set-gl-window (slot-value win 'window))
		   (init render-data)
		   (setf *running* t)
		   (loop :while (and *running* (not (shutting-down-p)))
		      :do (continuable (step-demo)))
		   (format t "Done with main loop, attempting cleanup~%")
		   (sleep 1)))))))))
;;OVR code ends here
