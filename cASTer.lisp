; cASTer.lisp
; 
; What is cASTer?
; cASTer is an implementation of a simple raycasting engine written in Common Lisp
; 
;
; Why cASTer? 
; 
; I shamelessly ripped off the naming convention used in another 
; (much more impressive) Common Lisp project: ASTeroids by AndyHD 
;

; To run this, you'll need SBCL with Quicklisp installed, as well as Lispbuilder SDL. Once you've got those
; you can load cASTer from the REPL with:
;
; (load "cASTer.lisp")
; (cASTer:main)

(sb-ext::set-floating-point-modes :traps nil)
(sb-ext:restrict-compiler-policy 'debug 3)

; load sdl  libraries
(eval-when (:compile-toplevel :execute)
  (ql:quickload :lispbuilder-sdl))
(defpackage :cASTer
  (:use :common-lisp)
  (:export main))

(in-package :cASTer)

; Globals
; ---------------------------------
; Define map dimensions
(defparameter *map-width* 48)
(defparameter *map-height* 48)
; Define game window size
(defparameter *window-width* 640)
(defparameter *window-height* 480)
(defparameter *wall-height*  *window-height* )
; Keep track of time
(defvar *old-time* 0.0)
(defvar *current-time* 0.0)
(defvar *frame-time* 0.0)

(defstruct cam
  ; x/y start position (middle of map)
  (pos-x (/ *map-width*  2.0d0) :type double-float)
  (pos-y (/ *map-height* 2.0d0) :type double-float)
  (slice-x               0.0d0  :type double-float)
  ; initial direction vector
  (dir-x         (- 1.0d0) :type double-float)
  (dir-y          0.0d0 :type double-float)
  (dir-x-old   0.0d0 :type double-float)
  ; define camera plane - These values together determine the FOV
  (plane-x        0.0d0 :type double-float) 
  (plane-y        0.66d0 :type double-float)
  (plane-x-old        0.0d0 :type double-float) 
  ; define move
  (mov-spd     0.0d0 :type double-float)
  (rot-spd 0.0d0 :type double-float))

(defstruct ray
  ; current x/y position 
  (pos-x       0.0d0 :type double-float)
  (pos-y       0.0d0 :type double-float)
  ; current square on the map
  (map-x       0 :type fixnum)
  (map-y       0 :type fixnum)
  ; current direction
  (dir-x       0.0d0 :type double-float)
  (dir-y       0.0d0 :type double-float)
  ; length of ray from current position to next x or y side
  (side-dist-x 0.0d0 :type double-float)
  (side-dist-y 0.0d0 :type double-float)
  ; which direction to step in x or y direction
  (step-x      0 :type fixnum)
  (step-y      0 :type fixnum)
  ; length of ray from one x or y side to next x or y side
  (delta-dist-x 0.0d0 :type double-float)
  (delta-dist-y 0.0d0 :type double-float)
  ; wall hit?
  (hit         0 :type fixnum)
  ; NS or EW wall hit? (to determine shading)
  (side        0 :type fixnum)
  )

; the camera
(defparameter *cam* (make-cam :mov-spd 0.0d0))
; the ray
(defparameter *ray* (make-ray))

; The Map! 1's are walls and 0's are empty space.

(defparameter *world-map* (make-array '(48 48) 
    :initial-contents 
    '((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 1)
      (1 0 0 1 1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 0 0 1)
      (1 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 1 1 1 1 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 0 1 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1)
      (1 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 0 0 1)
      (1 0 0 1 1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 1)
      (1 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 1 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 0 0 1)
      (1 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 1 1 1 1 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1)
      (1 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1)
      (1 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
      (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(defun square ( num ) (* num num))

(defun calc-ray-vector ( r c cur-slice ) 
  "Calculate the ray's vector when emitted from camera, set inital side distance."
  ; calculate ray position and direction
  ; (coerce x 'double-float)
  (setf (cam-slice-x c) (coerce 
                          ( - (/ (* 2 cur-slice) 
                                 (coerce *window-width* 'double-float)) 1) 'double-float))
  (setf (ray-pos-x r) (cam-pos-x c))                                            
  (setf (ray-pos-y r) (cam-pos-y c))                                            
  (setf (ray-dir-x r) (+ (cam-dir-x c) 
                         (* (cam-plane-x c)(cam-slice-x c)))) 
  (setf (ray-dir-y r) (+ (cam-dir-y c) 
                         (* (cam-plane-y c)(cam-slice-x c)))) 
  ; which box of the map we're in
  (setf (ray-map-x r) (round(ray-pos-x r)))                                        
  (setf (ray-map-y r) (round(ray-pos-y r))))

(defun calc-delta-x-y (r)
  "Calculate independent x y distances for ray."
  (setf (ray-delta-dist-x r)      
     (sqrt (+ 1 ( / (square (ray-dir-y r)) (square (ray-dir-x r))) ))) 
       
  (setf (ray-delta-dist-y r) 
     (sqrt (+ 1 ( / (square (ray-dir-x r)) (square (ray-dir-y r))) ))))

(defun calc-step-side (r)
  ""
  (progn
    (if(< (ray-dir-x r) 0 ) 
      (progn
        (setf (ray-step-x r) -1)
        (setf (ray-side-dist-x r) 
          (* (ray-delta-dist-x r) 
            (- (ray-pos-x r) (ray-map-x r)))))
      (progn
        (setf (ray-step-x r) 1)
        (setf (ray-side-dist-x r) 
          (* (ray-delta-dist-x r) 
            (- 
              (+ (ray-map-x r) 1.0) 
              (ray-pos-x r))))))
  (if (< (ray-dir-y r) 0 ) 
      (progn
        (setf (ray-step-y r) -1)
        (setf (ray-side-dist-y r) 
          (* 
            (ray-delta-dist-y r) 
              (- (ray-pos-y r) (ray-map-y r)))))
      (progn
        (setf (ray-step-y r) 1)
        (setf (ray-side-dist-y r) 
          (* (ray-delta-dist-y r) 
            (- 
              (+ (ray-map-y r) 1.0)
              (ray-pos-y r))))))))

(defun perform-dda-iter ( r ) 
  "Perform the DDA loop, accumulating independent x y distances in side-dist-y for each horizontal or vertical interval"
  (loop while (not (collidesp r)) 
      do 
        (if (< (ray-side-dist-x r) (ray-side-dist-y r) ) 
          (progn
            (incf (ray-side-dist-x r) (ray-delta-dist-x r))
            (incf  (ray-map-x r) (ray-step-x r))
            (setf (ray-side r) 0))
          (progn
            (incf (ray-side-dist-y r) (ray-delta-dist-y r))
            (incf  (ray-map-y r) (ray-step-y r))
            (setf (ray-side r) 1)))))
  
(defun collidesp (r) 
  "Return true if ray has intersected a wall"
    (> (aref *world-map* (ray-map-x r) (ray-map-y r)) 0))
  
(defun get-perp-wall-dist ( r ) 
  "Get the total distance from the ray origin to the wall intersection"
  (if (zerop (ray-side r))
        (/ ( - (ray-map-x r) (+ (ray-pos-x r) 
                               ( ash (1- (ray-step-x r) ) -1))) 
          (ray-dir-x r)) 
        
        (/ ( - (ray-map-y r) (+ (ray-pos-y r) 
                                ( ash (1- (ray-step-y r) ) -1))) 
           (ray-dir-y r))))
        
(defun get-vline-height (perp-wall-dist) 
  "Calculate height of line to draw on screen, using the distance from ray-origin to intersection"     
  (round( / *wall-height*  perp-wall-dist))) 

(defun draw-start (line-height) 
  "Calculate the lower y coordinate for the line start"              
  (progn
  (setq start (round(+ (/ (- line-height) 2 ) ( / *wall-height* 2))))
    (if (< start 0) 0 start)))

(defun draw-end (line-height)  
  "Calculate the upper y coordinate for the line end"             
  (progn
    (setq end (round (+ (/ line-height 2 )  ( / *wall-height* 2)))) 
      (if (>= end *wall-height*) (- *wall-height* 1) end)))

(defun update-frame-time () 
  "Calculate frame interval and set it."
  (setf *old-time* *current-time*)
  (setf *current-time* (sdl:sdl-get-ticks))
  (setf *frame-time* (/ (- *current-time* *old-time*) 1000.0)))

(defun update-movement-speed ( c )
  "Calculate movement speed by multiplying current frame time by a constant. This allows for more uniform movement regardless of current frame rate."
  (setf (cam-mov-spd c) (* *frame-time* 5.0d0))
  (setf (cam-rot-spd c) (* *frame-time* 3.0d0 )))

(defun draw-slice (r c x) 
  "Draw a single slice of the display's horizontal component."
    (calc-ray-vector r c x)
    (calc-delta-x-y r)
    (calc-step-side r)
    (perform-dda-iter r )
    (setq pwd  (get-perp-wall-dist r))
    (setq lh   (get-vline-height pwd))
    (if (= (ray-side r) 1) 
        (sdl:draw-vline x (draw-start lh) (draw-end lh)
        :color (sdl:color :r 0 :g 250 :b 0))
        
        (sdl:draw-vline x (draw-start lh) (draw-end lh)
       :color (sdl:color :r 0 :g 150 :b 0))))

(defun render-loop ()
  "Clear display,  draw the world, draw debug info, update the display."
    (sdl:clear-display sdl:*black*)
    
    ; Draw the floor in RetroTerminalGreen
    (sdl:draw-box-* 0 (/ *window-height* 2) *window-width* (/ *window-height* 2)
      :color (sdl:color :r 0 :g 50 :b 0))
    
    ; Draw the walls 
    (loop for i from 1 to *window-width*
      do (draw-slice *ray* *cam* i))
    
    (update-frame-time)
    (update-movement-speed *cam*)
    
    (sdl:draw-box-* 0 0 *window-width* 45
      :color (sdl:color :r 0 :g 0 :b 0))
    
    (sdl:draw-string-solid-* (format nil "x:~,4f y:~,4f fps:~,4f" (cam-pos-x *cam*) 
                                            (cam-pos-y *cam*) (/ 1.0 *frame-time*))
                                         5 5
                                        :color sdl:*green*)
    
    (sdl:draw-string-solid-* (format nil "dx:~,4f dy:~,4f px:~,4f py:~,4f" (cam-dir-x *cam*) 
                                            (cam-dir-y *cam*) (cam-plane-x *cam*)
                                            (cam-plane-y *cam*))
                                         5 25
                                        :color sdl:*green*)
    (sdl:update-display))


(defun move-forwards( c )
  "Independently increase camera x and y position, provided doing so would not place camera inside a wall."
    (progn (if (zerop (aref *world-map* 
                            (round( + (cam-pos-x c) (* (cam-dir-x c) (cam-mov-spd c)))) 
                            (round(cam-pos-y c))))     
               (incf (cam-pos-x c) 
                     (* (cam-dir-x c) (cam-mov-spd c))))
           (if (zerop (aref *world-map* 
                            (round( + (cam-pos-y c) (* (cam-dir-y c) (cam-mov-spd c))))
                            (round (cam-pos-x c))))     
               (incf (cam-pos-y c) 
                     (* (cam-dir-y c) (cam-mov-spd c))))))

(defun move-backwards( c )
    "Independently decrease camera x and y position, provided doing so would not place camera inside a wall."
    (progn 
      (if (zerop (aref *world-map* 
                       (round( - (cam-pos-x c) (* (cam-dir-x c) (cam-mov-spd c)))) 
                       (round (cam-pos-y c))))     
        (decf (cam-pos-x c) (* (cam-dir-x c) (cam-mov-spd c))))
      (if (zerop (aref *world-map* 
                       (round(cam-pos-x c)) 
                       (round ( - (cam-pos-y c) (* (cam-dir-y c) (cam-mov-spd c))))))     
        (decf (cam-pos-y c) (* (cam-dir-y c) (cam-mov-spd c))))))

(defun rotate-right ( c )
  (progn 
        (setf (cam-dir-x-old c) (cam-dir-x c))
        (setf (cam-dir-x c)  (-  (* (cam-dir-x c) ( cos(- (cam-rot-spd c))))
                                 (* (cam-dir-y c )(sin(- (cam-rot-spd c))))))
        (setf (cam-dir-y c)  (+  (* (cam-dir-x-old c) (sin(- (cam-rot-spd c))))
                                 (* (cam-dir-y c ) (cos(- (cam-rot-spd c))))))
        (setf (cam-plane-x-old c) (cam-plane-x c))
        (setf (cam-plane-x c) (- (* (cam-plane-x c) (cos(- (cam-rot-spd c))))
                                 (* (cam-plane-y c) (sin(- (cam-rot-spd c))))))
        (setf (cam-plane-y c) (+ (* (cam-plane-x-old c) (sin(- (cam-rot-spd c)))) 
                                 (* (cam-plane-y c) (cos(- (cam-rot-spd c))))))))

(defun rotate-left ( c )
  (progn 
        (setf (cam-dir-x-old c) (cam-dir-x c))
        (setf (cam-dir-x c)  (-  (* (cam-dir-x c) (cos(cam-rot-spd c)))
                                 (* (cam-dir-y c) (sin(cam-rot-spd c)))))
        (setf (cam-dir-y c)  (+  (* (cam-dir-x-old c) ( sin (cam-rot-spd c))) 
                                 (* (cam-dir-y c )(cos(cam-rot-spd c)))))
        (setf (cam-plane-x-old c) (cam-plane-x c))
        (setf (cam-plane-x c)  (-  (* (cam-plane-x c) (cos(cam-rot-spd c))) 
                                   (*(cam-plane-y c) (sin(cam-rot-spd c)))))
        (setf (cam-plane-y c)  (+  (* (cam-plane-x-old c) (sin(cam-rot-spd c))) 
                                   (*(cam-plane-y c) (cos(cam-rot-spd c)))))))

(defun interrupt-key-handler ()
  (when (sdl:key-down-p :sdl-key-escape)
    (sdl:push-quit-event)))

(defun idle-key-handler ()
    (when (sdl:key-down-p :sdl-key-w)
      (move-forwards *cam*))
    (when (sdl:key-down-p :sdl-key-s)
      (move-backwards *cam*))
    (when (sdl:key-down-p :sdl-key-a)
      (rotate-left *cam*))
    (when (sdl:key-down-p :sdl-key-d)
      (rotate-right *cam*)))

(defun main()
  (sdl:with-init ()
      (sdl:window *window-width* *window-height*
        :title-caption "cASTer"
        :icon-caption "cASTer")

      (sdl:initialise-default-font sdl:*font-9x18*)
      (setf (sdl:frame-rate) 60)
      (sdl:with-events ()
        (:key-down-event ()
           (interrupt-key-handler))
        (:idle ()
         (idle-key-handler)
         (render-loop)))))




