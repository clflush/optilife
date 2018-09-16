; (load (compile-file "optilife.lisp"))
(declaim (optimize (debug 3)))
;;; Borrowed life code from: https://www.cs.cmu.edu/Groups/AI/lang/lisp/code/fun/life.cl

(defvar *verbose?* t)

(defstruct (world (:print-function 
                   (lambda (s d o)
                     (declare (ignore d))
                     (format s "#<WORLD: ~D>" (world-numdots world)))))
  size
  current
  numdots
  next
  (xmin 1000000)    ; Initialize the region to ridiculous numbers.
  (xmax -1)
  (ymin 1000000)
  (ymax -1))

(defparameter *birth* '(3))
(defparameter *survival* '(2 3))

(defun setnext (world i j)
  (let* ((current (world-current world))
         (next (world-next world))
         (neighbors (count-neighbors current i j)))
    ;; set the next bit pattern
    (if (zerop (bit current i j))
	;; current = 0
	(cond ((not (member neighbors *birth*))
	       (setf (bit next i j) 0))
	      (t (setf (bit next i j) 1)
		 (incf (world-numdots world))))
      ;; current = 1
      (cond ((member neighbors *survival*)
	     (setf (bit next i j) 1))
	    (t (setf (bit next i j) 0)
               (decf (world-numdots world)))))
    ;; reset the bounds, if necessary
    (unless (zerop (bit next i j))
      (when (< i (world-xmin world)) (setf (world-xmin world) i))
      (when (> i (world-xmax world)) (setf (world-xmax world) i))
      (when (< j (world-ymin world)) (setf (world-ymin world) j))
      (when (> j (world-ymax world)) (setf (world-ymax world) j)))))

(defun count-neighbors (array i j)
  (+ (bit array (1- i) (1- j))
     (bit array i      (1- j))
     (bit array (1+ i) (1- j))
     (bit array (1- i) j)
     (bit array (1+ i) j)
     (bit array (1- i) (1+ j))
     (bit array i      (1+ j))
     (bit array (1+ i) (1+ j))))

(defun next-cycle (world)
  (let* ((lim (world-size world))
         (current (world-current world))
         (next (world-next world))
         (xlb (max 1 (1- (world-xmin world))))
         (xub (min (- lim 2) (1+ (world-xmax world))))
         (ylb (max 1 (1- (world-ymin world))))
         (yub (min (- lim 2) (1+ (world-ymax world)))))
    (dotimes (i (1+ (- xub xlb)))
      (dotimes (j (1+ (- yub ylb)))
        (setnext world (+ i xlb) (+ j ylb))))
    (dotimes (y lim)
      (dotimes (x lim)
        (setf (bit current x y) (bit next x y))))))

(defun print-world (world generations)
  (let ((lim (world-size world))
        (current (world-current world)))
    (dotimes (y lim)
      (dotimes (x lim)
        (if (zerop (bit current y x))
          (princ "  ")
          (princ "* ")))
      (terpri))
    (when *verbose?* (format t "~&~d Generations, ~d Organisms." 
			     generations (world-numdots world)))))

(defun propagate (world cycles)
  (when *verbose?* (print-world world 0))
  (loop for cycle below cycles
	as count = (world-numdots world)
	until (zerop count)
	do 
	(when *verbose?* (format t "~2&POPULATION 0 ... ~d generations" cycle))
	(next-cycle world)
	(when *verbose?* (print-world world cycle))
	collect count))

(defun life (source &key (cycles 25))
  (let* ((size (length (car source)))
	 (life (make-world
		:size size
		:current (make-array (list size size) :element-type 'bit
						      :initial-contents source)
		:next (make-array (list size size) :element-type 'bit
						   :initial-element 0)
		:numdots 0)))
    (dotimes (i size)
      (dotimes (j size)
	(unless (zerop (bit (world-current life) i j))
	  (incf (world-numdots life))
	  (when (< i (world-xmin life)) (setf (world-xmin life) i))
	  (when (> i (world-xmax life)) (setf (world-xmax life) i))
	  (when (< j (world-ymin life)) (setf (world-ymin life) j))
	  (when (> j (world-ymax life)) (setf (world-ymax life) j)))))
    (propagate life cycles)))

;;; Rule scanner creates a random pattern and then simply runs for n
;;; cycles, returning a list of the number of living cells on each
;;; cycle (even if 0). The way that we figure out the birth and
;;; survival values is to create an 8-long trinary vector, and then
;;; parcel it out into the globals. Note that the vector is little
;;; endin for convenience of incrementing.

(defparameter *size* 20)

(defun incf-bvec (vec)
  (loop with carry = 0
	with first = 1
	as trit in vec
	as next = (+ carry trit first)
	do 
	(setf first 0)
	(if (= 2 next)
	     (setf next 0 carry 1)
	     (setf carry 0))
	collect next))

;;; In the rule-vec, 0 = death, 1=survival, 2=birth. These are done in globals for speed.

(defun rule-vec (vec)
  (loop for count from 0 by 1 
	as v in vec 
	when (= v 1)
	collect count))
  
(defvar *results* nil)

(defun life-scan (&key (cycles 25) (verbose? nil) (b/s-vec-len 3)) ;; Set limit to >10 for full run
  (setq *results* nil)
  (setq *verbose?* verbose? n 0)
  (loop with birth-vec = (list 0 0 0 0 0 0 0 0 0)
	as *birth* = (rule-vec birth-vec)
	until (> (length *birth*) b/s-vec-len)
	do (setf birth-vec (incf-bvec birth-vec))
	(loop with survival-vec = (list 0 0 0 0 0 0 0 0 0)
	      as *survival* = (rule-vec survival-vec)
	      until (> (length *survival*) b/s-vec-len)
	      do (setf survival-vec (incf-bvec survival-vec))
	      (format t "Running birth on ~a, survival on ~a~%" *birth* *survival*)
	      (push `((:birth ,*birth* :survival ,*survival*)
		      ,(life (random-pattern *size*) :cycles cycles))
		    *results*))))

(defun random-pattern (s)
  (loop for line below s
	collect (loop for col below s
		      collect (random 2))))

;;; Example:
(defvar test-glider 
     '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
       ))



(defun test-life (&key (cycles 50))
  ;Standard rules: (setq *birth* '(3) *survival* '(2 3))
  (set-global-birth/survival-from-rule-vecs '(0 0 0 1 0 0 0 0 0 0) '(0 0 1 1 0 0 0 0 0))
  (let ((*verbose?* t))
    (life test-glider :cycles cycles)))

; '(:BIRTH (1 2) :SURVIVAL (0 2 3))
(defun try-life (rules &key (*size* 20) (*verbose?* t) (cycles 25))
  (setf *birth* (getf rules :birth)
	*survival* (getf rules :survival))
  (life (random-pattern *size*) :cycles cycles))

(life-scan :cycles 50 :b/s-vec-len 3) ;; >10 runs the whole thing!
(mapcar #'pprint *results*)

;(test-life)
