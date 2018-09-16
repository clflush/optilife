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

;;; ??? I don't get how the edge bits ever get changed ???

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
    ;; reset the bounds, if necessary (see "wacko optimization")
    #|
    (unless (zerop (bit next i j))
      (when (< i (world-xmin world)) (setf (world-xmin world) i))
      (when (> i (world-xmax world)) (setf (world-xmax world) i))
      (when (< j (world-ymin world)) (setf (world-ymin world) j))
      (when (> j (world-ymax world)) (setf (world-ymax world) j)))
    |#
    ))

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
	 )
	 ;; The upper and lower bound nonsense is a wacko optimization that
	 ;; stops the thing from computing where there are no bits.
	 ;; Unfortunately, this completely screws any run with 0 birth, as
	 ;; well as the cell counter, so we've dyked it!
	 ;;          (xlb (max 1 (1- (world-xmin world))))
	 ;;          (xub (min (- lim 2) (1+ (world-xmax world))))
	 ;;          (ylb (max 1 (1- (world-ymin world))))
	 ;;          (yub (min (- lim 2) (1+ (world-ymax world)))))

    #|
    ;; Compute next world (wacko optimized version)
    (dotimes (i (1+ (- xub xlb)))
      (dotimes (j (1+ (- yub ylb)))
        (setnext world (+ i xlb) (+ j ylb))))
    |#

    ;; ??? I don't get how the edge bits ever get changed ???

    ;; Compute next world (simple version)
    (dotimes (i (- lim 2))
      (dotimes (j (- lim 2))
        (setnext world (1+ i) (1+ j))))
    ;; Copy back (because the count is screwed up, we recount while doing this!)
    (setf (world-numdots world) 0)
    (dotimes (y lim)
      (dotimes (x lim)
	(when (= 1 (bit next x y)) (incf (world-numdots world)))
        (setf (bit current x y) (bit next x y))))))

(defvar *w* nil)

(defun print-world (world generations &aux (k 0))
  (setq *w* world) ;; DDD
  (let ((lim (world-size world))
        (current (world-current world)))
    (dotimes (y lim)
      (dotimes (x lim)
        (if (zerop (bit current y x))
          (princ "  ")
	  (progn (incf k)
		 (princ "* "))))
      (terpri))
    (when *verbose?* (format t "~&~d Generations, ~d Organisms." generations (world-numdots world))))
  k)

(defun propagate (world cycles)
  (when *verbose?* (print-world world 0))
  (let ((r 
	 (loop for cycle below cycles
	       as count = (world-numdots world)
	       until (zerop count)
	       do 
	       (when *verbose?* (format t "Population ~a ... ~d generations" count cycle))
	       (next-cycle world)
	       (when *verbose?* (print-world world cycle))
	       collect count)))
    (if (< (length r) cycles) ;; Stopped on 0, push on on the end!
	(reverse (cons 0 (reverse r)))
      r)))

(defun life (source &key (cycles 25))
  (let* ((size (length (car source)))
	 (life (make-world
		:size size
		:current (make-array (list size size) :element-type 'bit
						      :initial-contents source)
		:next (make-array (list size size) :element-type 'bit
						   :initial-element 0)
		:numdots 0)))

    ;; This is a wacko optimization that only computes as far as there
    ;; are bits. Unfortunately, this screws up the cell count at the
    ;; margins. We do this here just to count cells.
    (dotimes (i size)
      (dotimes (j size)
	(unless (zerop (bit (world-current life) i j))
	  (incf (world-numdots life))
	  #|
	  (when (< i (world-xmin life)) (setf (world-xmin life) i))
	  (when (> i (world-xmax life)) (setf (world-xmax life) i))
	  (when (< j (world-ymin life)) (setf (world-ymin life) j))
	  (when (> j (world-ymax life)) (setf (world-ymax life) j))
	  |#
	  )))
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
	      (let ((result (life (random-pattern *size*) :cycles cycles)))
		(push `((:birth ,*birth* :survival ,*survival*)
		      ,result ,(categorize result))
		      *results*)))))

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
  (let ((*survival* (rule-vec '(0 0 1 1 0 0 0 0 0)))
	(*birth* (rule-vec '(0 0 0 1 0 0 0 0 0 0)))
	(*verbose?* t))
    (life test-glider :cycles cycles)))

; '(:BIRTH (1 2) :SURVIVAL (0 2 3))
(defun try-life (rules &key (*size* 20) (*verbose?* t) (cycles 25))
  (setf *birth* (getf rules :birth)
	*survival* (getf rules :survival))
  (life (random-pattern *size*) :cycles cycles))

;;; Analyzer sends the REVERSED dynamics map to the indicated function.

(defparameter *tags*
  `((:dead ,#'(lambda (l) (zerop (car l))))
    (:static ,#'(lambda (l) (and (> (length l) 4)
				 (= (first l) (second l)) (= (second l) (third l)))))
    (:flashing ,#'(lambda (l) (and (> (length l) 4)
				   (= (first l) (third l)) (= (second l) (fourth l))
				   (not (= (first l) (second l))))))))

(defun categorize (l)
  (let ((rl (reverse l)))
    (loop for (tag fn) in *tags*
	  when (funcall fn rl)
	  collect tag)))

(untrace)
(life-scan :cycles 25 :b/s-vec-len 2) ;; >10 runs the whole thing!
(mapcar #'pprint *results*)
;(test-life)
