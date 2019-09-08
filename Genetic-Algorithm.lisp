(defun float-vector-sum-setup ()
  "Does nothing.  Perhaps you might use this function to set
  (ahem) various global variables which define the problem being evaluated
  and the floating-point ranges involved, etc.  I dunno."

  (defparameter *float-vector-length* 20 
    "The length of the vector individuals")
  (defparameter *float-min* -5.12
    "The minimum legal value of a number in a vector") 
  (defparameter *float-max* 5.12 
    "The maximum legal value of a number in a vector")
  ;; I just made up these numbers, you'll probably need to tweak them
  (defparameter *crossover-probability* 0.1
    "Per-gene probability of crossover in uniform crossover")
  (defparameter *mutation-probability* 1.0
    "Per-gene probability of mutation in gaussian convolution") 
  (defparameter *mutation-variance* 0.02
    "Per-gene mutation variance in gaussian convolution")
)

(defmacro while (test &rest body)
  "Repeatedly executes body as long as test returns true.  Then returns nil."
  `(loop while ,test do (progn ,@body)))

(defun random? (&optional (prob 0.5))
  "Tosses a coin of prob probability of coming up heads,
then returns t if it's heads, else nil."
  (< (random 1.0) prob))

(defun generate-list (num function &optional no-duplicates)
  "Generates a list of size NUM, with each element created by
  (funcall FUNCTION).  If no-duplicates is t, then no duplicates
are permitted (FUNCTION is repeatedly called until a unique
new slot is created).  EQUALP is the default test used for duplicates."
  (let (bag)
    (while (< (length bag) num)
      (let ((candidate (funcall function)))
	(unless (and no-duplicates
		     (member candidate bag :test #'equalp))
	  (push candidate bag))))
    bag))


(defun gaussian-random (mean variance)
  "Generates a random number under a gaussian distribution with the
given mean and variance (using the Box-Muller-Marsaglia method)"
  (let (x y (w 0))
    (while (not (and (< 0 w) (< w 1)))
	   (setf x (- (random 2.0) 1.0))
	   (setf y (- (random 2.0) 1.0))
	   (setf w (+ (* x x) (* y y))))
    (+ mean (* x (sqrt variance) (sqrt (* -2 (/ (log w) w)))))))


(defparameter *tournament-size* 7)
(defun tournament-select-one (population fitnesses)
  "Does one tournament selection and returns the selected individual."

  (let ((l 0) (best 0) (i 2))
    (setf l (list-length population))
    (setf best (random l))
    (while (<= i *tournament-size*)
      (let ((next (random l)))
        (if (> (nth next fitnesses) (nth best fitnesses)) (setf best next)))
      (incf i))
    (nth best population)))



(defun tournament-selector (num population fitnesses)
  "Does NUM tournament selections, and puts them all in a list, then returns the list"

  (let ((temp-list '()))
    (dotimes (i num)
      (setf temp-list (nconc temp-list (list (tournament-select-one population fitnesses)))))temp-list))


(defun simple-printer (pop fitnesses)
  "Determines the individual in pop with the best (highest) fitness, then
  prints that fitness and individual in a pleasing manner."

  (let (best-ind best-fit)
    (mapcar #'(lambda (ind fit)
		(when (or (not best-ind)
			  (< best-fit fit))
		  (setq best-ind ind)
		  (setq best-fit fit))) pop fitnesses)
    (format t "~%Best Individual of Generation...~%Fitness: ~a~%Individual:~a~%"
	    best-fit best-ind)
    fitnesses))



(defun evolve (generations pop-size &key setup creator selector modifier evaluator printer)
  "Evolves for some number of GENERATIONS, creating a population of size
POP-SIZE, using various functions"

  (funcall setup)
  (let ((p '()) (best nil) (fitneses '()))
    (dotimes (i pop-size)
      (setf p (nconc p (list (funcall creator)))))
      (dotimes (k generations)
      (format t "~%Generation Number: ~a" (+ k 1))
        (setf fitneses '())
        (dotimes (j (list-length p))
          (setf fitneses (nconc fitneses (list (funcall evaluator (nth j p))))))
        (dotimes (i (list-length p))
          (if (or (equalp best nil) (> (nth i fitneses) (funcall evaluator best))) (setf best (nth i p))))
        (let ((q '()) (parent-a '()) (parent-b '()))
        (dotimes (i (/ pop-size 2))
          (setf parent-a (funcall selector 10 p fitneses))
          (setf parent-b (funcall selector 10 p fitneses))
          (setf q (nconc q (funcall modifier (car parent-a) (car parent-b)))))
        (setf p q))
        (funcall printer p fitneses))))



(defun float-vector-creator ()
  "Creates a floating-point-vector *float-vector-length* in size, filled with
UNIFORM random numbers in the range appropriate to the given problem"

  (let ((v '()))
    (dotimes (x *float-vector-length*)
      (setf v (nconc v (list (+ *float-min* (random (- *float-max* *float-min*)))))))v))


(defun uniform-crossover (ind1 ind2)
  "Performs uniform crossover on the two individuals, modifying them in place.
*crossover-probability* is the probability that any given allele will crossover.  
The individuals are guaranteed to be the same length.  Returns NIL."

  (let ((p *crossover-probability*))
    (dotimes (i (list-length ind1))
      (if (random? p) (rotatef (nth i ind1) (nth i ind2))))))


(defun gaussian-convolution (ind)
  "Performs gaussian convolution mutation on the individual, modifying it in place.
 Returns NIL."
  (let ((p *mutation-probability*))
    (dotimes (i (list-length ind))
      (if (random? p)
      (progn 
        (let ((n (gaussian-random 0 *mutation-variance*)))
          (loop 
            (setf n (gaussian-random 0 *mutation-variance*))
            (when (and (<= *float-min* (+ (nth i ind) n)) (<= (+ (nth i ind) n) *float-max*)) (return (+ (nth i ind) n))))
          (setf (nth i ind) (+ (nth i ind) n)))))))ind)

(defun float-vector-modifier (ind1 ind2)
  "Copies and modifies ind1 and ind2 by crossing them over with a uniform crossover,
  then mutates the children.  *crossover-probability* is the probability that any
  given allele will crossover.  *mutation-probability* is the probability that any
  given allele in a child will mutate.  Mutation does gaussian convolution on the allele."

  (let ((child1 '()) (child2 '()) (temp-list '()))
    (setf child1 (copy-list ind1))
    (setf child2 (copy-list ind2))
    (uniform-crossover child1 child2)
      (setf temp-list (nconc temp-list (list (gaussian-convolution child1) (gaussian-convolution child2))))temp-list))


(defun sum-f (ind)
  "Performs the Sum objective function.  Assumes that ind is a list of floats"
  (reduce #'+ ind))

(defun step-f (ind)
  "Performs the Step objective function.  Assumes that ind is a list of floats"
  (+ (* 6 (length ind))
     (reduce #'+ (mapcar #'floor ind))))

(defun sphere-f (ind)
  "Performs the Sphere objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* x x)) ind))))

(defun rosenbrock-f (ind)
  "Performs the Rosenbrock objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x x1)
			   (+ (* (- 1 x) (- 1 x))
			      (* 100 (- x1 (* x x)) (- x1 (* x x)))))
			 ind (rest ind)))))

(defun rastrigin-f (ind)
  "Performs the Rastrigin objective function.  Assumes that ind is a list of floats"
  (- (+ (* 10 (length ind))
	(reduce #'+ (mapcar (lambda (x) (- (* x x) (* 10 (cos (* 2 pi x)))))
			    ind)))))

(defun schwefel-f (ind)
  "Performs the Schwefel objective function.  Assumes that ind is a list of floats"
  (- (reduce #'+ (mapcar (lambda (x) (* (- x) (sin (sqrt (abs x)))))	
			 (mapcar (lambda (x) (* x 100)) ind)))))


(evolve 1000 50
 	:setup #'float-vector-sum-setup
	:creator #'float-vector-creator
	:selector #'tournament-selector
	:modifier #'float-vector-modifier
        :evaluator #'sum-f
	:printer #'simple-printer)


;;; Report:
;;; 
;;; This project is designed to implement the floating point vector genetic algorithm with different objective functions which calculate the fitenss of the individuals. 
;;; 
;;; Initial population is created with float-vector-creator function, which creates the vector of length specified by the varibale float-vector-length. The range of the floating point is decided by
;;; the varibles flaot-min and float-max. The floating value is alwayas a random value between these two varables.
;;; 
;;; I have used float-vector-sum-setup funtion to initilize the global varibales which are required throughout the code such as float-max, float-min etc.
;;; 
;;; In unifrom-crossover funtion I have used the rotatef function which swaps the values of two vectors in place so the there's no need to return the values of the new individuals.
;;; 
;;; While imlementing the code, I found out that closer the values of both float-min and float-max, better is the fitness of the best individual in the population. Also tournament-size has an effect
;;; on the fitness of the model as the tournament size increases, fitness of the model decreases gradually. To increase the fitness of the individuals, number of generations should be high, as the 
;;; model gets to explore more in the space to reach the global optima.
;;; 
;;; Results with different objective functions:
;;; 
;;; 	              sum-f	      step-f	      spehre-f	       rosenbrock-f	  rastrigin-f	  schwefel-f
;;; Generaions	    1000	      1000	        1000	           1000	          1000	        1000
;;; Pop-size	      50	        50	          50	             50	            50	          50
;;; Fitness Range	  98 to 100   206 to 209    -0.50 to -0.65   -65 to -90     -85 to -125   5000 to 6500


