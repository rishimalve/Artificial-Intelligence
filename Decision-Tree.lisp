(setf *print-level* nil)
(setf *print-length* nil)
(setf *print-lines* nil)


(defparameter *restaurant-examples*
'(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time 0-10) (:label t))
((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
((alternative t) (has-bar nil) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 10-30) (:label t))
((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
((alternative nil) (has-bar t) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type italian) (estimated-wait-time 0-10) (:label t))
((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label nil))
((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
((alternative nil) (has-bar t) (open-friday t) (hungry nil) (patrons full) (price cheap) (raining t) (reservation nil) (type burger) (estimated-wait-time >60) (:label nil))
((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price expensive) (raining nil) (reservation t) (type italian) (estimated-wait-time 10-30) (:label nil))
((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t)))
"A list of examples for the Restaurant problem.
Examples take the form of ((attr1 val) (attr2 val) ... (:label label))")


(defparameter *restaurant-trial-examples*
'(((alternative t) (has-bar nil) (open-friday nil) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 30-60) (:label nil))
((alternative nil) (has-bar t) (open-friday nil) (hungry nil) (patrons some) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 0-10) (:label t))
((alternative t) (has-bar nil) (open-friday t) (hungry nil) (patrons full) (price expensive) (raining nil) (reservation t) (type french) (estimated-wait-time >60) (:label nil))
((alternative nil) (has-bar nil) (open-friday nil) (hungry t) (patrons some) (price medium) (raining t) (reservation t) (type thai) (estimated-wait-time 0-10) (:label t))
((alternative nil) (has-bar nil) (open-friday nil) (hungry nil) (patrons none) (price cheap) (raining nil) (reservation nil) (type thai) (estimated-wait-time 0-10) (:label nil))
((alternative t) (has-bar t) (open-friday t) (hungry t) (patrons full) (price cheap) (raining nil) (reservation nil) (type burger) (estimated-wait-time 30-60) (:label t))))


(defparameter *restaurant-feature-defs*
'((alternative t nil)
(has-bar t nil)
(open-friday t nil)
(hungry t nil)
(patrons none some full)
(price cheap medium expensive)
(raining t nil)
(reservation t nil)
(type french thai burger italian)
(estimated-wait-time 0-10 10-30 30-60 >60))
"A list of feature-defs for the Restaurant problem.
feature defs take the form (feature val1 val2 val3 ...)")


(defparameter *restaurant-labels*
'(t nil)
"label Values for the Restaurant problem")


(defun feature-val-for-example (example feature)
  "Returns the value of a given feature as stored in an example"
  (second (assoc feature example)))


(defun label-of-example (example)
  "Returns the label of an example, as stored in the example"
  (second (assoc :label example)))


(defun find-label-for-example (example decision-tree)
  "Given a decision tree and an example, uses the decision tree
    to determine the expected label for the example"
  (if (equalp (first decision-tree) :label)
      (second decision-tree) ;; return the label
    (find-label-for-example example
			    (second (assoc
				     (feature-val-for-example example (first decision-tree))
				     (rest decision-tree))))))

(defun examples-have-same-label-p (examples)
  "Returns t if all examples have the same label, else returns nil"
  (let ((label (label-of-example (first examples))))
    (dolist (example examples t)
      (when (not (equalp label (label-of-example example)))
	(return nil)))))


(defun num-examples-with-label (examples label)
  "Returns the number of examples with a given label"
  (count-if #'(lambda (example) (equalp 
				 (label-of-example example) label)) examples))
         

(defun most-common (elts &key (test #'equalp))
  "Returns the elt in the list elts that appears the most often;
     ties are broken arbitrarily, probably by picking the elt 
     which appeared earliest. Two elts are considered to be
     the same if they pass the :test (by default, equalp)"
  (let* ((unique-elts (remove-duplicates elts :test test))
	 (unique-nums (mapcar #'(lambda (x) (count x elts :test test)) unique-elts))
	 (best-elt (first unique-elts))
	 (best-num (first unique-nums)))
    (mapc #'(lambda (elt num) (when (> num best-num) 
				(setf best-elt elt) (setf best-num num)))
	  (rest unique-elts) (rest unique-nums))
    best-elt))


(defun normalize (numbers)
  "Normalizes a list of numbers by dividing them by their sum.  If
    the numbers are all 0.0, normalize just returns the original numbers."
  (let ((sum (apply #'+ numbers)))
    (if (= sum 0.0) numbers 
      (mapcar #'(lambda (num) (/ num sum)) numbers))))


(defun num-examples-with-feature-val (examples feature value)
  "Returns the number of examples with a given value for some feature"
  (count-if #'(lambda (example) (equalp
				 (feature-val-for-example example feature) value)) examples))


(defun examples-with-feature-val (examples feature val)
  "Returns the examples whose feature has value val"
  (remove-if-not #'(lambda (x) (equalp (feature-val-for-example x feature) val)) examples))


(defun unique (lsts &aux (h (make-hash-table :test 'equal)))
  (loop :for lst :in lsts
    :never (gethash lst h)
    :do (setf (gethash lst h) t)))


(defun most-common-label (examples)
  "Returns the most common label found in examples"
  (most-common (mapcar #'label-of-example examples)))

(defun label-probabilities-for-feature-val (examples feature value labels)
  "Returns a list, one item per label, 
    of the percentage of examples with a given value for a given feature,
    which belong to that label.  If they're all 0.0, you'll get back a list
    of 0.0's, even though that doesn't add to 1.0 (which should be just fine).
    If feature is nil, then this simply returns a list, one item per label, 
    of the percentage of examples which belong to that label."

  (let ((examples-with-feature-val
	 (if (null feature) examples 
	   (remove-if-not #'(lambda (x) (equalp 
					 (feature-val-for-example x feature) value)) examples))))
    (normalize
     (mapcar #'(lambda (label) (/ 
				(num-examples-with-label examples-with-feature-val label) 
				(length labels)))
	     labels))))

(defun information (probabilities)
"Given a list of probabilities, returns the information
theoretically stored in that list"

(let ((sum 0))
    (dolist (p probabilities)
      (if (equalp p 0)
        (setf sum (+ sum p)) (setf sum (+ sum (* -1 (* p (log p 2)))))))sum))


(defun remainder (feature-def examples labels)
"Returns the sum, over each value that the feature can take on,
of the probability of an example having that value, times the
information content of the probabilities of various labels
for all examples with that value."

(let ((final-rem 0) (feature-probabilities '()))
  (dolist (feature-value (cdr feature-def))
    (setf feature-probabilities (label-probabilities-for-feature-val examples (car feature-def) feature-value labels))
    (if (equalp (list-length examples) 0)
      (setf final-rem (+ final-rem 0))
      (setf final-rem (+ final-rem (* (information feature-probabilities) (/ (num-examples-with-feature-val examples (car feature-def) feature-value) (list-length examples)))))))final-rem))


(defun choose-feature (feature-defs examples labels)
"Returns the feature-def chosen from feature-defs
that best divides up examples. This is done by
picking the feature-def whose feature has
the lowest remainder given the examples and labels. Ties
for best are broken by picking the one considered earliest."

(let ((remainder-list '()) (min-feature '()) (minrem 1000) )
  (dolist (f feature-defs)
    (setf remainder-list (append remainder-list (list (list f (remainder f examples labels))))))
  (dolist (r remainder-list)
    (if (< (nth 1 r) minrem)
      (progn (setf minrem (nth 1 r)) (setf min-feature (nth 0 r)))))
      min-feature))


(let ((unique (gensym)))
(defun build-decision-tree (examples feature-defs labels &optional (default unique))
"Given a set of examples, a set of feature-defs, and a 
set of label values, returns a decision tree which correctly 
labelifies the examples. You can then run this decision tree
on examples to see how it does by passing it to the function
FIND-LABEL-FOR-EXAMPLE."

(if (equalp default unique)
(setf default (most-common-label examples)))

(let ((tree '()) (f (choose-feature feature-defs examples labels)))
(cond
    ;;; base conditions
    ((null examples)
    (list ':label default))
    ((examples-have-same-label-p examples)
    (list ':label default))
    ((null feature-defs)
    (list ':label default))
    (t
      (setf tree (nconc (list (car f)) tree))
      (dolist (v (cdr f))
        (setf tree (nconc tree (list (list v (build-decision-tree (examples-with-feature-val examples (car f) v) (remove f feature-defs) labels))))))
        tree)))))

(defparameter *t* '())
(setf *t* (build-decision-tree *restaurant-trial-examples* *restaurant-feature-defs* *restaurant-labels*))
(print "***final tree***")
(print *t*)
