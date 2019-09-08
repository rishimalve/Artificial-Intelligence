(defparameter *neutral*
'((0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5) (0.1)))

(defparameter *voting-records-short*
'(((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.1 0.9 0.9 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.5 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.5 0.5) (0.1))
((0.9 0.9 0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.5 0.5 0.5 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.9 0.5 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.9 0.9 0.9 0.1 0.9 0.5 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.5 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.5 0.5 0.5 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.5 0.5) (0.1))
((0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.5 0.1 0.5) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.5 0.5 0.1 0.9 0.5 0.5 0.5 0.9 0.9) (0.9))
((0.5 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.5 0.5 0.5 0.5 0.5 0.5) (0.9))
((0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.5 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.5 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.5 0.9 0.5 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.9 0.1) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.1) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.5 0.5 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5 0.5 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.5 0.5 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.5 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.5 0.9 0.9) (0.9))
((0.1 0.1 0.5 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.5 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.5 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5) (0.9))
((0.5 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5 0.5 0.1 0.1 0.1 0.5 0.5) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.1 0.5 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.5 0.9 0.9 0.9 0.1 0.5 0.5 0.1 0.5 0.5 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9 0.5 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.5 0.5 0.5 0.5 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.5) (0.1))
((0.9 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.5 0.9 0.9 0.5 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.9 0.9 0.1 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.9 0.9 0.5 0.1 0.1 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.5 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.9 0.5 0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.5 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.9 0.9 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.5 0.1 0.1 0.1 0.1 0.9 0.5 0.1) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.5 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.5 0.9 0.1 0.5 0.5 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.5 0.9 0.9 0.5 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5) (0.1))
((0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.5 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.5 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.5 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.5 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.5 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.5 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.5 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1) (0.1))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.1 0.5) (0.1))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.5 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.5 0.1 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.5 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.5) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.5 0.9 0.1 0.5 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.9))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.5) (0.9))
((0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1) (0.1))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.1 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1) (0.9))
((0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.1) (0.9))
((0.1 0.5 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.9 0.1 0.1 0.1 0.1 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.5 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.5 0.5 0.1 0.1 0.5 0.9 0.5 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.5) (0.9))
((0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.1) (0.9))
((0.9 0.9 0.5 0.5 0.5 0.9 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.5 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.5 0.1 0.1 0.9 0.9) (0.9))
((0.9 0.9 0.1 0.1 0.9 0.5 0.1 0.1 0.1 0.1 0.9 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.1 0.9 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1) (0.9))
((0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1) (0.1))
((0.9 0.1 0.9 0.1 0.1 0.9 0.9 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.1 0.1 0.9 0.9 0.1 0.9) (0.1))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.9 0.9 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.1 0.9 0.9 0.1 0.1 0.1 0.9 0.5) (0.9))
((0.9 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.9 0.9) (0.9))
((0.1 0.1 0.9 0.1 0.1 0.1 0.9 0.9 0.9 0.9 0.1 0.1 0.1 0.1 0.1 0.9) (0.9))))

(defun difference (list1 list2 &key (measure 'count))
 (if (equalp measure 'count)
   (count 'NIL (mapcar #'equalp list1 list2))
   (if (equalp measure 'squared)
     (apply #'+ (mapcar #'* (mapcar #'- list1 list2) (mapcar #'- list1 list2)))
     (if (equalp measure 'manhattan)
       (apply #'+ (mapcar #'abs (mapcar #'- list1 list2)))))))

(defun compare-1st (a b)
  (< (nth 0 a) (nth 0 b)))

(defun most-common (list)
  (let ((reduced-elts (mapcar (lambda (elt) (list elt (count elt list)))
  (remove-duplicates list))))
  (first (first (sort reduced-elts #'> :key #'second)))))

(defun k-nearest-neighbor (examples new-example &key (k 1) (measure 'count))
 (setq lst '())
 (setq lst (loop for x in examples
  collect (list (difference (car x) (car new-example) :measure measure) (nth 1 x)) append lst))
 (setq lst1 '())
 (setq lst1 (sort lst 'compare-1st))
 (let ((i 0)) (setq lst2 '())
  (setq lst2 (loop for x in lst1 
    until (= i k)
    do(incf i) 
    collect (list (nth 1 x)) append lst2)))
 (most-common lst2))

(defun generalization (training-examples test-examples &key (k 1) (measure 'count))
 (let ((i 0))
  (dolist (x test-examples)
    (if (equalp (k-nearest-neighbor training-examples x :k k :measure measure) (cdr x)) () (incf i)))
  (- 100 (float (* (/ i (list-length test-examples)) 100)))))

(print (generalization (butlast *voting-records-short* 94) (last *voting-records-short* 94) :measure 'squared :k 3))
