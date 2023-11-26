;; $Header: tak.cl,v 1.2 88/01/03 19:28:39 layer Exp $
;; $Locker:  $


(defun tak (x y z)
  (declare (fixnum x y z))
  (cond ((not (< y x)) z)
	(t
	 (tak
	   (tak (the fixnum (1- x)) y z)
	   (tak (the fixnum (1- y)) z x)
	   (tak (the fixnum (1- z)) x y)))))

(defun testtak ()
  (print (time
	   (progn (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))
		  (print (tak 18 12 6))))))

(testtak)
(print '(done tak))