;; $Header: ctak.cl,v 1.2 88/01/03 19:28:22 layer Exp $
;; $Locker:  $

;;; CTAK -- A version of the TAKeuchi function that uses the CATCH/THROW facility. 

(defun ctak (x y z)
  (catch 'ctak (ctak-aux x y z)))

(defun ctak-aux (x y z)
  ;;;(declare (fixnum x y z))
  (cond ((not (< y x))
	 (throw 'ctak z))
	(t (ctak-aux
	     (catch 'ctak
	       (ctak-aux (1- x)
			 y
			 z))
	     (catch 'ctak
	       (ctak-aux (1- y)
			 z
			 x))
	     (catch 'ctak
	       (ctak-aux (1- z)
			 x
			 y))))))

(print (ctak 18 12 6))
(print '(done ctak))
