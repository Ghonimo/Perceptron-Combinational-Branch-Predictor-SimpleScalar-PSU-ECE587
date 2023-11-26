;; $Header: triang.cl,v 1.2 88/01/03 19:28:49 layer Exp $
;; $Locker:  $

;;; TRIANG -- Board game benchmark.

(proclaim '(special board sequence a b c))

(defun triang-setup ()
  (setq board (make-array-init 16 1))
  (setq sequence (make-array-init 14  0))
  (setq a
    (make-array-list
      37
      '(1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4 7 11 8 12
	13 6 10 15 9 14 13 13 14 15 9 10 6 6)))
  (setq b (make-array-list
	    37
	    '(2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5
	      2 4 7 5 8 9 3 6 10 5 9 8 12 13 14 8 9 5 5)))
  (setq c (make-array-list
	    37 
	    '(4 7 11 8 12 13 6 10 15 9 14 13 13 14 15 9 10 6
	      1 2 4 3 5 6 1 3 6 2 5 4 11 12 13 7 8 4 4)))
  (setf (aref board 5) 0))

(defun last-position ()
  (do ((i 1 (the fixnum (+ i 1))))
      ((= i 16) 0)
    (declare (fixnum i))
    (if (eq 1 (aref board i))
	(return i))))

(defun try (i depth)
  (declare (fixnum i depth))
  (cond ((= depth 14) 
	 (let ((lp (last-position)))
	   (unless (member lp final :test #'eq)
	     (push lp final)))
	 (push (cdr (simple-vector-to-list sequence))
	       answer) t) 		; this is a hack to replace LISTARRAY
	((and (eq 1 (aref board (aref a i)))
	      (eq 1 (aref board (aref b i)))
	      (eq 0 (aref board (aref c i))))
	 (setf (aref board (aref a i)) 0)
	 (setf (aref board (aref b i)) 0)
	 (setf (aref board (aref c i)) 1)
	 (setf (aref sequence depth) i)
	 (do ((j 0 (the fixnum (+ j 1)))
	      (depth (the fixnum (+ depth 1))))
	     ((or (= j 36)
		  (try j depth)) ())
	   (declare (fixnum j depth)))
	 (setf (aref board (aref a i)) 1) 
	 (setf (aref board (aref b i)) 1)
	 (setf (aref board (aref c i)) 0) ())))

(defun simple-vector-to-list (seq)
  (do ((i (- (length seq) 1) (1- i))
       (res))
      ((< i 0)
       res)
    (declare (fixnum i))
    (push (aref seq i) res)))
		
(defun gogogo (i)
  (let ((answer ())
	(final ()))
    (try i 1)))

(defun testtriang ()
        (print  (triang-setup))
  (print (time (gogogo 22))))
(testtriang)
(print '(done triang))
