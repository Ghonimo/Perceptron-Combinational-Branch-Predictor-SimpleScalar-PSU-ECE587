(defun make-array-2d (list)
  (let ((ar (make-array (first list))))
        (do ((i 0 (1+ i)))
            (( = i (first list)))
            (setf (aref ar i) (make-array (second list))))
        ar))

(defun make-array-init (size elem)
  (let ((array (make-array size)))
    (array-init-elem array size elem)))

(defun make-array-list (size list)
  (let ((array (make-array size)))
    (array-init-list array size list)))

(defun array-init-elem (array size elem)
        (do ((i 0 (1+ i)))
            (( = i size))
            (setf (aref array i) elem))
        array)

(defun array-init-list (array size list)
        (do ((i 0 (1+ i)))
            (( = i size))
            (setf (aref array i) (nth i list)))
        array)

(defun proclaim (a)
  )

(defmacro format (a b) `(print ,b))

(defmacro declare (a) `t)

(defun time (a) `,a)

(defmacro the (a b) b)

(defmacro push (a b) `(progn
                        (setq ,b (cons ,a ,b))))

(defmacro unless (a b) `(if (not ,a) ,b))

(defmacro defvar (a b) `(setq ,a ,b))
(defmacro defconstant (a b) `(setq ,a ,b))

(defmacro aref2d (a i1 i2) `(aref (aref ,a ,i1) ,i2))

(defmacro first (a) `(car ,a))
(defmacro second (a) `(cadr ,a))

(defmacro svref (a) `(aref ,a))

(defun floor (a) (if (< a 0)
                         (if (eql a (truncate a))
                             a
                           (truncate (1- a)))
                   (truncate a)))

(defun floord (a b) (floor (/ a b)))

(defmacro floatd (a b) a)

(defun mexpt (a b)
  (let ((res a))
    (do ((i 1 (1+ i)))
        (( = i b))
        (setf res (* res a)))
    res))

(defmacro incf (a b)
        `(setf ,a (+ ,a ,b)))

(defun copy-tree (a)
    (cond 
        ((atom a) a)
        ( t (cons (copy-tree (car a)) (copy-tree (cdr a))))))

(defun schar (a b) 
    (char a b))

(defun gentemp () 
   (gensym))
