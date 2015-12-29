(defun move-down ()
  (do ((i (- +NB-LINES+ 2) (1- i))
       (move-steps 1 (1+ move-steps)))
      ((< i 0))
    (do ((k 1 (1+ k)))
	((> k move-steps))
      (do ((j 0 (1+ j)))
	  ((>= j +NB-COLUMNS+))
	(let ((below (aref array-numbers (+ i k) j))
	      (above (aref array-numbers i j)))
	  (unless (zerop above)
	    (if (zerop below)
		(progn
		  (setf (aref array-numbers (+ i k) j)
			above)
		  (setf (aref array-numbers i j) 
			0))
		(when (= above below)
		  (setf (aref array-numbers (+ i k) j) 
			(+ above below))
		  (setf (aref array-numbers i j) 0)))))))))

(defun print-grid ()
  (dotimes (i (* +NB-LINES+ 2))
    (dotimes (j +NB-COLUMNS+)
      (if (evenp i)
	  (if (= j (1- +NB-COLUMNS+))
	      (format t "~A" '+----+)
	      (format t "~A" '+----))
	  (if (= j (1- +NB-COLUMNS+))
	      (format t "~A    " '\|)
	      (format t "~A    ~A" '\| '\|))))
    (format t "~%")))
    
(defun play-game ()
  ;(init-grid)
  (set-grid)
  (print-numbers)
  
  (loop
     (setf entered-key (read))
     (when (equal entered-key 'q)
       (return))
     (when (equal entered-key 'd)
       (update-grid entered-key))
     (print-numbers)
     )
)

(defun update-grid (direction)
  "Move grid depending on the direction chosen by user. 4 directions possible: LEFT ('l), RIGHT ('r), UP ('u), DOWN ('d)"
  (cond 	 
    ((eq direction 'l) 
     (format t "LEFT"))
    ((eq direction 'r) 
     (format t "RIGHT"))
    ((eq direction 'u) 
     (format t "UP"))
    ((eq direction 'd) 
     (move-grid :line-begin (- +NB-LINES+ 2)))
    (t (format t "WRONG INPUT"))
  ))

;;; Move the whole grid
;;; Append appropriate cases
;;; Random one number at random-chosen empty case of the grid
;;; Parameters: (keyword)
;;;     + line-begin
;;; (generalize it also for up/right/left) !!!
(defun move-grid (&key line-begin)
  (do ((i line-begin (1- i))
       (move-steps 1 (1+ move-steps)))
      ((< i 0))
    (do ((k 1 (1+ k)))
	((> k move-steps))
      (print-numbers)
      (move-one-line (+ i (1- k))  (+ i k))))
  (random-empty-case)
  )))

;;; l1 : (x1 y1) coordinates of the to-append case
;;; l2 : (x2 y2) coordinates of the to-be-appended case
;;; 2 cases must be on same line or same column !!!
(defun append-2-cases (l1 l2)
  (let* ((x1 (car l1))
	 (y1 (cadr l1))
	 (x2 (car l2))
	 (y2 (cadr l2))
	 (case1 (aref array-numbers x1 y1))
	 (case2 (aref array-numbers x2 y2)))
    (setf (aref array-numbers x1 y2) 0)
    (if (= case1 case2)
	(setf (aref array-numbers x2 y2)
	      (+ case1 case2))
	(if (= x1 x2)
	    (setf (aref array-numbers x2 (1- y2))
		  case1)
	    (setf (aref array-numbers (1- x2) y2)
		  case1)))))

;;;
;;; Move one line down/left/up/right only
;;; A line and the ones right beside it (right/left/up/down)
;;; Something wrong here, try not to append them multiple times
;;; eg : x | 2 | 2 | 4 --> x | x | 4 | 4
;;; NOT: x | 2 | 2 | 4 --> x | x | x | 8
(defun move-one-line (line-moving line-standing)
  (do ((j 0 (1+ j)))
      ((>= j +NB-COLUMNS+))
    (let ((case-moving (aref array-numbers line-moving j))
	  (case-standing (aref array-numbers line-standing j)))
      (unless (zerop case-moving)
	(if (zerop case-standing)
	    (progn
	      (setf (aref array-numbers line-standing j)
		    case-moving)
	      (setf (aref array-numbers line-moving j) 
		    0))
	    (when (= case-moving case-standing)
	      (setf (aref array-numbers line-standing j) 
		    (+ case-moving case-standing))
	      (setf (aref array-numbers line-moving j) 0)))))))

;;;
;;; Direction ======
;;;  + 0 : same line
;;;  + 1 : same column
;;; Step ===========
;;;  + -1 : go left / go up
;;;  + +1 : go right / go down
;;; Origin =========
;;;  Point that runs to find the nearest not empty case
;;; Immobile =======
;;;  Position that doesn't move (same line/same column)
(defun nearest-case (direction step origin immobile)
  "Find the nearest case in the same line/column."
  (if (= direction 0)
      (loop
	 (if (and 
	      (zerop (aref array-numbers immobile origin))
	      (<= 0 origin +NB-COLUMNS+))
	     (setf origin (+ origin step))
	     (return (list immobile origin))))
      (if (>= (+ origin step) +NB-LINES+)
	  ;; check if overflow the array
	  (list)
	  (loop
	     (if (and
		  (zerop (aref array-numbers origin immobile))
		  (<= 0 origin +NB-LINES+))
		 (setf origin (+ origin step))
		 (return (list origin immobile))))))
  )
				  
(defun init-parameter ()
  "Initialize all the needed parameters"
  (defparameter entered-key nil)
  (defparameter +NB-LINES+ 4)
  (defparameter +NB-COLUMNS+ 4)
  (defparameter array-numbers 
    (make-array (list +NB-LINES+ +NB-COLUMNS+))))

(defun set-grid ()
  "Set a customized grid (just for test)"
  (init-parameter)
  (setf (aref array-numbers 0 2) 2)
  (setf (aref array-numbers 2 2) 2)
  (setf (aref array-numbers 3 2) 4)
  (setf (aref array-numbers 1 3) 2)
  (setf (aref array-numbers 3 3) 2))

(defun init-grid ()
  "Initialize a 2 dimensional array (4x4) with 2 random numbes (2 or 4) at random position"
  (init-parameter)
  (let ((times 2))
    (do ((i 0 (1+ i))
	 (x 0)
	 (y 0))
	((>= i times))
      (setf x (random 4))
      (setf y (random 4))
      ;; (random 2) = 0 --> 2, else 4
      (if (= (random 2) 0)
	  (setf (aref array-numbers x y) 2)
	  (setf (aref array-numbers x y) 4)))))

(defun print-numbers ()
  "Print the whole array of numbers (test)"
  (dotimes (i +NB-LINES+)
    (dotimes (j +NB-COLUMNS+)
      (format t " ~A " (aref array-numbers i j)))
    (format t "~%~%"))
  (format t "-----------~%"))

(defun random-empty-case ()
  "Random number 2 at a random-positioned empty case"
  (loop
       (let ((x (random 4))
	     (y (random 4)))
	 (when (zerop (aref array-numbers x y))
	   (setf (aref array-numbers x y) 2)
	   (return)))))
