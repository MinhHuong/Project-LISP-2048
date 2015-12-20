(defun print-grid ()
  "Print the grid with pretty graphical inteface"
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
    
;;;
;;; Main method
;;; Input key
;;;   + L : move left
;;;   + R : move right
;;;   + U : move up
;;;   + D : move down
;;;   + Q : quit game
;;;
(defun play-game ()
  (init-grid)
  (print-numbers)
  
  (loop
     (when (lose-game)
       (format t "~% YOU LOSE ! ~%")
       (return))
     (setf entered-key (read))
     (when (equal entered-key 'q) (return))
     (update-grid entered-key)
     (print-numbers)
     )
)

;;;
;;; Update the whole grid after receiving an input from user
;;; Input can be (symbol) 'l, 'r, 'u, 'd
;;; Use one only function for 4 directions
;;;                  MOVE-GRID (...)
;;; The behavior of the grid depends on all the parameters 
;;; that will be passed to the function move-grid
;;;
(defun update-grid (direction)
  "Move grid depending on the direction chosen by user. 4 directions possible: LEFT ('l), RIGHT ('r), UP ('u), DOWN ('d)"
  (cond 	 
    ;; LEFT
    ((eq direction 'l) 
     (move-grid
      :begin-outside 0
      :step-outside 1
      :limit-outside (lambda (k) (>= k +NB-LINES+))
      :begin-inside 1
      :step-inside 1
      :limit-inside (lambda (k) (>= k +NB-COLUMNS+))
      :border 0
      :direction 2))
    ;; RIGHT
    ((eq direction 'r) 
     (move-grid
      :begin-outside 0
      :step-outside 1
      :limit-outside (lambda (k) (>= k +NB-LINES+))
      :begin-inside (- +NB-COLUMNS+ 2)
      :step-inside -1
      :limit-inside (lambda (k) (< k 0))
      :border (1- +NB-COLUMNS+)
      :direction 3))
    ;; UP
    ((eq direction 'u) 
     (move-grid 
      :begin-outside 0
      :step-outside 1
      :limit-outside (lambda (k) (>= k +NB-COLUMNS+))
      :begin-inside 1
      :step-inside 1
      :limit-inside (lambda (k) (>= k +NB-LINES+))
      :border 0
      :direction 0))
    ;; DOWN
    ((eq direction 'd)
     (move-grid 
      :begin-outside 0
      :step-outside 1
      :limit-outside (lambda (k) (>= k +NB-COLUMNS+))
      :begin-inside (- +NB-LINES+ 2)
      :step-inside -1
      :limit-inside (lambda (k) (< k 0)) 
      :border (1- +NB-LINES+)
      :direction 1))
    (t (format t "WRONG INPUT"))
  ))

;;;
;;; Move the whole grid
;;; The behavior depends on all the passed parameters
;;; ALGORITHM :
;;; 1. Begin from line (or column)
;;; (depending on the moving direction)
;;; 2. Find the nearest case of the current case [i][j] 
;;; (or [j][i])
;;; 3. Append these 2 cases in appropriate way
;;;    3.1 If the nearest case is empty, 
;;;        move the case [i][j] (or [j][i]) to the border
;;;    3.2 Or else, append these 2 cases
;;; 4. Random one number at random-chosen empty case
;;;
(defun move-grid (&key 
		    begin-outside 
		    limit-outside 
		    step-outside 
		    begin-inside 
		    limit-inside 
		    step-inside 
		    border 
		    direction)
  "Move the grid in the appropriate way, append cases if necessary"
  (do ((i begin-outside (+ i step-outside)))
      ((funcall limit-outside i))
    (do ((j begin-inside (+ j step-inside)))
	((funcall limit-inside j))
      (let ((nearest-case (find-nearest-case 
			   :direction direction
			   :origin j
			   :immobile i)))
	(case direction
	  ;; Must divide to 2 cases here
	  ;; because the order to pass parameters to
	  ;; APPEND-2-CASES will differ
	  ;; depending on the moving direction

	  ;; 0 or 1 : UP or DOWN
	  ((0 1)
	   (if (endp nearest-case)
	       (append-2-cases (list j i)
			       (list border i))
	       (append-2-cases (list j i)
			       nearest-case)))
	  ;; 2 or 3 : LEFT or RIGHT
	  ((2 3)
	   (if (endp nearest-case)
	       (append-2-cases (list i j)
			       (list i border))
	       (append-2-cases (list i j)
			       nearest-case)))
	  (t (format t "~%Wrong direction~%")))))
    (setf has-been-appended NIL))
  (random-empty-case)
  )

;;;
;;; Append 2 cases passed by parameters
;;; case1 : to-append case
;;; case2 : to-be-appended case
;;; ATTENTION : on the same line/column, the "melting" process happens only once
;;; eg :  2 | . | 2 | 4     and we move LEFT
;;; --->  4 | 4 | . | .
;;; NOT : 8 | . | . | .
;;;
;;; ALGORITHM :
;;; 1. If 2 cases are not empty (= 0) :
;;;    1.1 If case1 = case2 :
;;;        1.1.1 If the same line/column hasn't had any "melting" action --> set case2 = (case1 + case2)
;;;        1.1.2. If not, stack case2 and case1 side by side
;;;    1.2 If case1 != case2 :
;;;        1.2.1 Stack them side by side in appropriate way
;;;        (code automatically recognize if those cases
;;;         are on same line or same column)
;;; 2. If case2 is empty, 
;;;    replace value on case2 by that of case1
;;;
(defun append-2-cases (l1 l2)
  "Append two cases, the 1st parameter is the to-append case, the 2nd parameter is the to-be-appended case. 2 cases MUST be on the same line or same column."
  (let* ((x1 (car l1))
	 (y1 (cadr l1))
	 (x2 (car l2))
	 (y2 (cadr l2))
	 (case1 (aref array-numbers x1 y1))
	 (case2 (aref array-numbers x2 y2)))
    ;; Erase value of the to-append case on the grid first
    (setf (aref array-numbers x1 y1) 0)
    (if (and (not (zerop case1)) (not (zerop case2)))
	(if (and (= case1 case2) (not has-been-appended))
	    ;; when 2 cases have the same value
	    ;; and in the same column/line 2 other cases
	    ;; haven't been appended
	    (progn
	      (setf (aref array-numbers x2 y2)
		    (+ case1 case2))
	      (setf has-been-appended T))
	    ;; when 2 cases have different value
	    (let ((step (append-2-cases-step x1 x2 y1 y2)))
	      (if (= x1 x2)
		  (setf (aref array-numbers 
			      x2 
			      (+ y2 step))
			case1)
		  (setf (aref array-numbers 
			      (+ x2 step)
			      y2)
			case1))))
	    (when (zerop case2)
	      (setf (aref array-numbers x2 y2) case1)))))

;;;
;;; Find the right step when 2 cases are to be appended side-by-side (values not equal)
;;; eg: append (0 0) in the left of (0 3) --> (0 2)
(defun append-2-cases-step (x1 x2 y1 y2)
  "Find the right 'sens' to stack cases side-by-side (position can differ depending on the moving direction L/R/U/D)"
  (if (= x1 x2)
      (if (> y1 y2)
	  1
	  -1)
      (if (> x1 x2)
	  1
	  -1)))

;;;
;;; Find nearest case depending on LEFT / RIGHT / UP / DOWN
;;; Direction:
;;;   0 : UP
;;;   1 : DOWN
;;;   2 : LEFT
;;;   3 : RIGHT
;;;
(defun find-nearest-case (&key direction origin immobile)
  "Find the nearest case. 3 arguments needed: direction (0 for UP, 1 for DOWN, 2 for LEFT, 3 for RIGHT) ; origin : the coordinate that will be moving during the whole searching for nearest case process ; immobile : the coordinate that will stand still."
  (case direction
    ;; UP
    ((0) 
     (find-nearest-case-aux 
      :running origin
      :standing immobile
      :sens -1
      :limit (lambda (k)
	       (>= k 0))
      :strategy 1))
    ;; DOWN
    ((1) 
     (find-nearest-case-aux 
      :running origin 
      :standing immobile 
      :sens 1 
      :limit (lambda (k)
	       (<= k (1- +NB-LINES+)))
      :strategy 1))
    ;; LEFT
    ((2)
     (find-nearest-case-aux 
      :running origin 
      :standing immobile 
      :sens -1
      :limit (lambda (k)
	       (>= k 0))
      :strategy 0))
    ;; RIGHT
    ((3)
     (find-nearest-case-aux 
      :running origin 
      :standing immobile 
      :sens 1
      :limit (lambda (k)
	       (<= k (1- +NB-COLUMNS+)))
      :strategy 0))
    (t (format t "Wrong direction"))))

;;;
;;; The main function that will find the nearest case
;;; running : coordinate that will be moving
;;; standing : coordinate that stand still
;;; sens : move --> or <--, up or down (1 or -1)
;;; limit : stop searching when the moving-point hits the border
;;; strategy :
;;;   + 0 : search on same line
;;;   + 1 : search on same column
;;; RETURN :
;;; (x y) --> coordinate of the nearest case
;;; ()    --> empty list if nearest case doesn't exist
;;;
(defun find-nearest-case-aux (&key running standing sens limit strategy)
  "The main function that will find the nearest case. Its behavior depends on all the passed arguments. Important : strategy = 0 : same line / = 1 : same column."
  (setf running (+ running sens))
  (loop
     ;; test if the moving coordinate overpasts the border
     (if (funcall limit running)
	 (let ((next-case 
		(if (= strategy 0)
		    (aref array-numbers standing running)
		    (aref array-numbers running standing))))
	   ;; if the next-case is empty (= 0)
	   (if (zerop next-case)
	       (setf running (+ running sens))
	       (if (= strategy 0)
		   (return (list standing running))
		   (return (list running standing)))))
	 (return (list)))))
	  
;;;
;;; Initialize all the needed parameters
;;;
(defun init-parameter ()
  "Initialize all the needed parameters"
  (defparameter entered-key nil)
  (defparameter +NB-LINES+ 4)
  (defparameter +NB-COLUMNS+ 4)
  (defparameter has-been-appended NIL)
  (defparameter array-numbers 
    (make-array (list +NB-LINES+ +NB-COLUMNS+))))

;;;
;;; Set a custom grid (in order to test)
;;;
(defun set-grid ()
  "Set a customized grid (just for test)"
  (init-parameter)
  (setf (aref array-numbers 0 0) 2)
  (setf (aref array-numbers 0 2) 4)
  (setf (aref array-numbers 2 2) 2)
  (setf (aref array-numbers 3 2) 2)
  (setf (aref array-numbers 1 3) 2)
  (setf (aref array-numbers 3 3) 2))

;;;
;;; Really init a grid
;;;
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
      ;; (random 2) = 0 --> 2, (random 2) = 1 --> 4
      (if (= (random 2) 0)
	  (setf (aref array-numbers x y) 2)
	  (setf (aref array-numbers x y) 4)))))

;;;
;;; Print just the numbers (simple version)
;;;
(defun print-numbers ()
  "Print the whole array of numbers (test)"
  (dotimes (i +NB-LINES+)
    (dotimes (j +NB-COLUMNS+)
      (if (zerop (aref array-numbers i j))
	  (format t " . ")
	  (format t " ~A " (aref array-numbers i j))))
    (format t "~%~%"))
  (format t "-----------~%"))

;;;
;;; Random a case of value 2 at random-positioned empty case
;;;
(defun random-empty-case ()
  "Random number 2 at a random-positioned empty case"
  (loop
       (let ((x (random 4))
	     (y (random 4)))
	 (when (zerop (aref array-numbers x y))
	   (setf (aref array-numbers x y) 2)
	   (return)))))

;;;
;;; Test if player loses the game or not
;;;
(defun lose-game ()
  "Test if the player loses the game or not"
  (let ((result T))
    (dotimes (i +NB-LINES+)
      (dotimes (j +NB-COLUMNS+)
	(when (= (aref array-numbers i j) 0)
	    (setf result NIL))))
    result))
