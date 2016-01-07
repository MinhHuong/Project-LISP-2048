;;;
;;; Main program
;;;
(defun main ()
  (with-ltk ()
    (let* ((frame-main (make-instance 'frame)) ;; Main frame that contains frame-sub and frame-grid
	   (frame-sub (make-instance 'frame :master frame-main)) ;; Frame that contains button NEW GAME and label 2048
	   (lb-2048 (make-instance 'label ;; Label 2048
				   :master frame-sub
				   :text 2048))
	   (frame-grid (make-instance 'frame :master frame-main)) ;; Frame that contains the grid of numbers
	   (canvas (make-instance 'canvas :master frame-grid)) ;; Canvas where we draw everything for the game
	   (bt-new-game (make-instance ;; Button that starts a new game
			 'button
			 :master frame-sub
			 :text "New game"
			 :command (lambda ()
				    (init-grid)
				    (paint canvas)
				    (focus frame-grid)
				    ))))
      ;; Set window's title
      (wm-title *tk* "LINF13 - 2048")

      ;; Pack the main frame
      (pack frame-main)
	      
      ;; Configuring the frame-sub, button and label
      (configure lb-2048 :font "Tahoma 27 bold")
      (configure lb-2048 :foreground "palevioletred4")
      (pack frame-sub :expand t :fill :x :side :top)
      (pack bt-new-game :side :left :padx 30 :pady 20)
      (pack lb-2048 :side :right :padx 30 :pady 20)
      
      ;; Configuring the frame-grid, canvas
      (configure canvas :width 380 :height 380)
      (configure frame-grid :takefocus t) ;; Make the frame-grid able to take focus
      (focus frame-grid) 
      ;; Focus on the frame-grid in order to read the event from Key Press
      ;; Without (focus frame-grid) the program won't read the key press event
      (pack frame-grid :after frame-sub :side :bottom)
      (pack canvas)
	      
      ;; Initialize the grid
      (init-grid)

      ;; Paint the whole canvas
      (paint canvas)
      
      ;; Bind event Key Press to frame-grid
      ;; so the program can read Key left / right / up / down
      (bind frame-grid "<KeyPress>"
	    (lambda (evt)
	      (case (event-keycode evt)
		((111) (run-up))    ;; Key up
		((113) (run-left))  ;; Key left
		((114) (run-right)) ;; Key right
		((116) (run-down))) ;; Key down
	      (paint canvas)
	      ;; Check if there's a case containing 2048 --> WIN the game
	      (when (check-if-2048)
		(focus frame-main) ;; set focus to other widget so the event keypress won't work anymore
		(let ((win-msg (create-text canvas 160 0 "You win!")))
		  (itemconfigure canvas win-msg :font :tahoma)
		  ))
	      ;; Check if there's possible move, if not --> LOSE the game
	      (unless (check-possibility) 
		(focus frame-main) ;; set focus to other widget so the event keypress won't work anymore
		(let ((lose-msg (create-text canvas 160 0 "You lose!")))
		  (itemconfigure canvas lose-msg :font :tahoma)))
	      ))
      )))


(defun paint (canvas)
  "Pain the canvas (the grid, cells, numbers)"
  ;; Clear everything first
  (clear canvas)

  ;; Paint the cells and its contained number
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      ;; each numbers is contained in a small rectangle cell
      (let ((cell (create-rectangle canvas 
				    (+ 30 (* 80 x))
					 (+ 30 (* 80 y))
					 (+ 30 (* 80 (1+ x)))
					 (+ 30 (* 80 (1+ y)))))
	    (value-cell (aref *array-numbers* x y))) ;; number in that cell
	(itemconfigure canvas cell :width 9)
	(itemconfigure canvas cell :outline "gray63")
	(if (numberp value-cell)
	    ;; if value-cell is a number, decorate cell and add number inside
	    (let* ((value-elements (gethash value-cell +cell-elements+)) ;; get the config details for that cell
		   (txt (create-text canvas 
				   (+ 30 (* 80 x) (getf value-elements :dx))
				   (+ 30 (* 80 y) (getf value-elements :dy))
				   (write-to-string value-cell))))
	      (itemconfigure canvas cell :fill (getf value-elements :bgcolor))
	      (itemconfigure canvas txt :font (getf value-elements :font-size))
	      (itemconfigure canvas txt :fill (getf value-elements :font-color)))
	    ;; if not, just color the cell gray, no text inside
	    (itemconfigure canvas cell :fill "gray77")))))
  )

;;;
;;; A hash table that indicates details to configuring the corresponding number
;;; eg : number 2 is contained within a cell with background-color Rosy, white text and font-size = 36...
;;;
(defconstant +cell-elements+
  (let ((table (make-hash-table)))
    (dolist (cell '((2 "rosybrown1" "white" "Tahoma 32 bold" 25 15)
		    (4 "darkgoldenrod1" "white" "Tahoma 32 bold" 25 15)
		    (8 "orangered2" "white" "Tahoma 32 bold" 25 15)
		    (16 "mediumpurple" "white" "Tahoma 32 bold" 11 16)
		    (32 "aquamarine4" "white" "Tahoma 32 bold" 11 16)
		    (64 "dark salmon" "white" "Tahoma 32 bold" 11 16)
		    (128 "chocolate1" "white" "Tahoma 26 bold" 4 20)
		    (256 "maroon3" "white" "Tahoma 26 bold" 4 20)
		    (512 "deepskyblue4" "white" "Tahoma 26 bold" 4 20)
		    (1024 "darkgrey" "white" "Tahoma 20 bold" 2 26)
		    (2048 "black" "white" "Tahoma 20 bold" 2 26)
		    ))
      ;; Key : the number 2 or 4 or 8 ...
      ;; Value : a list with this format (:BGCOLOR "rosybrown1" :FONT-COLOR "white" :FONT-SIZE ".." ...)
      ;; We extract the value from this list with (getf list :key)
      (setf (gethash (car cell) table)
	    (list :bgcolor (first (cdr cell))
		  :font-color (second (cdr cell))
		  :font-size (third (cdr cell))
		  :dx (fourth (cdr cell)) ;; x + dx --> to place number in the center of the cell
		  :dy (fifth (cdr cell))))) ;; y + dy --> to place number in the center of the cell
    table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;; 
;;;     ALGORITHME PART     ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-parameter ()
  "Initialize all the needed parameters"
  (defconstant +NB-LINES+ 4) ;; Size of the grid, number of lines = number of columns
  (defparameter *array-numbers* 
    (make-array (list +NB-LINES+ +NB-LINES+) :initial-element nil)) ;; Array that contains the numbers
  ;; This list is used to random a value in a randomly-chosen empty cell
  ;; It means the probability that the number 2 is picked is 80%, the number 4 is 20%
  (defconstant +new-cell-values+ '(2 2 2 2 4)))


(defun random-values() 
  "Random either number 2 or 4 in a randomly-chosen empty cell"
  (let* ((value (nth (random (length +new-cell-values+))
		     +new-cell-values+))
         (empty-cells (list-empty-cells))
         (cell (nth (random (length empty-cells)) empty-cells))
         (x (car cell))
         (y (cdr cell)))
    (setf (aref *array-numbers* x y) value)))

	
(defun list-empty-cells ()
  "Returns the list of empty cells : ( (1 . 1) (3 . 2) ... )"
  (let ((cells nil))
    (dotimes (x +NB-LINES+)
      (dotimes (y +NB-LINES+)
        (when (null (aref *array-numbers* x y))
          (push (cons x y) cells))))
    cells))


(defun init-grid ()
  "Initialize a 2 dimensional array (4x4) with 2 random numbes (2 or 4) at random position"
  (init-parameter)
  (random-values)
  (random-values))
  

(defun check-if-case-empty (x y)
  "Check if the case at postion (x,y) is empty (value = nil) or not"
  (null (aref *array-numbers* x y)))
  

(defun check-if-equal (x1 y1 x2 y2)
  "Check if the values in case (x1,y1) and (x2,y2) are equal or not"
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (and (numberp value1) (numberp value2))
      (= value1 value2))))
 

(defun check-if-2048 ()
  "Check if there's a case containing the number 2048"
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (and (numberp (aref *array-numbers* x y)) (= (aref *array-numbers* x y) 2048))
	(return-from check-if-2048 t)))))


(defun can-move-right ()
  "Verify if the whole grid is able to be moved to the right"
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+)
      (when (or (check-if-equal x y (1+ x) y) ;; if 2 consecutive cases have same value
                (and (not (check-if-case-empty x y))  ;; if (x,y) is not null
                     (check-if-case-empty (1+ x) y))) ;; and the case to the right of (x,y) is empty
        (return-from can-move-right t))))
  NIL)


(defun can-move-left ()
  "Verify if the whole grid is able to be moved to the left"
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+) 
      (when (or (check-if-equal x y (1+ x) y) ;; if 2 consecutive cases have same value
                (and (check-if-case-empty x y) ;; if (x,y) is empty
                     (not (check-if-case-empty (1+ x) y)))) ;; and the case to the right of (x,y) is not empty
        (return-from can-move-left t))))
  NIL)


(defun can-move-up ()
  "Verify if the whole grid is able to be moved up"
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y)) ;; if 2 consecutive cases have same value
                (and (check-if-case-empty x y) ;; if (x,y) is empty
                     (not (check-if-case-empty x (1+ y))))) ;; and the case below (x,y) is not empty
        (return-from can-move-up t))))
  NIL)


(defun can-move-down ()
  "Verify if the whole grid is able to be moved down"
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y)) ;; if 2 consecutive cases have same value
                (and (not (check-if-case-empty x y)) ;; if (x,y) is not empty
                     (check-if-case-empty x (1+ y)))) ;; and the case below (x,y) is empty
        (return-from can-move-down t))))
  NIL)


(defun swap-with-nil-case (x1 y1 x2 y2)
  "Swap an empty case with a not-empty case (one of the case MUST be empty and the other MUST NOT)"
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (numberp value1) ;; if case (x1,y1) is not empty
      (setf (aref *array-numbers* x1 y1) nil)
      (setf (aref *array-numbers* x2 y2) value1))
    (when (numberp value2) ;; or else if case (x2,y2) is not empty
      (setf (aref *array-numbers* x1 y1) value2)
      (setf (aref *array-numbers* x2 y2) nil))))


(defun combine-two-case (x1 y1 x2 y2)
  "Combine two cases that have the same value (both cases MUST NOT be empty). Combined value is saved in (x1,y1) ; (x2,y2) = NIL"
  (let ((value1 (aref *array-numbers* x1 y1)))
    (setf (aref *array-numbers* x1 y1) (* 2 value1))
    (setf (aref *array-numbers* x2 y2) nil)))


(defun check-possibility ()
  "Verify if there are any possible moves"
  (or (check-empty-case) ;; if there are empty cases
      (check-move))) ;; or if there are consecutive cases that have the same value


(defun check-empty-case ()
  "Search the grid to find if there are empty cases"
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (null (aref *array-numbers* x y))
        (return-from check-empty-case t)))))


(defun check-move ()
  "Iterate the grid to find if there are any consecutives cases that have the same values"
  (dotimes (x +NB-LINES+)
    (dotimes (y (1- +NB-LINES+))
      (when (check-if-equal x y x (1+ y))
        (return-from check-move t))))
  (dotimes (y +NB-LINES+)
    (dotimes (x (1- +NB-LINES+))
      (when (check-if-equal x y (1+ x) y)
        (return-from check-move t)))))


(defun move-right ()
  "Move the whole grid to the right"
  (let ((m nil)) ;; m serves to be the "locked case" for each line / column
    ;; so in the same line / column, numbers will not be appended all in once
    ;; eg  : 2 | 2 | 2 | 2 --> . | . | 4 | 4 (this is right) 
    ;; NOT : 2 | 2 | 2 | 2 --> . | . | . | 8 (this is wrong)
    (dotimes (y +NB-LINES+)
      (do ((x (- +NB-LINES+ 2) (1- x)))
	  ((minusp x))
	(when (not (check-if-case-empty x y))
	  (let ((n x))	
	    (loop
	       (cond 
		 ((= n (1- +NB-LINES+)) (return))
		 ((check-if-case-empty (1+ n) y)
		  (swap-with-nil-case (1+ n) y n y)
		  (incf n))
		 ((check-if-equal (1+ n) y n y)
		  (when (or (null m) (not(= (1+ n) m)))
		    (combine-two-case (1+ n) y n y)
		    (setq m (1+ n)))
		  (return))
		 (t (return)))
	       ))))
      (setq m nil))))


(defun run-right ()
  "Do 3 things : check if the grid is moveable to the right ; if it is, move it ; then random a number at a random position"
  (when (can-move-right)
    (move-right)
    (random-values)))

(defun move-left ()
  "Move the whole grid to the left"
  (let ((m nil))
    (dotimes (y +NB-LINES+)
      (do ((x 1 (1+ x)))
	  ((>= x +NB-LINES+))
	(when (not (check-if-case-empty x y))
	  (let ((n x))
	    (loop
	       (cond ((= n 0)
		      (return))
		     ((check-if-case-empty (1- n) y)
		      (swap-with-nil-case (1- n) y n y)
		      (decf n))
		     ((check-if-equal (1- n) y n y)
		      (when (or (null m) (not(= (1- n) m)))
			(combine-two-case (1- n) y n y)
			(setq m (1- n)))
		      (return))
		     (t
		      (return)))
	       ))))
      (setq m nil))))

(defun run-left ()
  "Do 3 things : check if the grid is moveable to the left ; if it is, move it ; then random a number at a random position"
  (when (can-move-left)
    (move-left)
    (random-values)))

(defun move-down ()
  "Move the whole grid down"
  (let ((m nil))
    (dotimes (x +NB-LINES+)
      (do ((y (- +NB-LINES+ 2) (1- y)))
	  ((minusp y))
	(when (not (check-if-case-empty x y))
	  (let ((n y))	
	    (loop
	       (cond ((= n (1- +NB-LINES+))
		      (return))
		     ((check-if-case-empty x (1+ n))
		      (swap-with-nil-case x (1+ n) x n)
		      (incf n))
		     ((check-if-equal x (1+ n) x n)
		      (when (or (null m) (not(= (1+ n) m)))
			(combine-two-case x (1+ n) x n)
			(setq m (1+ n)))
		      (return))
		     (t
		      (return)))
	       ))))
      (setq m nil))))

(defun run-down ()
  "Do 3 things : check if the grid is moveable below ; if it is, move it ; then random a number at a random position"
  (when (can-move-down)
    (move-down)
    (random-values)))

(defun move-up ()
  "Move the whole grid up"
  (let ((m nil))
    (dotimes (x +NB-LINES+)
      (do ((y 1 (1+ y)))
	  ((>= y +NB-LINES+))
	(when (not (check-if-case-empty x y))
	  (let ((n y))
	    (loop
	       (cond ((= n 0)
		      (return))
		     ((check-if-case-empty x (1- n))
		      (swap-with-nil-case x (1- n) x n)
		      (decf n))
		     ((check-if-equal x (1- n) x n)
		      (when (or (null m) (not (= (1- n) m)))
			(combine-two-case x (1- n) x n )
			(setq m (1- n)))
		      (return))
		     (t
		      (return)))
	       ))))
      (setq m nil))))

(defun run-up ()
  "Do 3 things : check if the grid is moveable above ; if it is, move it ; then random a number at a random position"
  (when (can-move-up)
    (move-up)
    (random-values)))
