(defparameter *custom-pathname* nil)

(defun main ()
  (with-ltk ()
    (let* ((lb-pathname (make-instance 'label
				       :text "Please enter a valid pathname leading to the directory where you save the project (eg: ~/Desktop/Prog3/PROJECT/)"))
	   (lb-input-warning (make-instance 'label
					    :text "WARNING : your input pathname must end with /"))
	   (entry-pathname (make-instance 'entry))
	   (bt-test (make-instance 'button :text "Submit path"))
	   (frame-main (make-instance 'frame))
	   (frame-sub (make-instance 'frame :master frame-main))
	   (lb-2048 (make-instance 'label
				   :master frame-sub
				   :text 2048))
	   (frame-grid (make-instance 'frame :master frame-main))
	   (canvas (make-instance 'canvas :master frame-grid))
	   (bt-new-game (make-instance 
			 'button
			 :master frame-sub
			 :text "New game"
			 :command (lambda ()
				    (init-grid)
				    (paint canvas)
				    (focus frame-grid)
				    )))
	   )
      
      (pack lb-pathname)
      (pack lb-input-warning)
      (pack entry-pathname)
      (pack bt-test)

      (bind bt-test "<ButtonPress>"
	    (lambda (evt)
	      (declare (ignore evt))
	      (setf *custom-pathname* (text entry-pathname))
	      (destroy entry-pathname)
	      (destroy bt-test)
	      (destroy lb-pathname)
	      (destroy lb-input-warning)
	      
	      ;; Pack the main frame
	      (pack frame-main)
	      
	      ;; Configuring the frame, button and label
	      (pack frame-sub :expand t :fill :x :side :top)
	      (pack bt-new-game :side :left :padx 30 :pady 20)
	      (pack lb-2048 :side :right :padx 30 :pady 20)
	      
	      ;; Configuring the canvas
	      (configure canvas :width 380 :height 380)
	      (configure frame-grid :takefocus t)
	      (focus frame-grid)
	      (pack frame-grid :after frame-sub :side :bottom)
	      (pack canvas)
	      
	      (init-grid)
	      (paint canvas)
	      
	      (bind frame-grid "<KeyPress>"
		    (lambda (evt)
		      (case (event-keycode evt)
			((111) (run-up))
			((113) (run-left))
			((114) (run-right))
			((116) (run-down)))
		      (paint canvas)
		      (when (check-if-2048)
					; in order to unfocus the frame-grid, so key press event doesn't work anymore
			(focus frame-main)
			(let ((win-msg (create-text canvas 160 0 "You win!")))
			  (itemconfigure canvas win-msg :font :tahoma)
			  ))
		      (unless (check-possibility)
			(focus frame-main)
			(let ((lose-msg (create-text canvas 160 0 "You lose!")))
			  (itemconfigure canvas lose-msg :font :tahoma)))
		      ))
	      ))
      )))

(defun paint (canvas)
  (clear canvas)

  (let ((rectangle (create-rectangle canvas 29 29 351 351)))
    ; Draw the rectangle
    (itemconfigure canvas rectangle :width 5)
    (itemconfigure canvas rectangle :outline :slategray)
    (itemconfigure canvas rectangle :fill :beige)
    
    ; Draw horizontal lines
    (create-line canvas (list 29 110 351 110))
    (create-line canvas (list 29 190 351 190))
    (create-line canvas (list 29 270 351 270))
    
    ; Draw vertical lines
    (create-line canvas (list 110 29 110 351))
    (create-line canvas (list 190 29 190 351))
    (create-line canvas (list 270 29 270 351))
    
    ; Add numbers
    (dotimes (i +NB-LINES+)
      (dotimes (j +NB-LINES+)
	(let ((image (make-image)))
	  (when (numberp (aref *array-numbers* i j))
	    (image-load image  
			(concatenate 'string 
				     *custom-pathname*
				     "img/number-"
				     (write-to-string (aref *array-numbers* i j)) ".png"))
	    (create-image canvas (+ 33 (* 80 i))  (+ 33 (* 80 j)) :image image)))))))

;;;
;;; ALGORITHME PART
;;;
(defun length-number (n)
  (if (null n)
      0
      (if (< n 10)
	  1
	  (1+ (length-number (floor n 10)) ))))

(defun cout (n)
  (let ((space (floor (- 6 (length-number n)) 2))
	(result NIL))
    (loop for i from 1 to space do
	 (setq result (concatenate 'string result " ")))
    (if (not (null n))
	(setq result (concatenate 'string result (write-to-string n))))
    (loop for i from 1 to (- 6 space (length-number n)) do
	 (setq result (concatenate 'string result " ")))
    result))

(defun print-grid ()
  "Print the grid with pretty graphical inteface"
  (dotimes (i (1+ (* +NB-LINES+ 2)))
    (dotimes (j  +NB-LINES+)
      (if (evenp i)
	  (if (= j (1- +NB-LINES+))
	      (format t "~A" '+------+)
	      (format t "~A" '+------))
	  (if (= j (1-  +NB-LINES+))
	      (format t "~A~A~A" '\| (cout (aref *array-numbers* (floor i 2) j))'\|)
	      (format t "~A~A" '\| (cout (aref *array-numbers* (floor i 2) j))))))
    (format t "~%")))

;;;
;;; Initialize all the needed parameters
;;;
(defun init-parameter ()
  "Initialize all the needed parameters"
  (defconstant +NB-LINES+ 4)
  (defparameter *array-numbers* 
    (make-array (list +NB-LINES+ +NB-LINES+) :initial-element nil)) 
  (defconstant +new-cell-values+ '(2 2 2 2 4)))
	
(defun random-values() 
  (let* ((value (nth (random (length +new-cell-values+)) 
		     +new-cell-values+))
         (empty-cells (list-empty-cells)) 
         (cell (nth (random (length empty-cells)) empty-cells))
         (x (car cell))
         (y (cdr cell)))
    (setf (aref *array-numbers* x y) value)))
	
(defun list-empty-cells ()
  (let ((cells nil))
    (dotimes (x +NB-LINES+)
      (dotimes (y +NB-LINES+)
        (when (null (aref *array-numbers* x y))
          (push (cons x y) cells))))
    cells))

;;;
;;; Initialize a grid
;;;
(defun init-grid ()
  "Initialize a 2 dimensional array (4x4) with 2 random numbes (2 or 4) at random position"
  (init-parameter)
  (random-values)
  (random-values))
  
(defun check-if-case-empty (x y)
  (null (aref *array-numbers* x y)))
  
(defun check-if-equal (x1 y1 x2 y2)
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (and (numberp value1) (numberp value2))
      (= value1 value2))))
 
(defun check-if-2048 ()
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (and (numberp (aref *array-numbers* x y)) (= (aref *array-numbers* x y) 2048))
	(return-from check-if-2048 t)))))

(defun can-move-right ()
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+)
      (when (or (check-if-equal x y (1+ x) y)
                (and (not (check-if-case-empty x y))
                     (check-if-case-empty (1+ x) y)))
        (return-from can-move-right t)))))

(defun can-move-left ()
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+) 
      (when (or (check-if-equal x y (1+ x) y)
                (and (check-if-case-empty x y)
                     (not (check-if-case-empty (1+ x) y))))
        (return-from can-move-left t)))))

(defun can-move-up ()
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y))
                (and (check-if-case-empty x y)
                     (not (check-if-case-empty x (1+ y)))))
        (return-from can-move-up t)))))

(defun can-move-down ()
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y))
                (and (not (check-if-case-empty x y))
                     (check-if-case-empty x (1+ y))))
        (return-from can-move-down t)))))
		
(defun swap-with-nil-case (x1 y1 x2 y2)
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (numberp value1)
      (setf (aref *array-numbers* x1 y1) nil)
      (setf (aref *array-numbers* x2 y2) value1))
    (when (numberp value2) 
      (setf (aref *array-numbers* x1 y1) value2)
      (setf (aref *array-numbers* x2 y2) nil))))
		
(defun combine-two-case (x1 y1 x2 y2)
  (let ((value1 (aref *array-numbers* x1 y1)))
    (setf (aref *array-numbers* x1 y1) (* 2 value1))
    (setf (aref *array-numbers* x2 y2) nil)))
	
(defun check-possibility ()
  (or (check-empty-case)
      (check-move)))

(defun check-empty-case ()
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (null (aref *array-numbers* x y))
        (return-from check-empty-case t)))))

(defun check-move ()
  (dotimes (x +NB-LINES+)
    (dotimes (y (1- +NB-LINES+))
      (when (check-if-equal x y x (1+ y))
        (return-from check-move t))))
  (dotimes (y +NB-LINES+)
    (dotimes (x (1- +NB-LINES+))
      (when (check-if-equal x y (1+ x) y)
        (return-from check-move t)))))
	
(defun move-right ()
  (dotimes (y +NB-LINES+)
    (do ((x (- +NB-LINES+ 2) (1- x)))
        ((minusp x))
      (when (not (check-if-case-empty x y))
        (let ((n x))	
          (loop
	     (cond ((= n (1- +NB-LINES+))
		    (return))
		   ((check-if-case-empty (1+ n) y)
		    (swap-with-nil-case (1+ n) y n y)
		    (incf n))
		   ((check-if-equal (1+ n) y n y)	
		    (combine-two-case (1+ n) y n y)
		    (return))
		   (t
		    (return)))))))))

(defun run-right ()
  (when (can-move-right)
    (move-right)
    (random-values)))

(defun move-left ()
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
		    (combine-two-case (1- n) y n y)
		    (return))
		   (t
		    (return)))))))))

(defun run-left ()
  (when (can-move-left)
    (move-left)
    (random-values)))

(defun move-down ()
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
		    (combine-two-case x (1+ n) x n)
		    (return))
		   (t
		    (return)))))))))

(defun run-down ()
  (when (can-move-down)
    (move-down)
    (random-values)))

(defun move-up ()
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
		    (combine-two-case x (1- n) x n )
		    (return))
		   (t
		    (return)))))))))

(defun run-up ()
  (when (can-move-up)
    (move-up)
    (random-values)))
