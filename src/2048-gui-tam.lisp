(defun main ()
  (with-ltk ()
    (let* ((frame-main (make-instance 'frame))
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
				    (init-grid-test)
				    (paint canvas)
				    (focus frame-grid)
				    ))))
 
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
		((111) (run-up canvas))
		((113) (run-left canvas))
		((114) (run-right canvas))
		((116) (run-down canvas)))
	      (paint canvas)
	      (when (check-if-2048)
		(focus frame-main)
		(let ((win-msg (create-text canvas 160 0 "You win!")))
		  (itemconfigure canvas win-msg :font :tahoma)
		  ))
	      (unless (check-possibility)
		(focus frame-main)
			(let ((lose-msg (create-text canvas 160 0 "You lose!")))
			  (itemconfigure canvas lose-msg :font :tahoma)))
	      ))
      )))

(defun paint (canvas)
  (clear canvas)

  (let ((rectangle (create-rectangle canvas 29 29 357 357)))
    ; Draw the rectangle
    (itemconfigure canvas rectangle :width 5)
    (itemconfigure canvas rectangle :outline :slategray)
    (itemconfigure canvas rectangle :fill :beige)
    
    ; Draw horizontal lines
    ;; (create-line canvas (list 29 110 351 110))
    ;; (create-line canvas (list 29 190 351 190))
    ;; (create-line canvas (list 29 270 351 270))
    
    ;; ; Draw vertical lines
    ;; (create-line canvas (list 110 29 110 351))
    ;; (create-line canvas (list 190 29 190 351))
    ;; (create-line canvas (list 270 29 270 351))
    
    (drawing canvas)
    ))

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
      (setf (gethash (car cell) table)
	    (list :bgcolor (first (cdr cell))
		  :font-color (second (cdr cell))
		  :font-size (third (cdr cell))
		  :dx (fourth (cdr cell))
		  :dy (fifth (cdr cell)))))
    table))

(defun drawing (canvas)
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      ;; each numbers is contained in a small rectangle cell
      (let ((value-cell (aref *array-numbers* x y)))
	(when (numberp value-cell)
	  (let* ((value-elements (gethash value-cell +cell-elements+))
		 (cell (create-rectangle canvas 
					 (+ 30 (* 80 x) 5)
					 (+ 30 (* 80 y) 5)
					 (+ 30 (* 80 (1+ x)))
					 (+ 30 (* 80 (1+ y)))))
		 (txt (create-text canvas 
				   (+ 33 (* 80 x)  (getf value-elements :dx))
				   (+ 33 (* 80 y)  (getf value-elements :dy))
				   (write-to-string value-cell))))
	    (itemconfigure canvas cell :fill (getf value-elements :bgcolor))
	    (itemconfigure canvas txt :font (getf value-elements :font-size))
	    (itemconfigure canvas txt :fill (getf value-elements :font-color))
	    (itemconfigure canvas cell :outline (getf value-elements :bgcolor))
	     ))))))

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

(defun init-grid-test ()
  (setf (aref *array-numbers* 0 0) 2)
  (setf (aref *array-numbers* 1 0) 4)
  (setf (aref *array-numbers* 2 0) 8)
  (setf (aref *array-numbers* 3 0) 16)
  (setf (aref *array-numbers* 1 1) 32)
  (setf (aref *array-numbers* 2 1) 64)
  (setf (aref *array-numbers* 3 1) 128)
  (setf (aref *array-numbers* 1 2) 256)
  (setf (aref *array-numbers* 2 2) 512)
  (setf (aref *array-numbers* 3 2) 1024)
  (setf (aref *array-numbers* 3 3) 2048))

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
        (return-from can-move-right t))))
  NIL)

(defun can-move-left ()
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+) 
      (when (or (check-if-equal x y (1+ x) y)
                (and (check-if-case-empty x y)
                     (not (check-if-case-empty (1+ x) y))))
        (return-from can-move-left t))))
  NIL)

(defun can-move-up ()
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y))
                (and (check-if-case-empty x y)
                     (not (check-if-case-empty x (1+ y)))))
        (return-from can-move-up t))))
  NIL)

(defun can-move-down ()
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y))
                (and (not (check-if-case-empty x y))
                     (check-if-case-empty x (1+ y))))
        (return-from can-move-down t))))
  NIL)
		
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
	
(defun move-right (canvas)
  (let ((m nil))
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
	       ;(paint canvas)
	       ;(sleep 0.01)
	       ))))
      (setq m nil))))

(defun run-right (canvas)
  (when (can-move-right)
    (move-right canvas)
    (random-values)))

(defun move-left (canvas)
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
		 ;(paint canvas)
		 ;(sleep 0.01)
	       ))))
      (setq m nil))))

(defun run-left (canvas)
  (when (can-move-left)
    (move-left canvas)
    (random-values)))

(defun move-down (canvas)
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
	       ;(paint canvas)
	       ;(sleep 0.01)
	       ))))
      (setq m nil))))

(defun run-down (canvas)
  (when (can-move-down)
    (move-down canvas)
    (random-values)))

(defun move-up (canvas)
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
	       ;(paint canvas)
	       ;(sleep 0.01)
	       ))))
      (setq m nil))))

(defun run-up (canvas)
  (when (can-move-up)
    (move-up canvas)
    (random-values)))
