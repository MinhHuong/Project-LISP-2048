(defun init-parameter ()
  "Initialize all the needed parameters"
  (defparameter entered-key nil)
  (defconstant +NB-LINES+ 4) 
  (defparameter *array-numbers*
    (make-array (list +NB-LINES+ +NB-LINES+) :initial-element nil))
  (defconstant +new-cell-values+  '(2 2 2 2 4)))
	
(defun random-values()
  (let* ((value (nth (random (length +new-cell-values+)) +new-cell-values+))
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

(defun init-grid ()
  "Initialize a 2 dimensional array (4x4) with 2 random numbes (2 or 4) at random position"
  (init-parameter)
  (random-values)
  (random-values))
  
(defun xet-o-trong (x y)
  (null (aref *array-numbers* x y)))
  
(defun xet-o-bang-nhau (x1 y1 x2 y2)
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (and (numberp value1) (numberp value2))
      (= value1 value2))))
  
(defun xet-dich-phai ()
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+) 
      (when (or (xet-o-bang-nhau x y (1+ x) y) 
                (and (not (xet-o-trong x y)) 
                     (xet-o-trong (1+ x) y)))
        (return-from xet-dich-phai t)))))

(defun xet-dich-trai ()
  (dotimes (x (1- +NB-LINES+))
    (dotimes (y +NB-LINES+) 
      (when (or (xet-o-bang-nhau x y (1+ x) y)
                (and (xet-o-trong x y) 
                     (not (xet-o-trong (1+ x) y))))
        (return-from xet-dich-trai t)))))

(defun xet-dich-len ()
  (dotimes (y (1- +NB-LINES+)) 
    (dotimes (x +NB-LINES+)
      (when (or (xet-o-bang-nhau x y x (1+ y)) 
                (and (xet-o-trong x y) 
                     (not (xet-o-trong x (1+ y)))))
        (return-from xet-dich-len t)))))

(defun xet-dich-xuong ()
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (xet-o-bang-nhau x y x (1+ y)) 
                (and (not (xet-o-trong x y)) 
                     (xet-o-trong x (1+ y))))
        (return-from xet-dich-xuong t)))))
		
(defun hoan-doi (x1 y1 x2 y2) 
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (numberp value1)
      (setf (aref *array-numbers* x1 y1) nil)
      (setf (aref *array-numbers* x2 y2) value1))
    (when (numberp value2)
      (setf (aref *array-numbers* x1 y1) value2)
      (setf (aref *array-numbers* x2 y2) nil))))
		
(defun ket-hop (x1 y1 x2 y2) 
  (let ((value1 (aref *array-numbers* x1 y1)))
    (setf (aref *array-numbers* x1 y1) (* 2 value1))
    (setf (aref *array-numbers* x2 y2) nil)))
	
(defun con-duong-di ()
  (or (con-o-trong)
      (kha-nang-di-chuyen)))

(defun con-o-trong () 
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (null (aref *array-numbers* x y))
        (return-from con-o-trong t)))))

(defun kha-nang-di-chuyen () 
  (dotimes (x +NB-LINES+)
    (dotimes (y (1- +NB-LINES+))
      (when (xet-o-bang-nhau x y x (1+ y))
        (return-from kha-nang-di-chuyen t))))
  (dotimes (y +NB-LINES+)
    (dotimes (x (1- +NB-LINES+))
      (when (xet-o-bang-nhau x y (1+ x) y)
        (return-from kha-nang-di-chuyen t)))))
	
(defun dich-phai () 
  (dotimes (y +NB-LINES+)
    (do ((x (- +NB-LINES+ 2) (1- x)));; for (int i = NB-LINES-2; i>0 ; i--) ;; phải chừa ra cột cuối cùng để dễ tính toán
        ((minusp x))
      (when (not (xet-o-trong x y)) ;; if ( cell[i][y] == NULL)
        (let ((n x))							;; int n = i;
          (loop								;;while (n<NB-LINES)
           (cond ((= n (1- +NB-LINES+)) ;; cond xài giống case
                  (return))
                 ((xet-o-trong (1+ n) y)	;;if(cell bên phải là cell trống)
                  (hoan-doi (1+ n) y n y)		;;ta hoán đổi và tăng biến n lên
                  (incf n))						;; vòng lặp sẽ chạy tiếp
                 ((xet-o-bang-nhau (1+ n) y n y)	;;if(cell bên phải có giá trị và bằng ô này thì)
                  (ket-hop (1+ n) y n y)		;; kết hợp lại
                  (return))						;; thoát khỏi vòng loop
                 (t
                  (return)))))))))
