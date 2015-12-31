 ;;Ham in ra de test cua tam
 ;;Ham tinh do dai cua so
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
;;; Thiết lập những hằng số, biến toàn cục ban đầu
;;;
(defun init-parameter ()
  "Initialize all the needed parameters"
  (defparameter entered-key nil)
  (defconstant +NB-LINES+ 4)
  (defparameter +locked-case+ (list))
  (defparameter *array-numbers* 
    (make-array (list +NB-LINES+ +NB-LINES+) :initial-element nil)) 
  (defconstant +new-cell-values+  '(2 2 2 2 4))) ;;; mới thêm vào để cho 1 hàm mới random. Dùng theo kiểu list là random khả năng 75% là số 2, 25% sẽ là số 4
	
(defun random-values() 
  (let* ((value (nth (random (length +new-cell-values+)) 
		     +new-cell-values+))
         (empty-cells (list-empty-cells)) 
         (cell (nth (random (length empty-cells)) empty-cells))
         (x (car cell))
         (y (cdr cell)))
    (setf (aref *array-numbers* x y) value)))
	
(defun list-empty-cells () ;; danh sách những ô còn trống chưa có giá trị để tiện cho việc random thêm số vào trong array
  (let ((cells nil))
    (dotimes (x +NB-LINES+)
      (dotimes (y +NB-LINES+)
        (when (null (aref *array-numbers* x y))
          (push (cons x y) cells))))
    cells))
;;;
;;; Really init a grid
;;; Tạo khung chơi ban đầu
;;;
(defun init-grid ()
  "Initialize a 2 dimensional array (4x4) with 2 random numbes (2 or 4) at random position"
  (init-parameter) ;; chạy hàm init-parameter trước để thiết lập các biến, hằng đầu chương trình
  (random-values)
  (random-values))
  
;;;những hàm xử lý của em dài quá, phải phân ra thành từng hàm nhỏ vậy mới tốt hơn, dễ xử lý, tính toán
(defun check-if-case-empty (x y) ;; hàm xét ô có tọa độ này có mang giá trị NIL hay không
  (null (aref *array-numbers* x y))) ;; ô này có NIL hay không đây
  
(defun check-if-equal (x1 y1 x2 y2) ;; hàm xét 2 ô có bằng nhau hay không
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (and (numberp value1) (numberp value2)) ;; trước tiên phải kiểm tra coi mọi giá trị có phải là số hay không, vì có thể là NIL
      (= value1 value2)))) ;; trả về true nếu 2 ô bằng nhau
  
(defun can-move-right () ;; hàm xem coi có thể dịch sang phải hay không?
  (dotimes (x (1- +NB-LINES+)) ;; trừ đi 1 vì phải chừa ô cuối cùng, ô cuối cùng bên phải không còn ô nào để so sánh
    (dotimes (y +NB-LINES+) 
      (when (or (check-if-equal x y (1+ x) y) ;;khi có 2 ô cạnh nhau mà bằng nhau
                (and (not (check-if-case-empty x y)) ;; hoặc ô đang xét không NIL và ô kế sau là NIL thì ta có thể dịch sang phải
                     (check-if-case-empty (1+ x) y)))
        (return-from can-move-right t)))))

(defun can-move-left ()
  (dotimes (x (1- +NB-LINES+)) ;; trừ đi 1 vì phải chừa ô cuối cùng, ô cuối cùng bên phải không còn ô nào để so sánh
    (dotimes (y +NB-LINES+) 
      (when (or (check-if-equal x y (1+ x) y) ;;khi có 2 ô cạnh nhau mà bằng nhau
                (and (check-if-case-empty x y) ;; hoặc ô đang xét NIL và ô kế sau là kô NIL thì ta có thể dịch sang trái
                     (not (check-if-case-empty (1+ x) y))))
        (return-from can-move-left t)))))

(defun can-move-up ()
  (dotimes (y (1- +NB-LINES+)) ;; đảo ngược y và x ở đây vì lúc này xét theo trục khác là trục đứng tahy vì trục ngang
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y)) ;;khi có 2 ô sát nhau trên dưới mà bằng nhau
                (and (check-if-case-empty x y) ;; hoặc khi ô đang xét là NIL và ô nằm ngay dưới nó không NIL thì có thể dịch lên trên
                     (not (check-if-case-empty x (1+ y)))))
        (return-from can-move-up t)))))

(defun can-move-down () ;; hàm xét coi có bước nào dịch xuống được hay không
  (dotimes (y (1- +NB-LINES+))
    (dotimes (x +NB-LINES+)
      (when (or (check-if-equal x y x (1+ y)) ;;khi có 2 ô cạnh nhau mà bằng nhau
                (and (not (check-if-case-empty x y)) ;; hoặc khi ô đang xét là ko NIL và ô nằm ngay dưới nó  NIL thì có thể dịch lên xuống
                     (check-if-case-empty x (1+ y))))
        (return-from can-move-down t)))))
		
(defun swap-with-nil-case (x1 y1 x2 y2) ;;không rõ lắm, chưa test,(theo phân tích thì khi hoán đổi, thì 1 trong 2 ô se NIL, nên coi ô nào có giá trị, thì chuyển đổi qua lại))
  (let ((value1 (aref *array-numbers* x1 y1))
        (value2 (aref *array-numbers* x2 y2)))
    (when (numberp value1)
		(setf (aref *array-numbers* x1 y1) nil)
		(setf (aref *array-numbers* x2 y2) value1))
    (when (numberp value2) ;; đôi khi có thể bỏ điều kiện này, vì nếu suy nghĩ kỹ thuật toán, thì chỉ có xét quan trọng là cell 1
		(setf (aref *array-numbers* x1 y1) value2)
		(setf (aref *array-numbers* x2 y2) nil))))
		
(defun combine-two-case (x1 y1 x2 y2) ;; theo kết hợp thì đơn giản cho cell 1 giá trị *2, ô cell 2 thì NIL
  (let ((value1 (aref *array-numbers* x1 y1)))
    (setf (aref *array-numbers* x1 y1) (* 2 value1))
    (setf (aref *array-numbers* x2 y2) nil)))
	
(defun check-possibility () ;;xét xem bàn cờ còn có thể di chuyển được không
  (or (check-empty-case)
      (check-move)))

(defun check-empty-case () ;; nếu bàn cờ vẫn còn ô trống thì break ra ngay và trả về true
  (dotimes (x +NB-LINES+)
    (dotimes (y +NB-LINES+)
      (when (null (aref *array-numbers* x y))
        (return-from check-empty-case t)))))

(defun check-move () ;; nếu bàn cờ vẫn có thể di chuyển trái phải, lên xuống thì trả về true ngay
  (dotimes (x +NB-LINES+)
    (dotimes (y (1- +NB-LINES+))
      (when (check-if-equal x y x (1+ y))
        (return-from check-move t))))
  (dotimes (y +NB-LINES+)
    (dotimes (x (1- +NB-LINES+))
      (when (check-if-equal x y (1+ x) y)
        (return-from check-move t)))))
	
(defun move-right () ;; dịch sang phải các ô
  (dotimes (y +NB-LINES+)
    (do ((x (- +NB-LINES+ 2) (1- x)));; for (int i = NB-LINES-2; i>0 ; i--) ;; phải chừa ra cột cuối cùng để dễ tính toán
        ((minusp x))
      (when (not (check-if-case-empty x y)) ;; if ( cell[i][y] == NULL)
        (let ((n x))	;; int n = i;
          (loop	;while (n<NB-LINES)
	     (cond ((= n (1- +NB-LINES+)) ;; cond xài giống case
		    (return))
		   ((check-if-case-empty (1+ n) y)
		    ;;if(cell bên phải là cell trống)
		    (swap-with-nil-case (1+ n) y n y)		
		    ;;ta hoán đổi và tăng biến n lên
		    (incf n))						
		   ;; vòng lặp sẽ chạy tiếp
		   ((check-if-equal (1+ n) y n y)	
		    ;;if(cell bên phải có giá trị và bằng ô này thì)
		    (combine-two-case (1+ n) y n y)		;; kết hợp lại
		    (return))						
		   ;; thoát khỏi vòng loop
		   (t
		    (return)))))))))


(defun move-left () ;; dịch sang trai  các ô
  (dotimes (y +NB-LINES+)
    (do ((x 1 (1+ x)));; for (int i = 1; i<NB-LINES ; i++) ;; phải chừa ra cột dau tien để dễ tính toán
        ((>= x +NB-LINES+))
      (when (not (check-if-case-empty x y)) ;; if ( cell[i][y] == NULL)
        (let ((n x))	;; int n = i;
          (loop	;while (n>0)
	     (cond ((= n 0) ;; cond xài giống case
		    (return))
		   ((check-if-case-empty (1- n) y)
		    ;;if(cell bên trai là cell trống)
		    (swap-with-nil-case (1- n) y n y)		
		    ;;ta hoán đổi và tăng biến n lên
		    (decf n))						
		   ;; vòng lặp sẽ chạy tiếp
		   ((check-if-equal (1- n) y n y)	
		    ;;if(cell bên trai có giá trị và bằng ô này thì)
		    (combine-two-case (1- n) y n y)		;; kết hợp lại
		    (return))						
		   ;; thoát khỏi vòng loop
		   (t
		    (return)))))))))

(defun move-down () ;; dịch xuong  các ô
  (dotimes (x +NB-LINES+)
    (do ((y (- +NB-LINES+ 2) (1- y)));; for (int j = +NB-LINES+ - 2; i>0 ; i--) ;; phải chừa ra hang cuoi cung để dễ tính toán
        ((minusp y))
      (when (not (check-if-case-empty x y)) ;; if ( cell[i][y] != NULL)
        (let ((n y))	;; int n = i;
          (loop	;while (n>0)
	     (cond ((= n (1- +NB-LINES+)) ;; cond xài giống case
		    (return))
		   ((check-if-case-empty x (1+ n))
		    ;;if(cell bên duoi là cell trống)
		    (swap-with-nil-case x (1+ n) x n)		
		    ;;ta hoán đổi và tăng biến n lên
		    (incf n))						
		   ;; vòng lặp sẽ chạy tiếp
		   ((check-if-equal x (1+ n) x n)	
		    ;;if(cell duoi có giá trị và bằng ô này thì)
		    (combine-two-case x (1+ n) x n)		;; kết hợp lại
		    (return))						
		   ;; thoát khỏi vòng loop
		   (t
		    (return)))))))))

(defun move-up () ;; dịch len  các ô
  (dotimes (x +NB-LINES+)
    (do ((y 1 (1+ y)));; for (int j = 1; j<NB-LINES ; j++) ;; phải chừa ra hang dau tien để dễ tính toán
        ((>= y +NB-LINES+))
      (when (not (check-if-case-empty x y)) ;; if ( cell[i][y] == NULL)
        (let ((n y))	;; int n = i;
          (loop	;while (n>0)
	     (cond ((= n 0) ;; cond xài giống case
		    (return))
		   ((check-if-case-empty x (1- n))
		    ;;if(cell bên tren là cell trống)
		    (swap-with-nil-case x (1- n) x n)		
		    ;;ta hoán đổi và giam biến n xuong
		    (decf n))						
		   ;; vòng lặp sẽ chạy tiếp
		   ((check-if-equal x (1- n) x n)	
		    ;;if(cell bên phải có giá trị và bằng ô này thì)
		    (combine-two-case x (1- n) x n )		;; kết hợp lại
		    (return))						
		   ;; thoát khỏi vòng loop
		   (t
		    (return)))))))))
