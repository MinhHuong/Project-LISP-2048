(defun hello-1 ()
  (with-ltk ()
    (let ((b (make-instance 'button
			    :master nil
			    :text "press me"
			    :command (lambda () (format t "Hello world!~&")))))
      (pack b))))

(defun hello-2 ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
	   (b1 (make-instance 'button
			      :master f
			      :text "button 1"
			      :command (lambda () (format t "Button 1~&"))))
	   (b2 (make-instance 'button
			      :master f
			      :text "button 2"
			      :command (lambda () (format t "Button 2~&")))))
      (pack f)
      (pack b1 :side :left)
      (pack b2 :side :left)
      (configure f :borderwidth 3)
      (configure f :relief :sunken))))

(defun canvas-test ()
  (with-ltk ()
    (let* ((sc (make-instance 'canvas))
	   (c (canvas sc)))
      (pack sc :expand 1 :fill :both)
      (scrollregion c 0 0 800 800))))

(defun make-grid ()
  (with-ltk ()
    (let* ((sc (make-instance 'canvas))
	   (c (canvas sc))
	   (gr (create-rectangle c 0 0 200 200))
	   (l1 (create-line c (list 0 50 200 50)))
	   (l2 (create-line c (list 0 100 200 100)))
	   (l3 (create-line c (list 0 150 200 150)))
	   (l4 (create-line c (list 50 0 50 200)))
	   (l5 (create-line c (list 100 0 100 200)))
	   (l6 (create-line c (list 150 0 150 200)))
	   )
      (pack sc))))

(defun img-example ()
  (with-ltk ()
    (let ((image (make-image)))
      (image-load image "~/2.gif")
      (let ((canvas (make-instance 'canvas)))
	(create-image canvas 0 0 :image image)
	(configure canvas :width 300)
	(configure canvas :height 300)
	(pack canvas)
	(bind canvas "<KeyPress-Return>"
	      (lambda (evt)
		(format t "Button down")))))))
    
(defun make-frame ()
  (with-ltk ()
    (let* ((f (make-instance 'frame)))
      (pack f)
      (configure f :width 500)
      (configure f :height 500)
      (configure f :relief :sunken)
      (configure f :takefocus t)
      (focus f)
      (bind f "<KeyPress>"
	    (lambda (evt)
	      (format t "~%~A" (event-keycode evt))))
      )))
