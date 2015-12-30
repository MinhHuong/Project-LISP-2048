(defun main ()
  (with-ltk ()
    (let* ((frame-sub (make-instance 'frame))
	   (bt-new-game (make-instance 'button
				       :master frame-sub
				       :text "New game"))
	   (lb-2048 (make-instance 'label
				      :master frame-sub
				      :text 2048))
	   (frame-grid (make-instance 'frame))
	   (canvas (make-instance 'canvas :master frame-grid))
	   (image (make-image))
	   (rectangle (create-rectangle canvas 29 29 351 351))
	   )
      ;; Configuring the frame, button and label
      (pack frame-sub :expand t :fill :x)
      (pack bt-new-game :side :left :padx 30 :pady 20)
      (pack lb-2048 :side :right :padx 30 :pady 20)

      ;; Configuring the canvas
      (configure canvas :width 380 :height 380)
      (configure frame-grid :takefocus t)
      (focus frame-grid)
      (pack frame-grid :after frame-sub)
      (pack canvas)
      
      ; Draw the lines
      (itemconfigure canvas rectangle :width 5)
      (itemconfigure canvas rectangle :outline :slategray)
      (itemconfigure canvas rectangle :fill :beige)
      
      ; Horizontal lines
      (create-line canvas (list 29 110 351 110))
      (create-line canvas (list 29 190 351 190))
      (create-line canvas (list 29 270 351 270))
      
      ;Vertical lines
      (create-line canvas (list 110 29 110 351))
      (create-line canvas (list 190 29 190 351))
      (create-line canvas (list 270 29 270 351))

      (image-load image "~/Desktop/Prog3/PROJECT/src/img/number-2.png")
      (setf image (create-image canvas 33 33 :image image))
      (bind frame-grid "<KeyPress>"
	    (lambda (evt)
	      (case (event-keycode evt)
		((111) ;; UP
		 ;(itemdelete canvas image)
		 (itemmove canvas image 0 -80)
		 )
		((113) ;; LEFT
		 (itemmove canvas image -80 0)
		 )
		((114) ;; RIGHT
		 (itemmove canvas image 80 0)
		 )
		((116) ;; DOWN
		 (itemmove canvas image 0 80))
		)))
      )))
			 
