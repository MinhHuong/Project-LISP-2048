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
