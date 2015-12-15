(defpackage :life
  (:use :cl :ltk))
(in-package :life)

(defun button-marked-p (button)
  (eql (char (text button) 0) #\X))

(defun toggle-button (button)
  (setf (text button) (if (button-marked-p button) " " "X")))

(defun life-from-buttons (buttons width height)
  (loop with life = (make-array (list width height))
     for i from 0 and button across buttons
     for x = (floor i height) and y = (mod i height) do
       (setf (aref life x y) (button-marked-p button))
     finally (return life)))

(defun life-setup (width height)
  (with-ltk ()
    (let* ((new-button  #'(lambda () (make-instance 'button :text " ")))
           (array       (make-array (* width height)))
           (buttons     (map-into array new-button))
           (save-board  #'(lambda () (life-from-buttons buttons width height)))
           (save-button (make-instance 'button :text "Generate GIF"
                                       :command save-board)))
      (loop initially (grid save-button 0 0)
         for button across buttons
         for i from 0 do
           (let ((i i))
             (bind button "<Button-1>"
                   #'(lambda (event)
                       (declare (ignore event))
                       (toggle-button (elt buttons i)))))
           (grid button (mod i height) (1+ (floor i height)))))))
