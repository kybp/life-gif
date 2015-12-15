(defpackage :life
  (:use :cl :ltk))
(in-package :life)

(defun button-marked-p (button)
  (eql (char (text button) 0) #\X))

(defun toggle-button (button)
  (setf (text button) (if (button-marked-p button) " " "X")))

(defun make-life (width height)
  (make-array (list width height) :initial-element nil))

(defun wrap (x min max)
  (cond ((< x min) max)
        ((> x max) min)
        (t x)))

(defun width  (life) (array-dimension life 0))
(defun height (life) (array-dimension life 1))

(defun cell (life x y)
  (let ((wrapped-x (wrap x 0 (1- (width  life))))
        (wrapped-y (wrap y 0 (1- (height life)))))
    (aref life wrapped-x wrapped-y)))

(defun (setf cell) (alivep life x y)
  (let ((wrapped-x (wrap x 0 (1- (width  life))))
        (wrapped-y (wrap y 0 (1- (height life)))))
    (setf (aref life wrapped-x wrapped-y) alivep)))

(defun cell-alive-p (life x y)
  (cell life x y))

(defun life-from-buttons (buttons width height)
  (loop with life = (make-array (list width height))
     for i from 0 and button across buttons
     for x = (floor i height) and y = (mod i height) do
       (setf (aref life x y) (button-marked-p button))
     finally (return life)))

(defun sync/buttons (life buttons)
  (let ((i 0))
    (dotimes (x (width life))
      (dotimes (y (height life))
        (setf (cell life x y) (button-marked-p (aref buttons i)))
        (incf i)))))

(defun sync-and-update (life buttons)
  (sync/buttons life buttons)
  (let ((next (next-generation life))
        (i    0))
    (dotimes (x (width life))
      (dotimes (y (height life))
        (setf (text (aref buttons i))
              (if (cell-alive-p next x y) "X" " "))
        (incf i)))))

(defun life-setup (width height)
  (with-ltk ()
    (let* ((new-button  #'(lambda () (make-instance 'button :text " ")))
           (array       (make-array (* width height)))
           (buttons     (map-into array new-button))
           (life        (life-from-buttons buttons width height))
           (update-life #'(lambda () (sync-and-update life buttons)))
           (next-button (make-instance 'button :text "Next"
                                       :command update-life)))
      (loop initially (grid next-button 0 0)
         for button across buttons
         for i from 0 do
           (let ((i i))
             (bind button "<Button-1>"
                   #'(lambda (event)
                       (declare (ignore event))
                       (toggle-button (elt buttons i)))))
           (grid button (1+ (mod i height)) (floor i height))))))

(defun neighbours (life x y)
  (loop for i from (1- x) to (1+ x)
     sum (loop for j from (1- y) to (1+ y)
            count (and (cell-alive-p life i j)
                       (not (and (= i x) (= j y)))))))

(defun next-generation (life)
  (let ((next (make-life (width life) (height life))))
    (dotimes (x (width life) next)
      (dotimes (y (height life))
        (let ((neighbours (neighbours life x y)))
          (if (cell-alive-p life x y)
              (setf (cell next x y) (member neighbours '(2 3)))
              (setf (cell next x y) (= neighbours 3))))))))
