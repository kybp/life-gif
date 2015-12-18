(defpackage :life-gif
  (:use :cl :ltk :skippy)
  (:shadowing-import-from :ltk :scale :make-canvas)
  (:shadowing-import-from :skippy :canvas-image :make-image)
  (:export :life-gif))
(in-package :life-gif)

;;; Util

(defun wrap (x min max)
  (cond ((< x min) max)
        ((> x max) min)
        (t x)))

;;; Life

(defclass life ()
  ((width  :reader width  :initarg :width)
   (height :reader height :initarg :height)
   (cells  :reader cells)))

(defun make-life (width height)
  (let* ((life  (make-instance 'life :width width :height height))
         (cells (make-array (list width height) :initial-element nil)))
    (setf (slot-value life 'cells) cells)
    life))

(defun cell (life x y)
  (let ((wrapped-x (wrap x 0 (1- (width  life))))
        (wrapped-y (wrap y 0 (1- (height life)))))
    (aref (cells life) wrapped-x wrapped-y)))

(defun (setf cell) (alivep life x y)
  (let ((wrapped-x (wrap x 0 (1- (width  life))))
        (wrapped-y (wrap y 0 (1- (height life)))))
    (setf (aref (cells life) wrapped-x wrapped-y) alivep)))

(defun cell-alive-p (life x y)
  (cell life x y))

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

;;; UI

(defparameter *generations* nil)

(defun life-gif (width height)
  (with-ltk ()
    (let* ((new-button  #'(lambda ()
                            (make-instance 'button :text " " :width 1)))
           (buttons*    (make-array (* width height)))
           (buttons     (map-into buttons* new-button))
           (life        (make-life width height))
           (prev-button (make-instance 'button :text "Prev" :state :disabled))
           (next-button (make-instance 'button :text "Next"))
           (delay-label (make-instance 'label :text "Delay"))
           (delay-scale (make-instance 'scale :from 1 :to 50))
           (size-label  (make-instance 'label :text "Cell Size"))
           (size-scale  (make-instance 'scale :from 1 :to 100))
           (frame-label (make-instance 'label :text "Frames"))
           (frame-scale (make-instance 'scale :from 1 :to 100))
           (file-label  (make-instance 'label :text "Filename"))
           (file-entry  (make-instance 'entry))
           (gif-button  (make-instance 'button :text "Generate GIF")))
      (grid prev-button 0 0) (grid next-button 0 1)
      (grid delay-label 1 0) (grid delay-scale 1 1)
      (grid frame-label 2 0) (grid frame-scale 2 1)
      (grid size-label  3 0) (grid size-scale  3 1)
      (grid file-label  4 0) (grid file-entry  4 1)
      (grid gif-button  5 0 :columnspan 2)
      (bind prev-button "<Button-1>"
            #'(lambda (event)
                (declare (ignore event))
                (unless (null *generations*)
                  (update-buttons buttons (pop *generations*))
                  (when (null *generations*)
                    (configure prev-button :state :disabled)))))
      (bind next-button "<Button-1>"
            #'(lambda (event)
                (declare (ignore event))
                (let ((next (next-generation life)))
                  (push life *generations*)
                  (setf life next)
                  (update-buttons buttons life)
                  (configure prev-button :state :enabled))))
      (bind gif-button "<Button-1>"
            #'(lambda (event)
                (declare (ignore event))
                (let* ((path (text file-entry))
                       (dir  (cl-fad:pathname-directory-pathname path)))
                  (if (and (probe-file dir)
                           (not (cl-fad:directory-pathname-p path)))
                      (generate-gif life
                                    (floor (or (value frame-scale) 2))
                                    (floor (or (value size-scale)  1))
                                    (floor (or (value delay-scale) 1))
                                    path)
                      (message-box (format nil "Invalid filename: ~s" path)
                                   "Error" :ok :error)))))
      (loop
         for button across buttons
         for i from 0
         for x = (floor i height) and y = (mod i height) do
           (let ((i i) (x x) (y y))
             (bind button "<Button-1>"
                   #'(lambda (event)
                       (declare (ignore event))
                       (setf (cell life x y) (not (cell life x y)))
                       (toggle-button (elt buttons i)))))
           (grid button y (+ x 2))))))

(defun toggle-button (button)
  (setf (text button) (if (eql (char (text button) 0) #\X) " " "X")))

(defun update-buttons (buttons life)
  (let ((i 0))
    (dotimes (x (width life))
      (dotimes (y (height life))
        (setf (text (elt buttons i))
              (if (cell-alive-p life x y) "X" " "))
        (incf i)))))

;;; GIF generation

(defvar *dead-rgb*  #x006400)
(defvar *alive-rgb* #xffff00)

(defun generate-frame (life image cell-size)
  (dotimes (x (width life))
    (dotimes (y (height life))
      (fill-area image (if (cell-alive-p life x y) 1 0)
                 :x (* x cell-size) :width  cell-size
                 :y (* y cell-size) :height cell-size))))

(defun generate-gif (life frames cell-size delay outfile)
  (let* ((width  (* (width  life) cell-size))
         (height (* (height life) cell-size))
         (ct     (make-color-table))
         (stream (make-data-stream :height height :width width
                                   :color-table ct :loopingp t)))
    (add-color *dead-rgb*  ct)
    (add-color *alive-rgb* ct)
    (dotimes (i frames)
      (let ((image (make-image :height height :width width
                               :color-table ct :delay-time delay
                               :data-stream stream)))
        (generate-frame life image cell-size)
        (add-image image stream)
        (let ((next (next-generation life)))
          (dotimes (x (width life))
            (dotimes (y (height life))
              (setf (cell life x y) (cell next x y)))))))
    (output-data-stream stream outfile)
    (print 'done)))
