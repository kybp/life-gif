(defpackage :life
  (:use :cl :ltk :skippy)
  (:shadowing-import-from :ltk :scale)
  (:shadowing-import-from :skippy :make-canvas :canvas-image :make-image))
(in-package :life)

(defun button-marked-p (button)
  (eql (char (text button) 0) #\X))

(defun toggle-button (button)
  (setf (text button) (if (button-marked-p button) " " "X")))

(defclass life ()
  ((width  :reader width  :initarg :width)
   (height :reader height :initarg :height)
   (cells  :reader cells)))

(defun make-life (width height)
  (let* ((life  (make-instance 'life :width width :height height))
         (cells (make-array (list width height) :initial-element nil)))
    (setf (slot-value life 'cells) cells)
    life))

(defun wrap (x min max)
  (cond ((< x min) max)
        ((> x max) min)
        (t x)))

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

(defun life-from-buttons (buttons width height)
  (loop with life = (make-life width height)
     for i from 0 and button across buttons
     for x = (floor i height) and y = (mod i height) do
       (setf (cell life x y) (button-marked-p button))
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
    (let* ((new-button  #'(lambda ()
                            (make-instance 'button :text " " :width 5)))
           (array       (make-array (* width height)))
           (buttons     (map-into array new-button))
           (life        (life-from-buttons buttons width height))
           (update-life #'(lambda () (sync-and-update life buttons)))
           (next-button (make-instance 'button :text "Next"
                                       :command update-life))
           (delay-scale (make-instance 'scale))
           (size-scale  (make-instance 'scale))
           (frame-scale (make-instance 'scale))
           (file-entry  (make-instance 'entry))
           (gen-button  (make-instance 'button :text "Generate GIF")))
      (grid next-button 0 0)
      (grid gen-button  1 0)
      (grid file-entry  2 0)
      (grid size-scale  3 0)
      (grid frame-scale 4 0)
      (grid delay-scale 5 0)
      (bind gen-button "<Button-1>"
            #'(lambda (event)
                (declare (ignore event))
                (generate-gif (life-from-buttons buttons width height)
                              (floor (* 500 (value frame-scale)))
                              (floor (* 50 (value size-scale)))
                              (floor (* 20 (value delay-scale)))
                              (text file-entry))))
      (loop
         for button across buttons
         for i from 0 do
           (let ((i i))
             (bind button "<Button-1>"
                   #'(lambda (event)
                       (declare (ignore event))
                       (toggle-button (elt buttons i)))))
           (grid button (mod i height) (1+ (floor i height)))))))

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
                                   :color-table ct)))
    (add-color #x224f00 ct)
    (add-color #xffff00 ct)
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
    (setf (loopingp stream) t)
    (output-data-stream stream outfile)
    (print 'done)))
