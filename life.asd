(defpackage :life-gif-asd
  (:use :cl :asdf))
(in-package :life-gif-asd)

(defsystem :life-gif
  :description "Generate animated GIFs of Conway's Game of Life"
  :depends-on (:ltk :skippy)
  :components ((:file "life")))
