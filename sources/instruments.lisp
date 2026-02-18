
(in-package :fingerings)

(defparameter *violin*
  (make-instrument
   :name "Violin"
   :stops 24
   :scale-length 330
   :hard-stretch 78
   :max-stretch 92
   :strings (list
             (make-instrument-string :open-note (parse "G3"))
             (make-instrument-string :open-note (parse "D4"))
             (make-instrument-string :open-note (parse "A4"))
             (make-instrument-string :open-note (parse "E5")))))

;viola 16.5''
(defparameter *viola*
  (make-instrument
   :name "Viola"
   :stops 18
   :scale-length 419
   :hard-stretch 78
   :max-stretch 92
   :strings (list
             (make-instrument-string :open-note (parse "C3"))
             (make-instrument-string :open-note (parse "G3"))
             (make-instrument-string :open-note (parse "D4"))
             (make-instrument-string :open-note (parse "A4")))))

;viola 16.5''
(defparameter *viola-16.5*
  (make-instrument
   :name "Viola 16.5"
   :stops 18
   :scale-length 419
   :hard-stretch 78
   :max-stretch 92
   :strings (list
             (make-instrument-string :open-note (parse "C3"))
             (make-instrument-string :open-note (parse "G3"))
             (make-instrument-string :open-note (parse "D4"))
             (make-instrument-string :open-note (parse "A4")))))
;viola 16''
(defparameter *viola-16*
  (make-instrument
   :name "Viola 16"
   :stops 18
   :scale-length 406
   :hard-stretch 78
   :max-stretch 92
   :strings (list
             (make-instrument-string :open-note (parse "C3"))
             (make-instrument-string :open-note (parse "G3"))
             (make-instrument-string :open-note (parse "D4"))
             (make-instrument-string :open-note (parse "A4")))))

;viola 15.5''
(defparameter *viola-15.5*
  (make-instrument
   :name "Viola 15.5"
   :stops 18
   :scale-length 394
   :hard-stretch 78
   :max-stretch 92
   :strings (list
             (make-instrument-string :open-note (parse "C3"))
             (make-instrument-string :open-note (parse "G3"))
             (make-instrument-string :open-note (parse "D4"))
             (make-instrument-string :open-note (parse "A4")))))

(defparameter *cello*
  (make-instrument
   :name "Cello"
   :stops 18
   :scale-length 690
   :hard-stretch 78
   :max-stretch 116
   :strings (list
             (make-instrument-string :open-note (parse "C2"))
             (make-instrument-string :open-note (parse "G2"))
             (make-instrument-string :open-note (parse "D3"))
             (make-instrument-string :open-note (parse "A3")))))

(defparameter *double-bass*
  (make-instrument
   :name "Double bass"
   :stops 24
   :scale-length 1100
   :hard-stretch 116
   :max-stretch 120
   :strings (list
             (make-instrument-string 
              :open-note (parse "E1")
              :additional-open-semitones '(-1 -2 -3 -4))
             (make-instrument-string :open-note (parse "A1"))
             (make-instrument-string :open-note (parse "D2"))
             (make-instrument-string :open-note (parse "G2")))))

