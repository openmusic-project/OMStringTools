(in-package :cl-user)

(defpackage fingerings
  (:use  "COMMON-LISP" "CL-USER")
  ;(:import-from )
  
  (:export #:note
           #:make-note
           #:note-name
           #:note-octave
           #:note-alteration
           #:note-number
           #:note-text
           #:note-abcnote
           #:try-parse
           #:parse
           #:get-note-number
           #:nn
           #:get-note-name
           #:abcnote
           #:get-stop-rel-pos
           #:calculate-fingerings
           #:fingering-hardness
           #:has-no-gaps
           #:has-possible-stretch)
  
  (:nicknames "FING")
  )