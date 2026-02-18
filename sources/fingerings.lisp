;;;; Fingerings - Music theory library for stringed instruments
;;;; Translated from TypeScript to Common Lisp
;;;; From: https://github.com/icalvo/StringTools


(in-package :fingerings)

;;; Note class
(defclass note ()
  ((name :initarg :name :accessor note-name :type character)
   (octave :initarg :octave :accessor note-octave :type integer)
   (alteration :initarg :alteration :accessor note-alteration :type string)
   (number :initarg :number :accessor note-number :type integer)))

(defun make-note (name octave alteration number)
  "Create a new note instance"
  (make-instance 'note
                 :name name
                 :octave octave
                 :alteration alteration
                 :number number))

(defmethod note-text ((n note))
  "Convert note to text representation"
  (format nil "~A~A~A" 
          (note-name n)
          (note-alteration n)
          (1- (note-octave n))))

(defmethod note-abcnote ((n note))
  "Convert note to ABC notation"
  (let* ((note-name (char-downcase (note-name n)))
         (alteration (case (intern (note-alteration n) :keyword)
                      (:|#| "^")
                      (:|b| "_")
                      (:|x| "^^")
                      (:|bb| "__")
                      (otherwise "")))
         (octave (note-octave n)))
    (if (<= octave 5)
        (let ((note-name (char-upcase note-name))
              (ticks (- 5 octave)))
          (format nil "~A~A~A" alteration note-name (make-string ticks :initial-element #\,)))
        (let ((ticks (- octave 6)))
          (format nil "~A~A~A" alteration note-name (make-string ticks :initial-element #\'))))))

;;; Parsing functions
(defun try-parse (note-text)
  "Try to parse a note from text. Returns note or error string."
  (setf note-text (string-trim '(#\Space #\Tab #\Newline) note-text))
  (cond
    ((< (length note-text) 2)
     "Note names must be at least two characters long")
    ((> (length note-text) 4)
     "Note names must be at most four characters long")
    (t
     (let* ((first-char (char-upcase (char note-text 0)))
            (last-char (char note-text (1- (length note-text)))))
       (cond
         ((or (char< first-char #\A) (char> first-char #\G))
          "First char must be A-G")
         ((or (char< last-char #\0) (char> last-char #\9))
          "Last char must be a number")
         (t
          (let* ((octave (digit-char-p last-char))
                 (alteration (subseq note-text 1 (1- (length note-text))))
                 (alteration-offset (cond
                                     ((string= alteration "bb") -2)
                                     ((string= alteration "x") 2)
                                     ((string= alteration "b") -1)
                                     ((string= alteration "#") 1)
                                     ((string= alteration "") 0)
                                     (t (return-from try-parse 
                                          (format nil "Second char must be #, b, x or bb. Text: ~A, alteration: ~A"
                                                  note-text alteration)))))
                 (note-index (mod (+ (- (char-code first-char) (char-code #\C)) 7) 7))
                 (semitone-index #(0 2 4 5 7 9 11))
                 (number (+ (aref semitone-index note-index)
                           alteration-offset
                           (* (1+ octave) 12))))
            (make-note first-char (1+ octave) alteration number))))))))

(defun parse (note-text)
  "Parse a note from text, throwing error if invalid"
  (let ((result (try-parse note-text)))
    (if (stringp result)
        (error result)
        result)))

(defun get-note-number (note-name)
  "Gets the MIDI note number from a text representation (middle C is 'C4')"
  (let ((n (try-parse note-name)))
    (if (stringp n)
        n
        (note-number n))))
;(get-note-number "E4")

(defun nn (note-name)
  "Get note number, throwing error if invalid"
  (let ((n (get-note-number note-name)))
    (if (stringp n)
        (error "Invalid note name: ~A" n)
        n)))

(defun get-note-name (note-number)
  "Convert MIDI note number to note name"
  (unless (and (integerp note-number) 
               (<= 0 note-number 127))
    (error "Invalid note number"))
  (let* ((note-index (mod note-number 12))
         (note-names #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
         (note-name (aref note-names note-index))
         (octave (- (floor note-number 12) 1)))
    (format nil "~A~A" note-name octave)))

;(get-note-name 64)

(defun abcnote (note-number)
  "Convert MIDI note number to ABC notation"
  (note-abcnote (parse (get-note-name note-number))))

;(abcnote 64)

;;; Stop calculation
(defstruct instrument-string
  open-note
  additional-open-semitones
  stops)

(defstruct instrument
  name
  strings
  stops
  scale-length
  max-stretch
  hard-stretch)

(defstruct stop
  string-index
  note-number
  stop-index
  natural-harmonic)

(defun stops-for-string (instrument string-index note-number include-natural-harmonics)
  "Generate stops for a single string"
  (format t "~&Calculating stop for note ~A on string ~A~%" note-number string-index)
  (let* ((instrument-string (nth string-index (instrument-strings instrument)))
         (stops nil))
    (when instrument-string
      (let* ((open-note (note-number (instrument-string-open-note instrument-string)))
             (stop-index (- note-number open-note))
             (additional-open-semitones (or (instrument-string-additional-open-semitones instrument-string) nil))
             (additional-open-notes (mapcar (lambda (s) (+ s open-note)) additional-open-semitones))
             (max-stops (or (instrument-string-stops instrument-string) 
                           (instrument-stops instrument))))
        
        (unless (and (< stop-index 0) (not (member note-number additional-open-notes)))
          (if (> stop-index max-stops)
              (format t "~&~A is too high to stop for ~A string~%" 
                      (get-note-name note-number)
                      (note-name (instrument-string-open-note instrument-string)))
              (push (make-stop :string-index string-index
                              :note-number note-number
                              :stop-index stop-index
                              :natural-harmonic nil)
                    stops)))
        
        (when include-natural-harmonics
          (let ((natural-harmonics 
                 '((:partial 2 :touch 12 :produces 12)
                   (:partial 3 :touch 7 :produces 19)
                   (:partial 3 :touch 19 :produces 19)
                   (:partial 4 :touch 5 :produces 24)
                   (:partial 4 :touch 24 :produces 24)
                   (:partial 5 :touch 4 :produces 28)
                   (:partial 5 :touch 9 :produces 28);manque!
                   (:partial 5 :touch 16 :produces 28)
                   (:partial 5 :touch 28 :produces 28)
                   )))
            (dolist (h natural-harmonics)
              (let ((produces (+ (getf h :produces) open-note)))
                (when (= produces note-number)
                  (push (make-stop :string-index string-index
                                  :stop-index (getf h :touch)
                                  :note-number produces
                                  :natural-harmonic t)
                        stops))))))))
    (nreverse stops)))

;this is from test "fingerings-test.lisp"

;(stops-for-string *violin* 1 86 t)

(defun get-stop-rel-pos (stop-index)
  "Returns the stop position relative to a string of length 1"
  (- 1 (expt 2 (/ (- stop-index) 12))))

(defun stops-for-instrument (instrument notes include-natural-harmonics)
  "Calculate all possible stops for each note"
  (let ((result 
         (loop for note in notes
               collect (loop for string-index from 0 below (length (instrument-strings instrument))
                            append (stops-for-string instrument string-index note include-natural-harmonics)))))
    (if (some #'null result)
        nil
        result)))

;Voila c'est ici (il faut peut-etre un mat-trans!)
;(stops-for-instrument *violin* '(64 82) t)

(defun cross-product (add-validation l1 l2)
  "Cross product with validation"
  (if (null l1)
      (mapcar #'list l2)
      (loop for i1 in l1
            append (loop for i2 in l2
                        when (funcall add-validation i1 i2)
                        collect (append i1 (list i2))))))
;

(defun pairwise (list)
  "Return consecutive pairs from list"
  (loop for (a b) on list
        while b
        collect (list a b)))

(defun fingering-stretch (instrument stop1 stop2)
  "Calculate physical stretch distance between two stops"
  (let ((stop-rel-pos1 (get-stop-rel-pos (stop-stop-index stop1)))
        (stop-rel-pos2 (get-stop-rel-pos (stop-stop-index stop2))))
    (* (instrument-scale-length instrument)
       (abs (- stop-rel-pos2 stop-rel-pos1)))))

(defun open-string-p (stop)
  "Check if stop is on open string"
  (<= (stop-stop-index stop) 0))

(defun stretch-hardness (instrument stop1 stop2 multiplier)
  "Calculate difficulty of a stretch between two stops"
  (let ((stretch (fingering-stretch instrument stop1 stop2)))
    (format t "~&Stretch: ~A ~A ~A~%" stretch stop1 stop2)
    (cond
      ((or (open-string-p stop1) (open-string-p stop2)) 0.0)
      ((> stretch (* (instrument-max-stretch instrument) multiplier)) 1.0)
      ((> stretch (* (instrument-hard-stretch instrument) multiplier)) 0.5)
      (t 0.1))))

(defun fingering-hardness (instrument fingering)
  "Calculate overall difficulty of a fingering"
  (let* ((sorted-fingering (sort (copy-list fingering) #'< :key #'stop-string-index))
         (stop-pairs (pairwise sorted-fingering))
         (sorted-by-stop-index (sort (copy-list sorted-fingering) #'< :key #'stop-stop-index))
         (min-stop (first sorted-by-stop-index))
         (max-stop (car (last sorted-by-stop-index)))
         (contiguous-hardnesses (mapcar (lambda (pair)
                                         (stretch-hardness instrument (first pair) (second pair) 1))
                                       stop-pairs))
         (total-hardness (stretch-hardness instrument min-stop max-stop 1.02))
         (all-hardnesses (append contiguous-hardnesses (list total-hardness))))
    (apply #'max all-hardnesses)))

(defun has-no-gaps (instrument fingering)
  "Check if fingering has no gaps between strings"
  (declare (ignore instrument))
  (let ((string-indices (sort (mapcar #'stop-string-index fingering) #'<)))
    (every (lambda (pair)
             (<= (abs (- (second pair) (first pair))) 1))
           (pairwise string-indices))))

(defun has-possible-stretch (instrument fingering)
  "Check if fingering stretch is physically possible"
  (/= (fingering-hardness instrument fingering) 1.0))

(defun calculate-fingerings (instrument notes validations &optional (include-natural-harmonics nil))
  "Calculates the fingerings for an instrument and a set of notes"
  (format t "~&Calculating fingerings for ~A on ~A~%" notes (instrument-name instrument))
  
  (let ((stops-by-note (stops-for-instrument instrument notes include-natural-harmonics)))
    (cond
      ((= (length notes) 1)
       (loop for note-stops in stops-by-note
             append (mapcar #'list note-stops)))
      (t
       (labels ((string-not-repeated (stop-list stop)
                  (every (lambda (existing-stop)
                          (/= (stop-string-index existing-stop)
                              (stop-string-index stop)))
                        stop-list))
                (validation (fng)
                  (every (lambda (val)
                          (funcall val instrument fng))
                        validations)))
         (remove-if-not #'validation
                       (reduce (lambda (acc stops)
                                (cross-product #'string-not-repeated acc stops))
                              stops-by-note
                              :initial-value nil)))))))

;;;;;;;OM Methods
#|
(om:defmethod! get-fingerings ((self list) 
                              (instr t)  
                              &key 
                              (natural-harmonics nil)
                              (rules (list (lambda (x y) (has-no-gaps *violin* y))
                                           (lambda (x y) (has-possible-stretch *violin* y))))
                              )
  :icon 133
  :indoc '("notes" "instr"  "natural-harmonics" "rules")
  :initvals '( '(5500 6200) 
               *violin* 
               nil
               (list (lambda (x y) (has-no-gaps *violin* y))
                     (lambda (x y) (has-possible-stretch *violin* y))))
  :menuins '((1 (("violin" *violin*) 
                 ("viola" *viola*))))
  
  (let* ((notes (om::om/ (om::approx-m self 2) 100))
         (query (calculate-fingerings instr notes rules natural-harmonics)))
    (loop for res in query
      collect 
        (loop for i in res
              collect (list 
                       (stop-string-index i)
                       (stop-note-number i)
                       (stop-stop-index i)
                       (stop-natural-harmonic i))))
    ))

;(get-fingerings '(8300) *violin* :natural-harmonics t)

(om:defmethod! get-fingerings ((self om::chord) 
                              (instr t)  
                              &key 
                              (natural-harmonics nil)
                              (rules (list (lambda (x y) (has-no-gaps *violin* y))
                                           (lambda (x y) (has-possible-stretch *violin* y))))
                              )
  (get-fingerings (om::lmidic self) instr 
                  :natural-harmonics natural-harmonics 
                  :rules rules))
|#


(om:defmethod! get-fingerings ((self list) 
                              (instr t)  
                              &key 
                              (natural-harmonics nil)
                              )
  :icon 133
  :indoc '("notes" "instr"  "natural-harmonics" "rules")
  :initvals '( '(5500 6200) 
               0
               nil
               )

  :menuins '((1 (("violin" 0) 
                 ("viola" 1)
                 ("cello" 2))))
  
             (let* ((instrument
                     (cond 
                      ((equal instr 0) *violin*)
                      ((equal instr 1) *viola*)
                      (t *cello*)))
                    (notes (om::om/ (om::approx-m self 2) 100))
                    (query (calculate-fingerings instrument notes 
                                                 (list (lambda (x y) (has-no-gaps instrument y))
                                                       (lambda (x y) (has-possible-stretch instrument y)))
                                                 natural-harmonics))
                    (res (loop for res in query
                               collect 
                                 (loop for i in res
                                       collect (list 
                                                (stop-string-index i)
                                                (stop-note-number i)
                                                (stop-stop-index i)
                                                (stop-natural-harmonic i))))))
    (loop for i in res
          collect (om:sort-list i :test '< :key 'car))
    ))

;(get-fingerings '(8300) 0 :natural-harmonics t)


(om:defmethod! get-fingerings ((self om::chord) 
                              (instr t)  
                              &key 
                              (natural-harmonics nil)
                              )
  (get-fingerings (om::lmidic self) instr 
                  :natural-harmonics natural-harmonics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;TEXT DISPLAY;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-name (stg inst)
  (if (= inst 0);violin
      (cond 
       ((= 0 stg) "G")
       ((= 1 stg) "D")
       ((= 2 stg) "A")
       (t "E"))
    (cond 
     ((= 0 stg) "C")
     ((= 1 stg) "G")
     ((= 2 stg) "D")
     (t "A"))))

;(string-name 1 0) 

(defun finger-pretty-print (lst inst) 
  (if lst
  (let ((resultat ""))
  (loop for res in lst
        for n from 1 to (length lst)
        do 
          (progn
          (setf resultat (om:string+ resultat (format nil "~%~%")))
          (setf resultat (om:string+ resultat
          (let ((str ""))
            (setf str (om::string+ str (format nil "FINGERING: ~d ~%~%" n)))
            (loop for i in res
                  do  (let ((stg (car i))
                            (note (second i))
                            (pos (third i))
                            (har (fourth i)))
                        (cond 
                         ((= pos 0)
                          (setf str (om::string+ str (format nil "Open ~A string ~%" (string-name stg inst)))));IMPORTANT ADAPT ON INSTR
                         (har 
                          (setf str (om::string+ str (format nil "Touch ~A string at ~A (natural harmonic) ~%" 
                                                             (string-name stg inst)
                                                             pos
                                                             ))))
                         (t
                          (setf str (om::string+ str (format nil "Stop ~A string at ~A ~%" 
                                                             (string-name stg inst) pos)))))))
            str)))))
  resultat)
    "No results!"))

;(finger-pretty-print (get-fingerings '(8300 6200 7500) 0 :natural-harmonics t) 0)