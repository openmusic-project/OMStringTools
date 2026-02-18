;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

; Author: Karim Haddad

(in-package :om)

;;;preobject

(defclass internalfingering ()
   ((chord :accessor chord :initarg :chord :initform (make-instance 'chord) :documentation "initial chord (symbolic pitches)")
    (thedata :initform nil :accessor thedata :initarg :thedata)
    (name :initform nil :accessor name :initarg :name)
    ))

;;; OM OBJECT
(defclass! fingerboard (internalfingering) 
   ((notes :initarg :notes :initform nil :accessor notes)
    (instrument :initarg :instrument :initform nil :accessor instrument)
    (solutions :initarg :solutions :initform nil :accessor solutions))
   (:icon 133)
   (:documentation "
The PICTURE box allows to display external images and draw simple graphics.

<background> allows to set or read the main background. It can be a pathname or a pixel array (list of lists of pixels (R G B alpha) between 0.0 and 1.0.)

<graphics> correspond to a list of vectorial graphics (graphic-object) displayed on top of the background picture.

Using the contextual menu on this box, the picture can be converted to a background picture for the patch.
Then use 'y' key to select then move, resize or delete background pictures, or convert them back to picture boxes with the same contextual menu.

The same contextual menu allow to choose to save or not the contents of the picture inside the patch. If this option is not enabled, the picture will need to be recomputed each time the patch is loaded.
"))

(defmethod omNG-save ((self fingerboard) &optional (values? nil)) 
  "Cons a Lisp expression that retuns a copy of self when it is evaluated."
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self) 
                               :chord (make-instance 'chord :lmidic ',(lmidic (chord self))) 
                               )))
       rep
       )))

(defmethod omNG-copy ((self fingerboard))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                             :chord ',(chord self)
                             )))
     (setf (chord rep) ',(chord self))
     rep
     ))
;;;;


(defmethod update-miniview ((self t) (type fingerboard)) (om-invalidate-view self t))

(defmethod get-initval ((self fingerboard)) (make-instance 'fingerboard))

(defmethod default-obj-box-size ((self fingerboard)) (om-make-point 80 80))


;====================================================
;BOX and frame

(defmethod get-type-of-ed-box ((self fingerboard))  'OMFingerboardbox)

(defclass OMFingerboardbox (OMBoxEditCall) ())

(defmethod get-frame-class ((self OMFingerboardbox)) 'Fingboardboxframe)

;(defmethod default-edition-params ((self fingerboard)) 
;  (pairlis '(winsize winpos save-data) 
;           (list (or (get-win-ed-size self) (om-make-point 370 280))
;                 (or (get-win-ed-pos self) (om-make-point 400 20))
;                 t)))

;---------
;FRAME
(defclass Fingboardboxframe (boxEditorFrame) ()
   (:documentation "Simple frame for OMBoxEditCall meta objects. #enddoc#
#seealso# (OMBoxEditCall) #seealso#"))

(defmethod om-get-menu-context ((self Fingboardboxframe)) nil)
;  (list (om-new-leafmenu  "Set as Background Fingering" #'(lambda () (pict2bkg self)))
;        (pict-save-menu (object self))))

;disabled
;(defun pict-save-menu (box)
;  (om-new-leafmenu (if (get-edit-param box 'save-data) "Do Not Save Fingering Data with Patch" "Save Fingering Data with Patch")
;                   #'(lambda () (set-edit-param box 'save-data (not (get-edit-param box 'save-data)))
;                       (setf (storemode (value box)) (if (get-edit-param box 'save-data) :internal :external)))))

(defun pict-save-menu (box)
  (om-new-leafmenu (if (equal (storemode (value box)) :internal) "Do Not Save Fingering Data with Patch" "Save Fingering Data with Patch")
                   #'(lambda () 
                       (setf (storemode (value box)) (if (equal (storemode (value box)) :internal) :external :internal)))
                   nil (not (source (value box)))))



;;;===================
;;; EDITOR


(defmethod class-has-editor-p ((self fingerboard)) t)

(defmethod get-editor-class ((self fingerboard)) 'fingeditor)

(defmethod good-val-p? ((self fingerboard))
   ;(thepict self) 
  t)

(defclass fingeditor (editorview) 
  ((mode :initform :normal :accessor mode)
   (chordobj :initform nil :accessor chordobj)
   (textobj :initform nil :accessor textobj)
   (instrument :initform "Violin" :accessor instrument)
   (fingerings :initform nil :accessor fingerings)
   (selection :initform nil :accessor selection)
   (controlview :initform nil :accessor controlview)))

(defclass fingpanel (om-view om-drag-view om-drop-view) ())

(defmethod editor ((self fingpanel)) (om-view-container self))

(defmethod get-help-list ((self fingeditor)) 
  '((("tab" "Select a Graphic Object")
     ("del" "Delete Selected Object"))))

(defmethod get-menubar ((self fingeditor)) 
  (list (om-make-menu "File"
                      (list (om-new-leafmenu "Save Fingering" #'(lambda () (save-pict self)) "s")
                            (om-new-leafmenu "Close" #'(lambda () (om-close-window (window self))) "w")))    
        (om-make-menu "Edit"
                      (list (list 
                             (om-new-leafmenu "Load Fingering" #'(lambda () (load-new-pict self)))
                             (om-new-leafmenu "Remove Fingering" #'(lambda () (remove-pict self))
                                              nil (thedata (object self))))
                            (om-new-leafmenu "Remove All Graphics" #'(lambda () (remove-all-extraobjs self)))))
        (make-om-menu 'help :editor self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;taille fenetre: 1020x864


(defmethod get-win-ed-size ((self fingerboard) ) (om-make-point 1122 866))
(defmethod get-win-ed-size2 ((self fingerboard)) (om-make-point 1122 866))

;in order to disable resize (only).
(defmethod make-editor-window ((class (eql 'fingeditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow nil) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil
                                                      :close-p t :winshow t
                                                      )))
     win))

(defmethod initialize-instance :after ((self fingeditor) &rest args) 
  (let* ((chord (chord (object self))); (make-instance 'chord)))
         (midics (lmidic chord)))
    (om-set-bg-color self (om-make-color 0.8 0.8 0.8))
    (om-add-subviews self
                   
                     (setf (panel self) (om-make-view 'fingpanel
                                                      :position (om-make-point 10 0)
                                                      :size (om-make-point (w self) (- (h self) 40))))
                   
                     (setf (controlview self) (om-make-view 'fing-controls
                                                            :position (om-make-point 0 (- (h self) 40))
                                                            :size (om-make-point (w self) 40)
                                                            :bg-color *om-light-gray-color*
                                                            :owner self ;required to pass on solutions
                                                            )))
    
    (om-add-subviews (panel self)
                     ;chorded
                     (setf (chordobj self) (om-make-view (get-editor-class chord) 
                                                         :owner self :object chord
                                                         :ref (ref (editor self))
                                                         :position (om-make-point 0 0) :size (om-make-point 600 400)))
                     (setf (textobj self) (om-make-dialog-item 'edit-comment
                                                               (om-make-point 0 400)
                                                               (om-make-point 600 400)
                                                               (format nil "~A" (lmidic (chord (object self))))
                                                               :allow-returns t 
                                                               :scrollbars :v
                                                               :vertical-scroll t 
                                                               :retain-scrollbars t
                                                               :filed-size (om-make-point 600 4000)
                                                               :focus nil
                                                               :object self 
                                                               :container (panel self)
                                                               :font *om-default-font2*
                                                       ;:after-fun 
                                                               ))   
                          
                          
                     )
    ;center chord staff
    (score-top-margin (panel (chordobj self)) 6)
      
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;DRAW;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *finger-colors*
  (list 
   ;*om-light-gray-color*
   *om-red-color* *om-blue-color* *om-green-color*
        *om-pink-color* *om-orange-color* *om-light-blue-color*
        *om-yellow-color* *om-salmon-color*))
        
(defvar *violinpict* nil)
(setf *violinpict*
      (om-load-and-store-picture 
       "vl" 'full 
       (namestring (make-pathname :directory 
                                  (append (pathname-directory 
                                           (lib-pathname (find-library "OMFingerings"))) 
                                          (list "resources" "pict"))))))


(defmethod om-draw-contents ((self fingpanel))
  (let* ((editor (editor self))
         (sols (fingerings editor))
         (all (om-get-selected-item (solutions (controlview editor))))
         (indx (om-get-selected-item-index (solutions (controlview editor))))
         (clrindx (if (= 0 indx) *om-black-color* (nth (1- indx) *finger-colors*))))
    

    ;legende
    ;open
    (om-with-focused-view self
      (om-with-line-size 2
        (om-with-fg-color self clrindx
          (om-draw-ellipse 740 680 5 5))
        (om-draw-string 760 685 "Open string")
        )
      (om-draw-rect 698 658 262 100)
      )
    ;stopped
    (om-with-focused-view self
      (om-with-line-size 2
        (om-with-fg-color self clrindx
          (om-draw-ellipse 740 700 5 5)
          (om-fill-ellipse 740 700 5 5)
          )
        (om-draw-string 760 705 "Stopped string")
        ))
    ;harmonic
    (om-with-focused-view self
      (om-with-line-size 2
        (om-with-fg-color self clrindx
          (om-draw-rect (- 740 5) (- 720 5) 10 10 :pensize 2))
        (om-draw-string 760 725 "Natural harmonic")
        ))
    
    
;picture
    (om-with-focused-view
        (om-with-fg-color self *om-dark-gray-color*
          (om-draw-picture self *violinpict* 
                           :pos (om-make-point 640 -185)
                           :size (om-make-point 380 800)
                           )))

    (om-with-focused-view self
      ;cordes
      (om-with-line-size 4
        (loop for n from 1 to 4 
              for i = 0  then (+ i 20)   
              do  (om-draw-line (+ i 800) 60 (+ i 800) 600))         

        ;(om-draw-rect 705 40 250 600)
        (om-draw-rect 698 58 262 556)
        ))
    ;frettes
    (om-with-focused-view self
      (om-with-line-size 2
        (loop for n from 0 to 24 ;viola & cello = 18
              for i = 0  then (+ i 20)         
              do (om-draw-line 800 (+ i 60) 860 (+ i 60))
                 )))
    ;;;;;
    ;;doigtes
    (when sols
      (if (equal  all "All")
          (loop for i in sols
                for n from 1 to (length sols)
                do (loop for s in i
                         do (draw-a-finger self s (1- n))))
        (loop for s in (nth (1- indx) sols)
              do (draw-a-finger self s (1- indx)))
        ))))



(defmethod draw-a-finger ((self fingpanel) lst n)
  (let* ((strg (car lst))
         (pos (third lst))
         (harm (fourth lst)))
    (cond
     ;harmonic
     (harm
     (om-with-focused-view self
      (om-with-fg-color self (nth n *finger-colors*)
        (om-draw-rect (- (+ 800 (* strg 20)) 5) (- (+ 60 (* pos 20)) 5) 10 10 :pensize 2)
        ;(om-fill-rect (- (+ 800 (* strg 20)) 5) (- (+ 60 (* pos 20)) 5) 10 10)
         )))
     ;open string
     ((= pos 0)
      (om-with-focused-view self
        (om-with-line-size 2
          (om-with-fg-color self (nth n *finger-colors*)
            (om-draw-ellipse (+ 800 (* strg 20)) (+ 60 (* pos 20)) 5 5)))))
     ;stopped string
     (t (om-with-focused-view self
        (om-with-line-size 2
          (om-with-fg-color self (nth n *finger-colors*)
            (om-draw-ellipse (+ 800 (* strg 20)) (+ 60 (* pos 20)) 5 5)
            (om-fill-ellipse  (+ 800 (* strg 20)) (+ 60 (* pos 20)) 5 5)
            )))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-subviews ((self fingeditor)) ;(om-inspect self)
  (let ((pict (thedata (object self)))
        (vw (w self)) (vh (- (h self) 40)))
    (if pict
      (let ((pw (om-pict-width pict)) (ph (om-pict-height pict)))
        (if (<= (* vw (/ ph pw)) vh)
            (let ((vh2 (* vw (/ ph pw))))
              (om-set-view-size (panel self) (om-make-point vw vh2))
              (om-set-view-position (panel self) (om-make-point 0 (- (/ vh 2) (/ vh2 2)))))
          (let ((vw2 (* vh (/ pw ph))))
            (om-set-view-size (panel self) (om-make-point vw2 vh))
            (om-set-view-position (panel self) (om-make-point (- (/ vw 2) (/ vw2 2)) 0)))))
      (progn
        (om-set-view-size (panel self) (om-make-point vw vh))
        (om-set-view-position (panel self) (om-make-point 0 0))))
    (om-set-view-size (controlview self) (om-make-point (w self) 40))
    (om-set-view-position (controlview self) (om-make-point 0 (- (h self) 40)))))



(defmethod handle-key-event ((self fingpanel) key) 
  (let ((chrd (panel (chordobj (editor self)))))
    ;(print (list "handle" self (editor self) (panel (chordobj (editor self))) key))
    (cond ((and (equal (mode (editor self)) :text) *draw-text*)
           (cond ((equal key :om-key-delete)
                  (unless (= 0 (length (cadr *draw-text*))) 
                    (setf (cadr *draw-text*) (subseq (cadr *draw-text*) 0 (- (length (cadr *draw-text*)) 1))))
                  (report-modifications (editor self)))
                 ((equal key :om-key-return)
                ;(setf (cadr *draw-text*) (concatenate 'string (cadr *draw-text*) (string #\Newline)))
                  (finish-text-extra self)
                  (report-modifications (editor self)))
                 ((characterp key) ;;; + eviter les caracteres speciaux !!
                  (setf (cadr *draw-text*) (concatenate 'string (cadr *draw-text*) (string key)))
                  (report-modifications (editor self)))
                 (t nil)))
          (t (case key 
               (#\h (show-help-window "Fingering Editor commands..." (get-help-list (editor self))))
               (:om-key-tab (change-obj-mode chrd 1))
               ;;;;chordpanel
                 ((equal char :om-key-up)
                  (move-selection chrd 0)
                  (update-panel chrd t)
                  (report-modifications self)
                  )
                 ((equal char :om-key-down)
                  (move-selection chrd 1)
                  (update-panel chrd t)
                  (report-modifications self)
                  )
                 ;;;;
               (:om-key-delete 
                (delete-selection chrd)
                (report-modifications self)
                ))))
    (report-modifications self)
    (call-next-method)
    (om-invalidate-view self)))
        
;update-panel
(defmethod report-modifications ((self fingpanel)) 
  (report-modifications (om-view-container self)))

(defmethod report-modifications ((self fingeditor))
  (let* ((midics (lmidic (object (chordobj self))))
         (inst (instrument self))
         (instr (cond 
                 ((equal inst "Viola") 1)
                 ((equal inst "Cello") 2)
                 (t 0)))
         (fings (fingerings::get-fingerings midics instr :natural-harmonics (harm (controlview self))))
         (res (fingerings::finger-pretty-print fings instr)))
    (setf (chord (object self)) (object (chordobj self)))
    (setf (fingerings self) fings)
    (om-set-dialog-item-text (numsol (controlview self)) (format nil "~D" (length fings))) 
    (setf (notes (object self)) midics)
    (setf (solutions (object self)) fings)
    (om-set-dialog-item-text (textobj self) 
                             (format nil "~A"  res))
    (when fings
      (om-set-item-list (solutions (controlview self))
                        (let ((res '("All")))
                          (loop for i from 1 to (length (fingerings self))
                                collect (push (format nil "~S" i) res))
                          (reverse res))))
    ))

;fingerings::get-fingerings


(defmethod om-view-click-handler ((self fingpanel) pos) ;(print (list "clic" self pos))
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (unless (equal (mode (editor self)) :polygon) (setf *draw-polyg* nil))
  (case (mode (editor self))
    (:normal (call-next-method)) ;(move-pict-object self pos))
    (:pen (add-pen-extra self pos))
    (:line (add-line-extra self pos))
    (:arrow (add-fleche-extra self pos))
    (:rect (add-rect-extra self pos))
    (:ellipse (add-cerc-extra self pos))
    (:polygon (polygon-extra-clic self pos))
    (:text (text-extra-clic self pos))
    (otherwise t))
  (report-modifications self)
  (om-invalidate-view self))

(defmethod om-click-motion-handler ((self fingpanel) pos)
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (when (and (equal (mode (editor self)) :pen) *draw-pen*)
    (let ((pt (list (/ (om-point-h pos) (w self)) (/ (om-point-v pos) (h self))))
          (lastpt (car (last *draw-pen*))))
          (unless (and (= (car lastpt) (car pt)) (= (cadr pt) (cadr lastpt)))
            (pushr pt *draw-pen*)))
    (om-invalidate-view self)))



(defmethod om-click-release-handler ((self fingpanel) pos)
  (unless (equal (mode (editor self)) :normal) (setf (selection (editor self)) nil))
  (when (and (equal (mode (editor self)) :pen) *draw-pen*)
    (let ((ctrl (controlview (editor self))))
      (pushr (list 'pen 
                  (copy-list *draw-pen*)
                  (list (currentcolor ctrl) (currentsize ctrl)
                        (if (equal 'dash (currentline ctrl)) (list (* 2 (currentsize ctrl)) (* 2(currentsize ctrl))) (currentline ctrl))
                        (currentfill ctrl))
                  nil)
            (extraobjs (object (editor self))))))
  (setf *draw-pen* nil)
  (report-modifications (editor self))
  (om-invalidate-view self))

;peut-etre pas terrible
(defmethod om-view-click-handler ((self chordpanel) pos) 
  (when (typep (om-view-container
               (om-view-container self)) 'fingpanel)
    (report-modifications (om-view-container
               (om-view-container self))))
  (call-next-method))


;=====================
(defclass fing-controls (3Dborder-view)  
  ((graphic-controls :initform nil :accessor graphic-controls)
   (harm :initform t :accessor harm)
   (solutions :initform nil :accessor solutions)
   (numsol :initform nil :accessor numsol)
   (color :initform nil :accessor color)
   (instr :initform nil :accessor instr)
   ))


(defclass om-finger-color-view (om-color-view)())
(defmethod om-view-click-handler ((self om-finger-color-view) pos))

(defmethod initialize-instance :after ((self fing-controls) &rest args)
  (let ((graphics-begin 250)
        (editor (om-view-container self)))
    (setf (graphic-controls self)
          (list 
           
           (om-make-dialog-item 'om-static-text 
                                (om-make-point (+ graphics-begin 126) 10)
                                (om-make-point 80 20) "Instrument:" :font *om-default-font1*)
           (setf (instr self) (om-make-dialog-item 'om-pop-up-dialog-item
                                                   (om-make-point (+ graphics-begin 200) 8)
                                                   (om-make-point 80 20)
                                                   "" 
                                                   :font *om-default-font1*
                                                   :range '("Violin" "Viola" "Cello")
                                                   :di-action (om-dialog-item-act item 
                                                                (setf (instrument editor) (om-get-selected-item item))
                                                                (let ((inst (instrument editor))
                                                                      (clef (second (om-subviews (ctr-view (chordobj editor)))))
                                                                      (chrdpanel (panel (chordobj editor))))
                                                                  (cond 
                                                                   ((equal inst "Viola") 
                                                                    (om-set-selected-item-index clef 6)
                                                                    (change-system chrdpanel 'c3))
                                                                   ((equal inst "Cello") 
                                                                    (om-set-selected-item-index clef 0)
                                                                    (change-system chrdpanel 'f))
                                                                   (t (om-set-selected-item-index clef 6)
                                                                      (change-system chrdpanel 'g))
                                                                   )))))
           (om-make-dialog-item 'om-static-text 
                                (om-make-point (+ graphics-begin 300) 10)
                                (om-make-point 120 20) "Natural harmonics:" :font *om-default-font1*)
           
           (om-make-dialog-item 'om-check-box (om-make-point (+ graphics-begin 390) 8) (om-make-point 40 15) "" 
                                          :di-action (om-dialog-item-act item 
                                                       (if (om-checked-p item)
                                                           (setf (harm self) t)
                                                         (setf (harm self) nil)))
                                          :font *controls-font*
                                          :checked-p t
                                          )
           
           (om-make-dialog-item 'om-static-text 
                                (om-make-point (+ graphics-begin 470) 10)
                                (om-make-point 60 20) "Solutions:" :font *om-default-font1*)
           
           (setf (numsol self) (om-make-dialog-item 'om-static-text ;edit-numbox 
                                (om-make-point (+ graphics-begin 540) 8)
                                (om-make-point 40 20)
                                "0" 
                                :font *om-default-font3*
                                                              
                                ))

           (setf (solutions self) (om-make-dialog-item 'om-pop-up-dialog-item
                                                       (om-make-point (+ graphics-begin 580) 8)
                                                       (om-make-point 60 20)
                                                       "" 
                                                       :font *om-default-font1*
                                                       :range '("All")
                                                       :di-action (om-dialog-item-act item 
                                                                    (if (= 0 (om-get-selected-item-index item))
                                                                        (progn 
                                                                          (om-set-bg-color (color self) *om-light-gray-color*)
                                                                          (setf (color (color self)) *om-light-gray-color*))
                                                                      (progn
                                                                        (om-set-bg-color (color self)
                                                                                         (nth (1- (om-get-selected-item-index item)) *finger-colors*))
                                                                        (setf (color (color self))
                                                                              (nth (1- (om-get-selected-item-index item)) *finger-colors*))
                                                                        ;(om-draw-contents (panel editor))
                                                                        (om-invalidate-view (panel editor) t)
                                                                        (om-invalidate-view self t)
                                                                        )))
                                                       ))
           (setf (color self) (om-make-view 'om-finger-color-view 
                                            :position (om-make-point (+ graphics-begin 650) 8) :size (om-make-point 25 25) 
                                            :bg-color *om-light-gray-color* 
                                            :color *om-light-gray-color* 
                                   ;:after-fun #'(lambda (item) (set-pref modulepref :sound-wave-color (color item)))
                                            ))
           ))
    (apply 'om-add-subviews self (append (graphic-controls self)))
    ))


(defmethod om-draw-contents ((self fing-controls))
(om-with-focused-view self
  (om-with-line-size 4
    (om-draw-rect 780 7 30 25))))


