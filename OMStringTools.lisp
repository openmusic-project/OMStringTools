;; ==================================================================================== 
;;                                OMStringTools
;; ==================================================================================== 
;;
;;                                  
;;                          author : Karim Haddad   
;;                     
;;
;This program is free software; you can redistribute it and/or 
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------
(in-package :cl-user)


;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'(lambda (file) 
          (compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "sources")) :name file)))
      '(
        "package"
        "fingerings"
        "instruments"
        "fingeringeditor"
        ))



(in-package :fingerings)
;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library '(
                    (nil  
                     nil nil nil nil)
                    (nil nil (om::fingerboard))
                    ))

;--------------------------------------------------
;doc & info
;--------------------------------------------------
#|
(doc-library "omFingerings, a library for editing and printing score and shapes.
 ---- TO DO ----
" 
             (find-library "omstringtools"))

; (gen-lib-reference (find-library "omscoretools"))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(set-lib-release 0.1) 
|#

(print "
;;;============================================================                                
;;               OMStringTools 
;;      author : Karim Haddad 
;;      RepMus - IRCAM
;;;============================================================
")

;;; (gen-lib-reference (find-library "omscholar"))



