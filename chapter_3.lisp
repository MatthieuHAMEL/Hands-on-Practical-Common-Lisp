;;;; Chapter 3 (Practical project : a CD database)

;; 1 CD == title, artist, rating, flag (ripped/not ripped)

;; This is how to declare a list :
;;> (list 1 2 3)
;; (1 2 3)

;; This is a property list (plist) using keyword symbols :
;; (list :a 1 :b 2 :c 3) ; :a, :b and :c are keyword symbols

;; The function GETF : (plist, symbol) -> value following the symbol in the plist.

;; Function taking the 4 characteristics of a CD and building the corresponding
;; plist :

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
;;> (make-cd "a" "b" "c" "d")
;; (:TITLE "a" :ARTIST "b" :RATING "c" :RIPPED "d")

;; A global variable called *db* (that notation is a convention)
(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(add-record (make-cd "Roses 2" "Kathy Mattea" 7 t))

(defun dump-db ()
  (dolist (cd *db*) ; like a foreach
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; Explanation
;; ~ : begin format directive
;; ~a : consume one element (of cd) so in our case a keyboard symbol, then a value
;; ~10t : tabulating up to 10 spaces before printing anything else
;; ~{ ... } : format a list, so the next argument must be a list (here cd).
;; This way the formatter iterates over the list elements
;; ~% : Newline.

(defun dump-db-oneliner () ; knowing that db is a list of lists ... 
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

