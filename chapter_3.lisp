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

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*) ; Don't wait for a newline to print that 
  (read-line *query-io*))

;; Example :
;;> (prompt-read "Title")
;;"Title: <user prompt>"
;; It returns the user prompt

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0) ;That last param is just to avoid errors 
   (y-or-n-p "Ripped"))) ; will be T or NIL ... 

;;(add-record (prompt-for-cd))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
    (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename)
  (with-open-file (out filename ; Opens a file and bind the stream to the out variable
                       :direction :output ; parameters of the with-open-file function
                       :if-exists :supersede)
    (with-standard-io-syntax ; Expressions executed by (with-open-file) ; they can refer to that out variable
      (print *db* out)))) ; That format produced by print is able to be read back by Lisp ...


(defun load-db (filename)
  (with-open-file (in filename) ; :direction will be input by default 
    (with-standard-io-syntax
      (setf *db* (read in))))) ; /!\ Warning: It empties *db*

;; Querying the database 

;; remove-if-not : "removing" elements satisfying a predicate (creates a new list in fact)
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10)) ; #'evenp to designate a function
;; (2 4 6 8 10)

(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10)) ; With an anonymous function (lambda keyword)

(remove-if-not
 #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

;; Take the name of the artist as an argument (I tried this without looking at the solution) :
(defun select-by-artist (artist)
  (remove-if-not #'(lambda (cd) (equal (getf cd :artist) artist)) *db*))

;; Generic version
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun select-by-artist-bis (artist)
  (select #'(lambda (cd) (equal (getf cd :artist) artist))))

;; We can abstract things a little more by defining that lambda as an "artist-selector":

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

;; E.g.
(select (artist-selector "Dixie Chicks"))

;;; Keyword parameters
;; This is a function that takes 3 parameters 
(defun foo (a b c) (list a b c))

;; The following takes 3 parameters whose keys must be :a, :b and :c
;; It produces a list of the corresponding values with NIL if the
;; key is not given. So it is like taking a variable number of arguments ...
(defun foo2 (&key a b c) (list a b c))

;; I can also give a list of 2 or 3 elelements instead of those a b c params :
(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))

;; Here (b 20) means "default value of :b will be 20 so if you don't give it, the
;; resulting list will still have 20 as its second element

;; In (c 30 c-p), the c-p boolean parameter is a "supplied-p" parameter, boolean,
;; and will let us know if the :c parameter was supplied or not.

;; (foo :a 1 :b 2 :c 3)  ==> (1 2 3 T)
;; (foo :c 3 :b 2 :a 1)  ==> (1 2 3 T)
;; (foo :a 1 :c 3)       ==> (1 20 3 T)
;; (foo)                 ==> (NIL 20 30 NIL)


;; E.g
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(select (where :rating 10 :ripped nil))







