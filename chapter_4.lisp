;;;; Chapter 4 : Syntax & semantics

;;; S-Expressions
;; -> Lists and atoms : lists are delimited by parentheses ( ) and atoms are the rest.
;; Elements of the lists are S-Expressions themselves.

;; So the syntax is very simple and pure. We just have to know the different types of atoms :

;; 1. Numbers
123
3/7
1.0
1.0d0 ; double precision
1.0e-4
-42
246/2 ; It is an integer ! ...

;; 2. Strings
"foo"     ; the string containing the characters f, o, and o.
"fo\o"    ; the same string
"fo\\o"   ; the string containing the characters f, o, \, and o.
"fo\"o"   ; the string containing the characters f, o, ", and o.

;; 3. Symbols : all non-double-quote strings that are interpreted
;; as objects by the evaluator, e.g. hello-world, format, *db*, ...
;; (handled case-insensitively!)


;; x                 ; the symbol X
;;()                 ; the empty list
;;(1 2 3)            ; a list of three numbers
;;("foo" "bar")      ; a list of two strings
;;(x y z)            ; a list of three symbols
;;(x 1 "foo")        ; a list of a symbol, a number, and a string
;; (+ (* 2 3) 4)     ; a list of a symbol, a list, and a number.




