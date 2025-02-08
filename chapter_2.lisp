;;;; Chapter 2 : A tour of the REPL
;;;; I took the "Portacle" emacs distribution, since "Lisp in a Box"
;;;; was a bit outdated.

;;; read-eval-print loop (REPL)

;;> (+ 2 3) ; List of 3 elements : +, 2 and 3. + is evaluated as a function
;;5

;; With a string : 
;; > "hello, world"
;; "hello, world"

;;> (format t "Hello, world!")
;;Hello, world!      ; This is a side effect 
;;NIL                ; This is the return value of format

;; A function whose name is hello-world (case-insensitive),
;; With an empty parameter list () and returning (format t "Hello, World!")
;;> (defun hello-world () (format t "Hello, World!"))
;;HELLO-WORLD

;; The side effect is invisible but it has created a function that we can
;; call :
;;> (hello-world)
;;"Hello, World!"
;;NIL

;; In a LISP file indentation is not necessary but critical for readability
;; It is an opportunity to test the SLIME tool suite 
(defun hello-world ()
  (format t "Hello, World!..."))

;; This is how to compile and load this file in the REPL :
;; (load (compile-file "chapter_2.lisp"))












