;; A brainfuck interpreter in lisp

;(load "~/projects/cl-yacc/yacc.fas")

(use-package '#:yacc)

; Lexer - consume from list

(defun bf-lexer-list (list)
  #'(lambda ()
      (let ((value (pop list))) 
	(if (null value)
	    (values nil nil)
	    (let ((terminal 
		   (cond 
		     ((member value '(> < + - |.| |,| [ ])) value) ; Return value itself
		     (t 'ignore))))
	      (values terminal value))))))

; Grammar 

; expression := <
; expression := >
; expression := +
; expression := -
; expression := .
; expression := ,
; expression := [
; expression := ]
; expression := expression expression?

(define-parser *bf-parser*
    (:start-symbol expressions)
  (:terminals (< > + - |.| |,| [ ] 'ignore))
  
  (expressions
   expression (expression expressions)
   )

  (expression
   <
   >
   +
   -
   |.|
   |,|
   [
   ]
   'ignore
   )
)

(defun bf()
  #'(lambda (list)
      (parse-with-lexer (bf-lexer-list list) *bf-parser*)))