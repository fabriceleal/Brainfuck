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

;(defun bf-flatten (a b)
;  (append a b))

;; Drop 1st and last elements of a list

(defun aux-copy2-1 (list)
  (if (eql (list-length list) 1) 
      '()
      (cons (car list) (aux-copy2-1 (cdr list)))
      ))

(defun copy2-1(list)
   (aux-copy (cdr list)))

;; Create an entry in the AST for a loop

(defun bf-loop (&rest args)
  ; Drop 1st and last, the [ and ]
  (list (cons 'loop (copy2-1 args))) )

;; Parser

(define-parser *bf-parser*
    (:start-symbol expressions)
  (:terminals (< > + - |.| |,| [ ] 'ignore))
  
  (expressions
   expression (expression expressions #'append)
   )

  (expression
   (term #'list)
   ([ expressions ] #'bf-loop)
   )

  (term
   <
   >
   +
   -
   |.|
   |,|
   'ignore
   )
)

(defparameter *mem-size* 30000)


; Create a closured environment for manipulating a vm
(defun make-bf-vm () 
  (let* ((mem (make-array *mem-size* :element-type 'integer :initial-element 0)) (i 0))
    (labels ((manipulator (command)
	     (cond 
	       ;; Pointer manipulation
	       ((eql command '>) (setf i (+ i 1)) (values command i))
	       ((eql command '<) (setf i (- i 1)) (values command i))

	       ;; Manipulate memory
	       ((eql command '+) (values command (setf (aref mem i) (+ (aref mem i) 1))))
	       ((eql command '-) (values command (setf (aref mem i) (- (aref mem i) 1))))

	       ;; output
	       ((eql command '|.|) (format t "~c" (code-char (aref mem i))) (values command t))
	    
	       ;; input and store
	       ((eql command '|,|) nil)

	       ((and (listp command) (eql (car command) 'LOOP)) 
	       ; Evaluate commands in (cdr command) until (eql (aref mem i) 0)
		(let ((commands (cdr command)) (it 0))
		  (loop for it from 1 to 999999 until (eql (aref mem i) 0)
		    do (mapcar #'manipulator commands))
		  (values command it)))

	       ; Asks for the closured values
	       ((eql command '?i) (values command i))
	       ((eql command '?mem) (values command mem))
	       ((eql command '?cur) (values command (aref mem i)))


	       (t (values command nil)))) )

      #'manipulator) ))

; Generates a brainfuck lexer-parser for lists of instructions
(defun make-bf()
  #'(lambda (list)
      (parse-with-lexer (bf-lexer-list list) *bf-parser*)))

(defvar *bf* (make-bf))
(defvar *bf-vm* (make-bf-vm))
