#!/usr/bin/clisp

;; A brainfuck interpreter in lisp

(load "~/projects/cl-yacc/yacc")

(use-package '#:yacc)

;; Lexer - consume from list

(defun bf-lexer-list (list)
  #'(lambda ()
      (let ((value (pop list))) 
	(if (null value)
	    (values nil nil)
	    (let ((terminal 
		   (cond 
		     ((member value '(> < + - |.| |,| [ ])) value)
		     (t :ignore))))
	      (values terminal value))))))

(defun bf-lexer-file (&optional (stream *standard-input*))
      (loop 
	 (let ((c (read-char stream nil nil)))
	   (let ((terminal 
		  (cond 
		    ((member c '(nil)) 
		     (return-from bf-lexer-file (values nil nil)))

		    ((eql c #\<) '<)
		    ((eql c #\>) '>)
		    ((eql c #\+) '+)
		    ((eql c #\-) '-)
		    ((eql c #\.) '|.|)
		    ((eql c #\,) '|,|)
		    ((eql c #\[) '[)
		    ((eql c #\]) '])
		    
		    (t :ignore) )))
	     (return-from bf-lexer-file (values terminal terminal)) )
      ))) 

;; Grammar 

;; expression := <
;; expression := >
;; expression := +
;; expression := -
;; expression := .
;; expression := ,
;; expression := [
;; expression := ]
;; expression := expression expression?


;; Drop 1st and last elements of a list

(defun aux-copy2-1 (list)
  (if (eql (list-length list) 1) 
      '()
      (cons (car list) (aux-copy2-1 (cdr list)))
      ))

(defun copy2-1(list)
   (aux-copy2-1 (cdr list)))

;; Create an entry in the AST for a loop

(defun bf-loop (&rest args)
  ;; Drop 1st and last, the [ and ]
  (list (cons 'loop (copy2-1 args))) )

;; Parser

(define-parser *bf-parser*
    (:start-symbol expressions)
  (:terminals (< > + - |.| |,| [ ] :ignore))
  
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
   :ignore
   )
)

(defparameter *mem-size* 30000)

;; Create a closured environment for manipulating a vm
(defun make-bf-vm () 
  (let* ((mem (make-array *mem-size* :element-type 'integer :initial-element 0)) (i 0) (nbr 0))
    (labels ((manipulator (command)
	     (cond 

	       ;; So we dont waste too much time with pointless comparisions
	       ((eql command :ignore) (values command nil))

	       ;; Pointer manipulation
	       ((eql command '>) (setf i (+ i 1)) (values command i))
	       ((eql command '<) (setf i (- i 1)) (values command i))

	       ;; Manipulate memory
	       ((eql command '+) (values command (setf (aref mem i) (+ (aref mem i) 1))))
	       ((eql command '-) (values command (setf (aref mem i) (- (aref mem i) 1))))

	       ;; output
	       ((eql command '|.|) (format t "~c" (code-char (aref mem i))) (values command t))

	       ;; TODO input and store
	       ((eql command '|,|) nil)

	       ;; Evaluate commands in (cadr command) until (eql (aref mem i) 0)
	       ((and (listp command) (eql (car command) 'LOOP)) 
		;; cdr of command will be a list; take its car
		(let ((commands (cadr command)) (itr 0))

		     (loop until (eql (aref mem i) 0)
			do (mapc #'manipulator commands))

		     (values command 'looped)
		  ))

	       ;; This is silly, but I'll keep it
	       ((and (listp command) (eql (car command) 'BUNCH))
		(let ((commands (cdr command)))
		  (mapc #'manipulator commands)
		  (values command 'bunched)))
	       
	       ;; Asks for the closured values (debug commands)
	       ((eql command '?i) (values command i))
	       ((eql command '?mem) (values command mem))
	       ((eql command '?cur) (values command (aref mem i)))

	       ;; Bah! Feed me with valid commands, you douche!
	       (t (values command nil)))) )

      #'manipulator) ))

;; Generates a brainfuck lexer-parser for lists of instructions
(defun make-ls-bf()
  #'(lambda (list)
      (parse-with-lexer (bf-lexer-list list) *bf-parser*)))

;; Interpret brainfuck source file
;; Output to default stream
(defun bf-interpret-file (filename)
  ;; Create brainfuck vm
  (let ((vm (make-bf-vm)) (stream nil))

    ;; Redirect input, read from file
    (with-open-file (*standard-input* filename :direction :input)

      ;; Feed parser with result of lexer, fed with *standard-input*
      (mapc vm (parse-with-lexer #'bf-lexer-file *bf-parser*))

      ;; We dont care about the result of the application
      t)))


(bf-interpret-file (car *args*))