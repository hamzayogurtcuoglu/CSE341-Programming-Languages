; *********************************************
; *  341 Programming Languages                *
; *  31 December 2018                         *
; *  Project2 => G++ Parser                   *
; *  Author: Hamza Yoğurtcuoğlu               *
; *********************************************

;Parser Function Take File Name
;Completely ------Recursive------ Working 

; 1 )EACH KEYWORD IS TAKEN RECURSIVELY
; 2 )DERIVATION DONE
; 3 )PARSETREE IS CREATED ACCORDING TO BNF FORM

;GETTING FILE NAME FOR KEYWORDS
;-------------------------------------------------------------
;START -> INPUT
;INPUT -> EXPI | EXPLISTI

;EXPI -> (set ID EXPI)
;EXPI -> (+ EXPI EXPI)
;EXPI -> (- EXPI EXPI)
;EXPI -> (* EXPI EXPI)
;EXPI -> (/ EXPI EXPI)
;EXPI -> ID | (ID EXPLISTI) | VALUES
;EXPI -> (deffun ID IDLIST EXPLISTI)
;EXPI -> (ID EXPLISTI)
;EXPI -> (defvar ID EXPI)
;EXPI -> (set ID EXPI)
;EXPI -> (if EXPB EXPLISTI)
;EXPI -> (if EXPB EXPLISTI EXPLISTI)
;EXPI -> (while (EXPB) EXPLISTI)
;EXPI -> (for (ID EXPI EXPI) EXPLISTI)

;EXPB -> (and EXPB EXPB)
;EXPB -> (or EXPB EXPB)
;EXPB -> (not EXPB)
;EXPB -> (equal EXPB EXPB)
;EXPB -> (equal EXPI EXPI)
;EXPB -> BinaryValue

;EXPLISTI -> (concat EXPLISTI EXPLISTI) | (append EXPI EXPLISTI) | null | ‘( VALUES ) | ‘()
;VALUES -> VALUES IntegerValue | IntegerValue
;IDLIST -> ID | (IDLIST) | ID IDLIST

(defun parser(filename)

	(setq parserTree '())   	
	 
	(setq file_Content filename)

	(setq parserTree (newLine parserTree) )

	(setq parserTree(concatenate 'string parserTree "START") )
	
	(setq parserTree (newLine parserTree) )
	
	(setq parserTree(concatenate 'string parserTree " INPUT") )

	(setq parserTree (mainParser  file_Content parserTree 2 )) ;main node start in this callee

	(save-tree-data file_Content parserTree)

	(format t "~a~%" "Completely------Recursive------ Working")

	(format t "~a" "-------171044086.tree File Saved-----.")
)

(defun mainParser(file_Content parserTree spaceNumber)
			
	; two space reducing for ) operator
	(if (equal (car file_Content) '("operator" ")"))
			(setq spaceNumber(- spaceNumber 2))

	)
    ; addition newline for ( operator
	(if (or (equal (car file_Content) '("operator" "(")) (equal (first (car file_Content)) "integer" )
    		(and (equal (first (car file_Content)) "identifier" ) (not(equal (car (cdr file_Content)) '("operator" "(")))))
		
    	(if (not(null file_Content))
			(setq parserTree (newLine parserTree) )
		)
    )
	; addition space for ( operator
    (if (or (equal (car file_Content) '("operator" "(")) (equal (first (car file_Content)) "integer" )
    		(and (equal (first (car file_Content)) "identifier" ) (not(equal (car (cdr file_Content)) '("operator" "(")))))

    	(if (not(null file_Content))
			(setq parserTree(treeSpace parserTree spaceNumber))
		)
    )
			
	; Main Node Finding +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	(if (not(null file_Content))
		(cond ((equal (car file_Content) '("operator" "("))  (setq parserTree (paranthesisControl file_Content parserTree spaceNumber )))	  	  
			  ((equal (first (car file_Content)) "integer" ) (setq parserTree(concatenate 'string parserTree "EXPI")))	
			  ((and (equal (first (car file_Content)) "identifier" ) (not(equal (car (cdr file_Content)) '("operator" "(")))) 
			  	(setq parserTree(concatenate 'string parserTree "IDLIST")))  	
		)
	)
	(if (or (null file_Content) (equal (first (car file_Content)) "integer" ) )
		(setq parserTree (newLine parserTree) )  ;--------------------------------------
	)	
	
	(if (equal (first (car file_Content)) "integer" )
		(setq spaceNumber(spaceControl file_Content spaceNumber)) ;---------------------------	
	)

	(if (equal (first (car file_Content)) "integer" )
			(setq parserTree(treeSpace parserTree spaceNumber)) ;---------------------------------
	)
	
	(if (equal (first (car file_Content)) "integer" )
			(setq parserTree(concatenate 'string parserTree "IntegerValue"))  
	)
;---------------------------------------------------------------------------------
	(if (not(or (null file_Content) (equal (first (car file_Content)) "keyword")(equal (first (car file_Content)) "operator") ))
		(setq parserTree (newLine parserTree) )  ;--------------------------------------
	)	
	
	(setq spaceNumber(spaceControl file_Content spaceNumber)) ;---------------------------

	(if (not(or (null file_Content) (equal (first (car file_Content)) "keyword")(equal (first (car file_Content)) "operator") ))
		(setq parserTree(treeSpace parserTree spaceNumber)) ;---------------------------------
	)
	;Subnode Finding ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	(if (not(null file_Content))
		(cond 
			  ((equal (first (car file_Content)) "identifier" ) (setq parserTree(concatenate 'string parserTree "ID")))	
			  ((equal (first (car file_Content)) "integer" ) (setq parserTree(concatenate 'string parserTree "VALUES")))		
			  ((equal (second (car file_Content)) "true" ) (setq parserTree(concatenate 'string parserTree "BINARYVALUE")))
			  ((equal (second (car file_Content)) "false" ) (setq parserTree(concatenate 'string parserTree "BINARYVALUE")))	
			  ((equal (second (car file_Content)) "null" ) (setq parserTree(concatenate 'string parserTree "VALUES")))	
			  ((equal (second (car file_Content)) "'()" ) (setq parserTree(concatenate 'string parserTree "LISTVALUE")))	
		)
	)

  	(if (not(null file_Content))
		(setq parserTree (newLine parserTree) )  ;--------------------------------------
	)	
	
	(setq spaceNumber(spaceControl  file_Content spaceNumber)) ;---------------------------

	(if (and (equal (first (car file_Content)) "identifier" ) 
		(equal (car (cdr file_Content)) '("operator" "(")))
				
		(setq spaceNumber(+ spaceNumber 1))
	)	

	(if (not(null file_Content))
		(setq parserTree(treeSpace parserTree spaceNumber)) ;---------------------------------
	)

  	;Finding Leaf +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	(if (not(null file_Content))
		(cond ((equal (car file_Content) '("operator" "(")) (setq parserTree(concatenate 'string parserTree "(")))	  
			  ((equal (car file_Content) '("operator" ")")) (setq parserTree(concatenate 'string parserTree ")")))  	
			  ((equal (second (car file_Content)) "set" ) (setq parserTree(concatenate 'string parserTree "set")))	
			  ((equal (second (car file_Content)) "+" ) (setq parserTree(concatenate 'string parserTree "+")))		
			  ((equal (second (car file_Content)) "-" ) (setq parserTree(concatenate 'string parserTree "-")))	
			  ((equal (second (car file_Content)) "*" ) (setq parserTree(concatenate 'string parserTree "*")))	
			  ((equal (second (car file_Content)) "/" ) (setq parserTree(concatenate 'string parserTree "/")))
			  ((equal (first (car file_Content)) "identifier" ) (setq parserTree(concatenate 'string parserTree (second (car file_Content)) )))		
			  ((equal (second (car file_Content)) "deffun" ) (setq parserTree(concatenate 'string parserTree "deffun")))	
			  ((equal (first (car file_Content)) "integer" ) (setq parserTree(concatenate 'string parserTree (second (car file_Content)) )))	
			  ((equal (second (car file_Content)) "defvar" ) (setq parserTree(concatenate 'string parserTree "defvar")))	
			  ((equal (second (car file_Content)) "if" ) (setq parserTree(concatenate 'string parserTree "if")))
			  ((equal (second (car file_Content)) "while" ) (setq parserTree(concatenate 'string parserTree "while")))	
			  ((equal (second (car file_Content)) "for" ) (setq parserTree(concatenate 'string parserTree "for")))	
			  ((equal (second (car file_Content)) "and" ) (setq parserTree(concatenate 'string parserTree "and")))	
			  ((equal (second (car file_Content)) "or" ) (setq parserTree(concatenate 'string parserTree "or")))	
			  ((equal (second (car file_Content)) "not" ) (setq parserTree(concatenate 'string parserTree "not")))	
			  ((equal (second (car file_Content)) "equal" ) (setq parserTree(concatenate 'string parserTree "equal")))	
			  ((equal (second (car file_Content)) "true" ) (setq parserTree(concatenate 'string parserTree "true")))
			  ((equal (second (car file_Content)) "false" ) (setq parserTree(concatenate 'string parserTree "false")))	
			  ((equal (second (car file_Content)) "null" ) (setq parserTree(concatenate 'string parserTree "null")))
			  ((equal (second (car file_Content)) "append" ) (setq parserTree(concatenate 'string parserTree "append")))
			  ((equal (second (car file_Content)) "concat" ) (setq parserTree(concatenate 'string parserTree "concat")))	
			  ((equal (second (car file_Content)) "'" ) (setq parserTree(concatenate 'string parserTree "'")))	
			  ((equal (second (car file_Content)) "'()" ) (setq parserTree(concatenate 'string parserTree "'()")))		
		)
		
	)		

	(if (and (equal (first (car file_Content)) "identifier" ) 
		(equal (car (cdr file_Content)) '("operator" "(")))
				
		(setq spaceNumber(- spaceNumber 1))
	)		
	;2 subtracting for ) operator end of operation in spaceNumber
	(if (or (equal (car file_Content) '("operator" ")")) (equal (first (car file_Content)) "integer" )(and (equal (first (car file_Content)) "identifier" ) 
		(not(equal (car (cdr file_Content)) '("operator" "(")))) )
			(setq spaceNumber(- spaceNumber 2))

	)
	(if (equal (first (car file_Content)) "integer" )
			(setq spaceNumber(- spaceNumber 1))		  
	)
	;Recursive situation all the time working to null file_Content(in end of Keyword List)
	(if (not(null file_Content))
		(setq parserTree (mainParser (cdr file_Content) parserTree spaceNumber ))
		parserTree
	)
	
)

;Begin of paranthesis is controlled for up-node
(defun paranthesisControl(file_Content parserTree spaceNumber)

	(if  (not(or (equal (second (first (cdr file_Content))) "append" ) (equal (second (first (cdr file_Content))) "concat") 
		 (equal (second (first  (cdr file_Content )) ) "'()") (equal (first (first (cdr file_Content))) "integer" ) 
		 (equal (second (first  (cdr file_Content) )) "null" ) (equal (second (first  (cdr file_Content) )) "'" ) ))
		
		 (if (not (or(equal (second (first (cdr file_Content))) "and" ) (equal (second (first (cdr file_Content))) "not" ) 
		 	         (equal (second (first (cdr file_Content))) "or" ) (equal (second (first (cdr file_Content))) "equal" )
		 			 (equal (second (first (cdr file_Content))) "false" ) (equal (second (first (cdr file_Content))) "true" ) 
		 	))
		 		
		 	(if (equal (first (car (cdr file_Content ))) "identifier" )

		 		(setq parserTree(concatenate 'string parserTree "IDLIST"))	
		 		(setq parserTree(concatenate 'string parserTree "EXPI"))							 	
		 	)	
		 	
		 	(setq parserTree(concatenate 'string parserTree "EXPB"))	
		 )
		 (setq parserTree(concatenate 'string parserTree "EXPLISTI"))
	)
)

;adding to spaceNumber in this function
(defun spaceControl(file_Content spaceNumber)

	(if (not(null file_Content))
		(cond ((equal (car file_Content) '("operator" "(")) (setq spaceNumber(+ spaceNumber 1)) )	  
			  ((equal (car file_Content) '("operator" ")")) (setq spaceNumber(+ spaceNumber 1)) )	
			  ((equal (first (car file_Content)) "integer" )  (setq spaceNumber(+ spaceNumber 1)) )	
			   ((and (equal (first (car file_Content)) "identifier" ) (not(equal (car (cdr file_Content)) '("operator" "("))))
			    (setq spaceNumber(+ spaceNumber 1)) )	
		)
	)
	spaceNumber
)

;putting space in parserTree for each node
(defun treeSpace(parserTree spaceNumber)

	(if (> spaceNumber 0)
		(setq parserTree(concatenate 'string parserTree " "))
	)
	(if (> spaceNumber 0)
		(setq parserTree(treeSpace parserTree (- spaceNumber 1)))
	)
	(setq parserTree(concatenate 'string parserTree ""))
)

;getting newline for each keyword
(defun newLine (parserTree)

	(setq parserTree(concatenate 'string parserTree "
") )

)

;writing to file 171044086.tree string tree
(defun save-tree-data (file_Content file-trees)
	(with-open-file (stream "171044086.tree" :direction :output)
 	
	(format stream "~a" ";DIRECTIVE: parse tree")	
	(format stream "~a" file-trees)
	)

)
;concatenate string that means keyword.
(defun concatString (list)
  "A recursive function that concatenates a list of strings."
  (if (listp list)
      (with-output-to-string (s)
         (dolist (item list)
           (if (stringp item)
             (format s "~a" item))))))
