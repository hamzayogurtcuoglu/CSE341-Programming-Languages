; *********************************************
; *  341 Programming Languages                *
; *  26 November 2018                         *
; *  Project1 => G++ Lexer                    *
; *  Author: Hamza Yoğurtcuoğlu               *
; *********************************************


;Lexer Function Take File Name

;OPERATOR LIST
(setf OPERATORS #(  #\+  #\-  #\/   "**"  #\*  ))

;KEYWORD LIST
(setf KEYWORDS #( "and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit"))


(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents))
)

;Digit Control
(defun digit_or_not(digit)
	(if (and (> (char-int digit) 47) (< (char-int digit) 58))
		t
 		nil
 	)	
)

;Convert String to List
(defun str-list(x)
	(with-input-from-string (s x)
   (let ((r nil))
      (do ((line (read s nil 'eof)
                (read s nil 'eof)))
          ((eql line 'eof))
          (push line r)
      )
   (reverse r)  ))
)


(defun finding_token(stringlist)

	;DFA or regular expression reader.

	(setq stringlist(remove #\Space stringlist))
	(setq stringlist(remove #\NewLine stringlist))
	(setq stringlist(remove #\Tab stringlist))

	(setf tokens '())
	(defparameter *my-function* stringlist )
	(setq size (length stringlist))
	(setq index 0)
	(dotimes(index2 size )
			
		(setq first_length (length tokens))
		
		(dotimes(i (length OPERATORS ))
			
			(if (not(equal 1 (- (+ index2 2) index) ))	
				
				(if (equal  "(" (subseq *my-function* index2 (+ index2 1) ))
					
					(if ( <  0 (length(subseq *my-function* index index2  )))
			
						(dotimes(a 1)
							
							(setq integer_id 0)
							(setq valid1 0)	
							(setq valid 0)
					    	(dotimes(b (length KEYWORDS ))
					    		
					    		(if (equal (svref KEYWORDS i) (subseq *my-function* index index2  )  )
					    				(setq valid 1)
					    		)		
					    	)
					    	(dotimes(b (length (subseq *my-function* index index2 ) ))
					    		
					    		(if ( digit_or_not (char (subseq *my-function* index index2  ) b )  )
					    				(setq valid 2)
										(dotimes (f 1)
					    					(setq valid1 1 )
					    					(setq integer_id (+ b 1))
										)						    				
					    		)		
					    		
					    		(if ( or (equal "true"  (subseq *my-function* index index2  )  ) 
					    			(equal "false" (subseq *my-function* index index2  ))
					    			 )
					    				(setq valid 3)
					    		)
					    	)
					    		
					    		
								(if (equal valid 2)
					    			(if (equal valid1 1)
					    				
					    				(dotimes (c 1)
					    					
					    					(setf tokens (cons (append  '(IDENTIFIER -> ) (str-list (subseq *my-function* index (+ index integer_id)  )) )  tokens) ) 
					    					(setf tokens (cons (append  '(INTEGER -> ) (str-list (subseq *my-function* (+ index integer_id) index2  )) )  tokens) ) 
					    				)

					    				(setf tokens (cons (append  '(INTEGER -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 

					    			)		
					    		)
					    		(if (equal valid 3)
					    			
					    			(setf tokens (cons (append  '(BINARYVALUE -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 
					    					
					    		)
								(if (equal valid 0)
					    			
					    			(setf tokens (cons (append  '(IDENTIFIER -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 
					    					
					    		)	

								(setf tokens(cons (append  '(OPERATOR -> ) '("(")   ) tokens ) )

						)		
						
						(setf tokens(cons (append  '(OPERATOR -> ) '("(")  ) tokens ) )					
					
					)
					
					(if (equal  ")" (subseq *my-function* index2 (+ index2 1) ))
					
							(if ( <  0 (length(subseq *my-function* index index2  )))
			
						(dotimes(a 1)
							(setq integer_id 0)
							(setq valid1 0)	
							(setq valid 0)
					    	(dotimes(b (length KEYWORDS ))
					    		
					    		(if (equal (svref KEYWORDS i) (subseq *my-function* index index2  )  )
					    				(setq valid 1)
					    		)		
					    	)
					    	(dotimes(b (length (subseq *my-function* index index2 ) ))
					    		
					    		(if ( digit_or_not (char (subseq *my-function* index index2  ) b )  )
					    				(setq valid 2)
										(dotimes (f 1)
					    					(setq valid1 1 )
					    					(setq integer_id  (+ b 1) )
										)						    				
					    		)		
					    		
					    		(if ( or (equal "true"  (subseq *my-function* index index2  )  ) 
					    			(equal "false" (subseq *my-function* index index2  ))
					    			 )
					    				(setq valid 3)
					    		)
					    	)	
					    		
								(if (equal valid 2)
					    			(if (equal valid1 1)
					    				
					    				(dotimes (c 1)

					    					(setf tokens (cons (append  '(IDENTIFIER -> ) (str-list (subseq *my-function* index  (+ index integer_id)  )) )  tokens) )
					    					(setf tokens (cons (append  '(INTEGER -> ) (str-list (subseq *my-function* (+ index integer_id) index2  )) )  tokens) ) 
					    				)

					    				(setf tokens (cons (append  '(INTEGER -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 

					    			)		
					    		)
					    		(if (equal valid 3)
					    			
					    			(setf tokens (cons (append  '(BINARYVALUE -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 
					    					
					    		)
								(if (equal valid 0)
					    			
					    			(setf tokens (cons (append  '(IDENTIFIER -> ) (str-list (subseq *my-function* index index2  )) )  tokens) ) 
					    					
					    		)	

								(setf tokens(cons (append  '(OPERATOR -> ) '(")")   ) tokens ) )

						)		
						
						(setf tokens(cons (append  '(OPERATOR -> ) '(")")  ) tokens ) )					
					
						)

						(if (equal (string(svref OPERATORS i)) (subseq *my-function* index2 (+ index2 1) ))
							
							(dotimes(k 1 )
									
								(if (equal "**" (subseq *my-function* index2 (+ index2 2)))
										
									(setf tokens(cons (append '(OPERATOR -> ) (str-list (subseq *my-function* index2 (+ index2 2) )) ) tokens ))	
									(setf tokens(cons (append '(OPERATOR -> ) (str-list (subseq *my-function* index2 (+ index2 1) )) ) tokens ))	

								)
								(if (equal "**" (subseq *my-function* index2 (+ index2 2)))
									(setq index2 (+ 1 index2) )		
								)
							)
						)
					)
				)
			)
			
			(if (not(equal first_length (length tokens) ))
				
				(setq index (+ 1 index2) )
			)
			
			(if (not(equal first_length (length tokens) ))
				(setq i (length OPERATORS ) )
			)			
		)

		(setq second_length (length tokens))				

		(dotimes(i (length KEYWORDS ))
			
			(if (equal (svref KEYWORDS i) (subseq *my-function* index (+ index2 1) ))	
				
				(setq tokens(cons (append '(KEYWORDS -> ) (str-list (subseq *my-function* index (+ index2 1) ))) tokens ))
				
			)
			(if (equal (svref KEYWORDS i) (subseq *my-function* index (+ 1 index2) )) 
				
				(setq index (+ 1 index2) )
				
			)
			(if (not(equal second_length (length tokens) ))
				(setq i (length KEYWORDS ) )
			)

		)	
	
	)		
	
	(reverse tokens)
)

;This function take a file name and perform lexical analysis of the program
;contained within this fule. The output of the function should be the tokens in a list.

(defun lexer (filename)
   

	(setq file_Content (file-get-contents filename))

	(setf tokens '())

	(setf tokens (finding_token file_Content))
		
)
