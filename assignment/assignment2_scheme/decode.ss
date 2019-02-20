; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2017                                *
; *  Author: Ulrich Kremer                    *
; *********************************************
;; -----------------------------------------------------
;; ENVIRONMENT
;; contains "ltv", "vtl",and "reduce" definitions
(load "include.ss")

;; contains a test document consisting of three paragraphs. 
(load "document.ss")

;; contains a test-dictionary, which has a much smaller dictionary for testing
;; the dictionary is needed for spell checking
(load "test-dictionary.ss")

;; (load "dictionary.ss") ;; the real thing with 45,000 words


;; -----------------------------------------------------
;; HELPER FUNCTIONS
(define list_num_count
  (lambda(p)
    (map num_true
       (list
            (map spell-checker (map (encode-n 0) p))
            (map spell-checker (map (encode-n 1) p))
            (map spell-checker (map (encode-n 2) p))
            (map spell-checker (map (encode-n 3) p))
            (map spell-checker (map (encode-n 4) p))
            (map spell-checker (map (encode-n 5) p))
            (map spell-checker (map (encode-n 6) p))
            (map spell-checker (map (encode-n 7) p))
            (map spell-checker (map (encode-n 8) p))
            (map spell-checker (map (encode-n 9) p))
            (map spell-checker (map (encode-n 10) p))
            (map spell-checker (map (encode-n 11) p))
            (map spell-checker (map (encode-n 12) p))
            (map spell-checker (map (encode-n 13) p))
            (map spell-checker (map (encode-n 14) p))
            (map spell-checker (map (encode-n 15) p))
            (map spell-checker (map (encode-n 16) p))
            (map spell-checker (map (encode-n 17) p))
            (map spell-checker (map (encode-n 18) p))
            (map spell-checker (map (encode-n 19) p))
            (map spell-checker (map (encode-n 20) p))
            (map spell-checker (map (encode-n 21) p))
            (map spell-checker (map (encode-n 22) p))
            (map spell-checker (map (encode-n 23) p))
            (map spell-checker (map (encode-n 24) p))
            (map spell-checker (map (encode-n 25) p))))))

(define num_true
  (lambda(l)
    (reduce + (map (lambda (x) (if (equal? x #t) 1 0)) l) 0 )))

(define find_n
  (lambda(l t)
    (if (null? l) 0
        (if (equal? t (car l))
            0
            (+ 1 (find_n (cdr l) t))))))

(define num_letter
  (lambda (c)
    (lambda(w)
      (reduce + (map (lambda (x) (if (equal? x c) 1 0)) w) 0 ))))


(define list_num_letter
  (lambda(p)
       (list
            (reduce + (map (num_letter 'a) p) 0)
            (reduce + (map (num_letter 'b) p) 0)
            (reduce + (map (num_letter 'c) p) 0)
            (reduce + (map (num_letter 'd) p) 0)
            (reduce + (map (num_letter 'e) p) 0)
            (reduce + (map (num_letter 'f) p) 0)
            (reduce + (map (num_letter 'g) p) 0)
            (reduce + (map (num_letter 'h) p) 0)
            (reduce + (map (num_letter 'i) p) 0)
            (reduce + (map (num_letter 'j) p) 0)
            (reduce + (map (num_letter 'k) p) 0)
            (reduce + (map (num_letter 'l) p) 0)
            (reduce + (map (num_letter 'm) p) 0)
            (reduce + (map (num_letter 'n) p) 0)
            (reduce + (map (num_letter 'o) p) 0)
            (reduce + (map (num_letter 'p) p) 0)
            (reduce + (map (num_letter 'q) p) 0)
            (reduce + (map (num_letter 'r) p) 0)
            (reduce + (map (num_letter 's) p) 0)
            (reduce + (map (num_letter 't) p) 0)
            (reduce + (map (num_letter 'u) p) 0)
            (reduce + (map (num_letter 'v) p) 0)
            (reduce + (map (num_letter 'w) p) 0)
            (reduce + (map (num_letter 'x) p) 0)
            (reduce + (map (num_letter 'y) p) 0)
            (reduce + (map (num_letter 'z) p) 0)
           )))


(define find_max_char
  (lambda (l)
    (find_n (list_num_letter l) (apply max (list_num_letter l))))) ; it gives index number of the max char



;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***


;; -----------------------------------------------------
;; SPELL CHECKER FUNCTION

;;check a word's spell correctness
;;INPUT:a word(a global variable "dictionary" is included in the file "test-dictionary.ss", and can be used directly here)
;;OUTPUT:true(#t) or false(#f)
(define spell-checker 
  (lambda (w)
    (if (member w dictionary)
        #t
        #f   
    )))

;; -----------------------------------------------------
;; ENCODING FUNCTIONS

;;generate an Caesar Cipher single word encoders
;;INPUT:a number "n"
;;OUTPUT:a function, whose input is a word, and output is the encoded word
(define encode-n
  (lambda (n);;"n" is the distance, eg. n=3: a->d,b->e,...z->c
    (lambda (w);;"w" is the word to be encoded
      (if (equal? n 0) w
        (map (lambda(x) (vtl x))
          (map (lambda(x) (modulo (+ x n) 26)) ; we have to mod by 26 becaues of 'z' = 26+ n is over 26.
               (map (lambda(x) (ltv x)) w)))))))

;;encode a document
;;INPUT: a document "d" and a "encoder"
;;OUTPUT: an encoded document using a provided encoder
(define encode-d;;this encoder is supposed to be the output of "encode-n"
  (lambda (d encoder)
    (map (lambda (y) (map encoder y)) d)
    ))
    
;; -----------------------------------------------------
;; DECODE FUNCTION GENERATORS
;; 2 generators should be implemented, and each of them returns a decoder

;;generate a decoder using brute-force-version spell-checker
;;INPUT:an encoded paragraph "p"
;;OUTPUT:a decoder, whose input=a word, output=decoded word
(define Gen-Decoder-A
  (lambda (p)
    (encode-n (find_n (list_num_count p) (apply max (list_num_count p))))
                      
    ))

;;generate a decoder using frequency analysis
;;INPUT:same as above
;;OUTPUT:same as above
(define Gen-Decoder-B
  (lambda (p)
    (encode-n (modulo (+ 26 (- 4 (find_max_char p))) 26)))
    )

;; -----------------------------------------------------
;; CODE-BREAKER FUNCTION

;;a codebreaker
;;INPUT: an encoded document(of course by a Caesar's Cipher), a decoder(generated by functions above)
;;OUTPUT: a decoded document
(define Code-Breaker
  (lambda (d decoder)
    (if (null? d) d
        (append (cons (map decoder (car d)) (Code-Breaker (cdr d) decoder)))) ;use recursion to change the document to be correct
     ))
;; -----------------------------------------------------
;; EXAMPLE APPLICATIONS OF FUNCTIONS
;;(spell-checker '(h e l l o))
;;(define add5 (encode-n 5))
;;(encode-d document add5)
;;(define decoderSP1 (Gen-Decoder-A paragraph))
;;(define decoderFA1 (Gen-Decoder-B paragraph))
;;(Code-Breaker document decoderSP1)
