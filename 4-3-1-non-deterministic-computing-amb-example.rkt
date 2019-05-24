#lang racket

; Non compiling examples,
; Copy them into the input from 4-3-1-non-deterministic-computing-amb.rkt evaluator

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items)
       (an-element-of (cdr items))))


; Pair example
(define (pairs)
  (define (require p)
    (if (not p) (amb)))
  (let ([a (amb 1 2 3)]
        [b (amb 1 2 3)])
    (require (= (+ a b) 4))
    (list a b)))

(pairs)

; Baker, Cooper, Fletcher, Miller, and Smith live on different floors of an apartment house
; that contains only five floors.
; * Baker does not live on the top floor.
; * Cooper does not live on the bottom floor.
; * Fletcher does not live on either the top or the bottom floor.
; * Miller lives on a higher floor than does Cooper.
; * Smith does not live on a floor adjacent to Fletcher’s.
; * Fletcher does not live on a floor adjacent to Cooper’s.
; Where does everyone live?
(define (multiple-dwelling)
  (let ([baker (amb 1 2 3 4 5)]
        [cooper (amb 1 2 3 4 5)]
        [fletcher (amb 1 2 3 4 5)]
        [miller (amb 1 2 3 4 5)]
        [smith (amb 1 2 3 4 5)])
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))

    (list (cons 'baker baker)
          (cons 'cooper cooper)
          (cons 'fletcher fletcher)
          (cons 'miller miller)
          (cons 'smith smith))))

(multiple-dwelling)

; Parsing natural language

(define nouns
  '(noun student professor cat class))

(define verbs
  '(verb studies lectures eats sleeps))

(define articles
  '(article the a))

(define prepositions
  '(prep for to in by with))
   
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

; The grammar defines that a sentence is composed by a noun phrase, followed by a verb.
; A noun phrase is composed of an article followed by a noun.
; "The cat east" is parsed as "(sentence (noun-phrase (article the) (noun cat)) (verb eats))"
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase (maybe-extend (list 'noun-phrase noun-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase (maybe-extend (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) ; memq: take in list from member equal to first argument.
                 (cdr word-list)))
  (let ([found-word (car *unparsed*)])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '()) ; global variable holding all unparsed words

(define (parse input)
  (set! *unparsed* input)
  (let ([sent (parse-sentence)])
    (require (null? *unparsed*))
    sent))

