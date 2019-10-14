#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack '())
(define (make-stack) (list))

(define (push element stack) (cons element stack))
(define (top stack)
  (if (null? stack)
      '()
      (car stack)))

(define (pop stack)
  (if (null? stack)
      '()
      (cdr stack)))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (cons stack (cons co-varnames (cons co-consts (cons co-names (cons co-code (cons IC '())))))))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (car (cddddr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (last stack-machine))

(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (find symbol symbols))

(define (find symbol symbols)
  (if (equal? symbol (car symbols))
      0
      (add1 (find symbol (cdr symbols)))))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-helper item symbol stack-machine place)
  (if (empty? stack-machine)
      '()
      (if (equal? place 0)
          (cons item (update-helper item symbol (cdr stack-machine) (sub1 place)))
          (cons (car stack-machine) (update-helper item symbol (cdr stack-machine) (sub1 place))))))

(define (update-stack-machine item symbol stack-machine)
  (update-helper item symbol stack-machine (get-symbol-index symbol)))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push  value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;;find element 
(define (search list position)
  (if (equal? position 0)
      (car list)
      (search (cdr list) (sub1 position))))

;; get operation
(define (operation index x y)
  (cond
    ((equal? index 0) (< x y))
    ((equal? index 1) (<= x y))
    ((equal? index 2) (equal? x y))
    ((equal? index 3) (not (equal? x y)))
    ((equal? index 4) (> x y))
    ((equal? index 5) (>= x y))
    ((equal? index 6) (member x y))
    ((equal? index 7) (not (member x y)))))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

;; advance with instruction counter
(define (next x)
  (run-stack-machine (update-stack-machine (add1 (get-IC x)) 'INSTRUCTION-COUNTER x)))

;; load instruction
(define (load f stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                              (push-exec-stack (hash-ref (f stack-machine) (instruction cdr stack-machine)) stack-machine))))
;; binary instruction
(define (binary op stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                                                  (push-exec-stack (op  (top (pop (get-stack stack-machine))) (top (get-stack stack-machine)))
                                                                   (pop-exec-stack (pop-exec-stack stack-machine))))))
;; inplace instruction
(define (inplace op stack-machine)
  (binary op stack-machine))

;; instruction argumets
(define (instruction p stack-machine)
  (p (search (get-code stack-machine) (get-IC stack-machine))))

;; compare instruction
(define (compare stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                            (push-exec-stack (operation (instruction cdr stack-machine) (cadr (get-stack stack-machine)) (top (get-stack stack-machine)))
                                             (pop-exec-stack (pop-exec-stack stack-machine))))))
;; pop-jump instruction
(define (pop-jump stack-machine)
  (run-stack-machine (update-stack-machine (if (top (get-stack stack-machine))
                                                      (add1 (get-IC stack-machine))
                                                      (quotient (instruction cdr stack-machine) 2)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))
;; call-function instruction
(define (call-function stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                                                  (push-exec-stack (apply (get-function (search (get-stack stack-machine) (instruction cdr stack-machine)))
                                                                          (take (get-stack stack-machine) (instruction cdr stack-machine)))
                                                                   (update-stack-machine (drop (get-stack stack-machine) (add1 (instruction cdr stack-machine)))
                                                                                         'STACK stack-machine)))))
;; for instruction
(define (for-iter stack-machine)
  (if (null? (top (get-stack stack-machine)))
             (run-stack-machine (update-stack-machine (+ (get-IC stack-machine) (quotient (+ (instruction cdr stack-machine) 2) 2)) 'INSTRUCTION-COUNTER
                                                      (pop-exec-stack stack-machine)))
             (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                                                      (push-exec-stack (top (top (get-stack stack-machine)))
                                                                       (push-exec-stack (pop (top (get-stack stack-machine))) (pop-exec-stack stack-machine)))))))
;; jump instruction
(define (jump stack-machine)
  (run-stack-machine (update-stack-machine (quotient (instruction cdr stack-machine) 2) 'INSTRUCTION-COUNTER stack-machine)))

;; pop-top instruction
(define (pop-top stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER (pop-exec-stack stack-machine))))

;; store instruction
(define (store stack-machine)
  (run-stack-machine (update-stack-machine (add1 (get-IC stack-machine)) 'INSTRUCTION-COUNTER
                                (update-stack-machine (hash-set (get-varnames stack-machine) (instruction cdr stack-machine) (top (get-stack stack-machine))) 'CO-VARNAMES
                                                      (pop-exec-stack stack-machine)))))

(define (run-stack-machine stack-machine)
  (if (= (get-IC stack-machine) (length (get-code stack-machine)))
      stack-machine
      (cond
        ((equal? (instruction car stack-machine) 'LOAD_CONST)
        (load get-consts stack-machine))
        ((equal? (instruction car stack-machine) 'STORE_FAST)
         (store stack-machine))
        ((equal? (instruction car stack-machine) 'LOAD_FAST)
         (load get-varnames stack-machine))
        ((equal? (instruction car stack-machine) 'LOAD_GLOBAL)
        (load get-names stack-machine))
        ((equal? (instruction car stack-machine) 'BINARY_ADD)
         (binary + stack-machine))
        ((equal? (instruction car stack-machine) 'BINARY_SUBTRACT)
         (binary - stack-machine))
        ((equal? (instruction car stack-machine) 'BINARY_MODULO)
         (binary modulo stack-machine))
        ((equal? (instruction car stack-machine) 'INPLACE_ADD)
         (inplace + stack-machine))
        ((equal? (instruction car stack-machine) 'INPLACE_SUBTRACT)
         (inplace - stack-machine))
        ((equal? (instruction car stack-machine) 'INPLACE_MODULO)
         (inplace modulo stack-machine))
        ((equal? (instruction car stack-machine) 'COMPARE_OP)
         (compare stack-machine))
        ((equal? (instruction car stack-machine) 'POP_JUMP_IF_FALSE)
         (pop-jump stack-machine))
        ((equal? (instruction car stack-machine) 'POP_TOP)
         (pop-top stack-machine))
        ((equal? (instruction car stack-machine) 'JUMP_ABSOLUTE)
         (jump stack-machine))        
        ((equal? (instruction car stack-machine) 'FOR_ITER)
         (for-iter stack-machine))
        ((equal? (instruction car stack-machine) 'CALL_FUNCTION)
         (call-function stack-machine))                                           
         (else (next stack-machine)))))