#lang racket

;EP1 de Logica
;Algoritmo para encontrar um feixo transitivo reflexivo

;definicao de tupla
(define (tupla x y)
  (list x y))

;conjunto de tuplas definido na mao
(define (conjunto a b c d) (
                      list a b c d))

;conjunto efetivo que iremos percorrer
(define arg (conjunto (tupla 1 2) (tupla 3 4) (tupla 4 5) (tupla 6 7)))


;item 1 - funcao que percorre lista

;funcao auxiliar for
(define (percorreConjunto myconj)
  (if (empty? myconj)
      ;fim da recursao
      (display "")
      ;esse begin e necessario para executarmos multiplos procedures dentro do if
      (begin 
        ;display dos dados
        (display (first myconj))
        ;recursao que permite percorrer atraves dos conjuntos...
        (percorreConjunto (rest myconj))
      )
   )
)

;chamando a funcao definida anteriormente - teste
(percorreConjunto arg)

;percorrendo uma relacao binaria
(define (percorreBinaria rbin n)
  (if (empty? rbin)
      null
      (begin 
        ((cdr (first rbin)))
      )
  )
)


  

