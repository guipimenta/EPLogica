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

;conjunto de dotted pairs
(define dpair_list '((1 2) (3 4) (4 5) (6 7) (4 7)))


;item 1 - funcao que percorre lista

;funcao auxiliar for
(define (percorreConjunto myconj)
  (if (empty? myconj)
      ;fim da recursao
      null
      ;esse begin e necessario para executarmos multiplos procedures dentro do if
      (begin
        ;recursao que permite percorrer atraves dos conjuntos...
        (cons (first myconj)(percorreConjunto (rest myconj)))
        ;(percorreConjunto (rest myconj))
      )
   )
)

;chamando a funcao definida anteriormente - teste
(percorreConjunto dpair_list)

;percorrendo uma relacao binaria
(define (percorreBinaria rbin n)
  (if (empty? rbin)
      null
      (begin
        ;compara o valor n com o primeiro elemento do primeiro par da lista
        (if (= n (first (first rbin)))
            (cons (first rbin) (percorreBinaria (rest rbin) n))

            (percorreBinaria (rest rbin) n)
        ) 
      )
  )
)

(percorreBinaria dpair_list 4)

