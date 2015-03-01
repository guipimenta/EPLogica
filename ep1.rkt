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
(define dpair_list (list (cons 1 2) (cons 3 4) (cons 4 5) (cons 4 4) (cons 6 7) (cons 4 7)))

;
;     PARTE 1
;
;item 1 - funcao que percorre lista

;funcao auxiliar for
(define (percorreConjunto myconj)
  (if (empty? myconj)
      ;fim da recursao
      null
      ;esse begin e necessario para executarmos multiplos procedures dentro do if
      (begin
        ;recursao que permite percorrer atraves dos conjuntos...
        (cons (car myconj)(percorreConjunto (cdr myconj)))
        ;(percorreConjunto (cdr myconj))
      )
   )
)

;chamando a funcao definida anteriormente - teste
;(percorreConjunto dpair_list)

;
;     PARTE 2
;

;percorrendo uma relacao binaria
(define (percorreBinaria rbin n)
  (if (empty? rbin)
      null
      (begin
        ;compara o valor n com o primeiro elemento do primeiro par da lista
        (if (= n (caar rbin))
            (cons (car rbin) (percorreBinaria (cdr rbin) n))

            (percorreBinaria (cdr rbin) n)
        ) 
      )
  )
)

;(percorreBinaria dpair_list 4)

;
;     PARTE 3
;

;fecho reflexivo

;funcao que adiciona um par a uma lista caso esse par ainda não esteja na lista
(define (adicionaParEmLista par rbin)
  (let ([x '()])
  (if (empty? rbin)
    (list par)
    (if (member par rbin)
      rbin
      (let ([x  (append rbin (list par))])
        x)
      
    )
  )
  )
)


;funcao que calcula o fecho reflexivo de uma lista de pares ordenados
;o primeiro parametro é a lista original e o segundo é o fecho reflexivo no retorno da funcao
(define (determinaFechoReflexivo rbin freflexivo)
  (let ([x '()])
    (if (empty? rbin)
        freflexivo
        (if (member (cons (caar rbin) (caar rbin)) rbin)
          (determinaFechoReflexivo (cdr rbin) freflexivo)
          (let ([x (determinaFechoReflexivo (cdr rbin) (adicionaParEmLista (cons (caar rbin) (caar rbin)) freflexivo))]) x))
    )
  )
)


;exemplo
(define fecho_reflexivo (list))
(print (determinaFechoReflexivo dpair_list fecho_reflexivo))
;(adicionaParEmLista (cons 4 4) dpair_list)

