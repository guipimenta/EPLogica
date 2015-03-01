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
(define (determinaFechoReflexivo rbin freflexivo original)
  (let ([x '()])
    (if (empty? rbin)
        freflexivo
        ;procura par reflexivo do primeiro elemento do par ordenado
        (if (member (cons (caar rbin) (caar rbin)) original)
          (begin
            ;caso nao encontre, faz o mesmo para o segundo elemento do par ordenado
            (if (member (cons (cdar rbin) (cdar rbin)) original)
              (determinaFechoReflexivo (cdr rbin) freflexivo original)
              (let ([x (determinaFechoReflexivo (cdr rbin) (adicionaParEmLista (cons (cdar rbin) (cdar rbin)) freflexivo) original)]) x))
          ) 
          (let ([x (determinaFechoReflexivo (cdr rbin) (adicionaParEmLista (cons (caar rbin) (caar rbin)) freflexivo) original)]) x))
    )
  )
)


;fecho transitivo

(define (determinaFechoTransitivo rbin)
  (if (empty? rbin)
    null
    (begin
      ;(criaParesParaFechoTransitivo (car rbin) (percorreBinaria rbin (cdar rbin)))
      (determinaFechoTransitivo (cdr rbin))
    )
  )
)

;funcao recebe em par1 um par ordenado (ex.: (1,2)) e em listaDePares2 uma lista de pares ordenados (ex.: ((2,3) (2,4)))
;e retorna uma lista com os pares ordenados que completam o fech transitivo (ex.: ((1,3) (1,4)))
(define (criaParesParaFechoTransitivo par1 listaDePares2 listaNova)
    (if (empty? listaDePares2)
      listaNova
      (begin
        (adicionaParEmLista (cons (car par1) (cdar listaDePares2)) listaNova)
        (criaParesParaFechoTransitivo par1 (cdr listaDePares2) listaNova)
      )
    )
)

;exemplo
(display dpair_list)
(display "\n\n")
;(determinaFechoTransitivo dpair_list)
(criaParesParaFechoTransitivo (cons 1 2) (list (cons 2 3) (cons 2 4)) '())

;exemplo
;(define fecho_reflexivo (list))
;(print (determinaFechoReflexivo dpair_list fecho_reflexivo dpair_list))

;(adicionaParEmLista (cons 4 4) dpair_list)

