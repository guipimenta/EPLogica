#lang racket

;EP1 de Logica Computacional (PCS2046)
;Tomás Azevedo       7209968
;Guilherme Pimenta   
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
(define dpair_list (list (cons 'ab 'ce) (cons 3 4) (cons 4 5) (cons 4 6) (cons 'ce 'cf) (cons 'ce 8)))

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
        (if (equal? n (caar rbin))
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

;obs.: foi considerado que o fecho reflexivo leva em conta ambos o domínio e o contra-domínio

;funcao que adiciona um par a uma lista caso esse par ainda não esteja na lista
(define (adicionaParEmLista par rbin)
    (if (empty? rbin)
      (list par)
      (if (member par rbin)
        rbin
        (append rbin (list par))
      )
    )
)


;funcao que calcula o fecho reflexivo dos PRIMEIROS elementos de uma lista de pares ordenados
(define (determinaFechoReflexivo1 rbin freflexivo original)
  (let ([x '()])
    (if (empty? rbin)
        freflexivo
        (if (member (cons (caar rbin) (caar rbin)) original)
          (determinaFechoReflexivo1 (cdr rbin) freflexivo original)
          (determinaFechoReflexivo1 (cdr rbin) (adicionaParEmLista (cons (caar rbin) (caar rbin)) freflexivo) original)
        )
    )
  )
)

;funcao que calcula o fecho reflexivo dos SEGUNDOS elementos de uma lista de pares ordenados
(define (determinaFechoReflexivo2 rbin freflexivo original)
  (let ([x '()])
    (if (empty? rbin)
        freflexivo
        (if (member (cons (cdar rbin) (cdar rbin)) original)
          (determinaFechoReflexivo2 (cdr rbin) freflexivo original)
          (determinaFechoReflexivo2 (cdr rbin) (adicionaParEmLista (cons (cdar rbin) (cdar rbin)) freflexivo) original)
        )
    )
  )
)

;funcao que junta os dois fechos reflexivos parciais
(define (determinaFechoReflexivo rbin)
  (concatListas (determinaFechoReflexivo1 dpair_list '() dpair_list) (determinaFechoReflexivo2 dpair_list '() dpair_list))
)


;fecho transitivo

(define (determinaFechoTransitivo rbin)
  (determinaFechoTransitivoAux rbin '())
)

(define (determinaFechoTransitivoAux rbin ftransitivo)
  (let ([x '()])
    (if (empty? rbin)
      ftransitivo
      (begin
        (determinaFechoTransitivoAux (cdr rbin) (criaParesParaFechoTransitivo (car rbin) (percorreBinaria (cdr rbin) (cdar rbin)) ftransitivo))
      )
    )
  )
)


;funcao recebe em par1 um par ordenado (ex.: (1,2)) e em listaDePares2 uma lista de pares ordenados (ex.: ((2,3) (2,4)))
;e retorna uma lista com os pares ordenados que completam o fecho transitivo (ex.: ((1,3) (1,4)))
(define (criaParesParaFechoTransitivo par1 listaDePares2 listaNova)
  (let ([x '()])
    (if (empty? listaDePares2)
        listaNova
        (criaParesParaFechoTransitivo par1 (cdr listaDePares2) (adicionaParEmLista (cons (car par1) (cdar listaDePares2)) listaNova))

     )
   )
)

;funcao que obtem um fecho reflexivo e transitivo chamando as funcoes de cada fecho
(define (determinaFechoReflexivoTransitivo dpair_list)
  (concatListas (determinaFechoReflexivo dpair_list) (determinaFechoTransitivo dpair_list))
)

;funcao auxiliar para concatenar duas listas de pares ordenados retirando repeticoes
(define (concatListas lista1 lista2)
  (removerDuplicatas (append lista1 lista2))
)

;funcao de remocao de elementos iguais em uma lista
(define (removerDuplicatas lista)
  (cond ((null? lista)
         '())
        ((member (car lista) (cdr lista))
         (removerDuplicatas (cdr lista)))
        (else
         (cons (car lista) (removerDuplicatas (cdr lista)))
        )
  )
)

;exemplo
(display "Relacao = ")
(display dpair_list)
(display "\n\n")

(display "Fecho Reflexivo = ")
(display (determinaFechoReflexivo dpair_list))
(display "\n")
(display "Fecho Transitivo = ")
(display (determinaFechoTransitivo dpair_list))
(display "\n")
(display "Fecho Reflexivo e Transitivo = ")
(display (determinaFechoReflexivoTransitivo dpair_list))
(display "\n")

