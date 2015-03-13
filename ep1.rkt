#lang racket

;EP1 de Logica Computacional (PCS2046)
;Tomás Azevedo       7209968
;Guilherme Pimenta   6872835
;Algoritmo para encontrar um feixo transitivo reflexivo

;
;     PARTE 1
;

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

;
;     PARTE 3
;

;fecho reflexivo

;obs.: foi considerado que o fecho reflexivo leva em conta ambos o domínio e o contra-domínio

;funcao que calcula o fecho reflexivo dos PRIMEIROS elementos de uma lista de pares ordenados
(define (determinaFechoReflexivo1 rbin freflexivo original)
  (if (empty? rbin)
      freflexivo
      (if (member (cons (caar rbin) (caar rbin)) original)
        (determinaFechoReflexivo1 (cdr rbin) freflexivo original)
        (determinaFechoReflexivo1 (cdr rbin) (adicionaParEmLista (cons (caar rbin) (caar rbin)) freflexivo) original)
      )
  )
)

;funcao que calcula o fecho reflexivo dos SEGUNDOS elementos de uma lista de pares ordenados
(define (determinaFechoReflexivo2 rbin freflexivo original)
  (if (empty? rbin)
      freflexivo
      (if (member (cons (cdar rbin) (cdar rbin)) original)
        (determinaFechoReflexivo2 (cdr rbin) freflexivo original)
        (determinaFechoReflexivo2 (cdr rbin) (adicionaParEmLista (cons (cdar rbin) (cdar rbin)) freflexivo) original)
      )
  )
)

;funcao que junta os dois fechos reflexivos parciais
(define (determinaFechoReflexivo rbin)
  (concatListas (determinaFechoReflexivo1 rbin '() rbin) (determinaFechoReflexivo2 rbin '() rbin))
)


;fecho transitivo

(define (determinaFechoTransitivo rbin)
  (removerElementosDeL1EmL2 (concatListas (determinaFechoTransitivoAux rbin '()) (determinaFechoTransitivoAux (reverse rbin) '())) rbin)
)

(define (determinaFechoTransitivoAux rbin ftransitivo)
  (if (empty? rbin)
    ftransitivo
    (begin
      (determinaFechoTransitivoAux (cdr rbin) (criaParesParaFechoTransitivo (car rbin) (percorreBinaria (cdr rbin) (cdar rbin)) ftransitivo))
    )
  )
)


;funcao recebe em par1 um par ordenado (ex.: (1,2)) e em listaDePares2 uma lista de pares ordenados (ex.: ((2,3) (2,4)))
;e retorna uma lista com os pares ordenados que completam o fecho transitivo (ex.: ((1,3) (1,4)))
(define (criaParesParaFechoTransitivo par1 listaDePares2 listaNova)
  (if (empty? listaDePares2)
    listaNova
    (begin
      ;apenas adiciona par ordenado transitivo se nao for da forma (x,x)
      (if (equal? (car par1) (cdar listaDePares2))
        (criaParesParaFechoTransitivo par1 (cdr listaDePares2) listaNova)
        (criaParesParaFechoTransitivo par1 (cdr listaDePares2) (adicionaParEmLista (cons (car par1) (cdar listaDePares2)) listaNova))
      )
    )
  )
)

;funcao que obtem um fecho reflexivo e transitivo chamando as funcoes de cada fecho
(define (determinaFechoReflexivoTransitivo rbin)
  (concatListas (determinaFechoReflexivo rbin) (determinaFechoTransitivo rbin))
)


;funcoes auxiliares

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

;funcao para concatenar duas listas de pares ordenados retirando repeticoes
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

(define (removerElementosDeL1EmL2 lista1 lista2)
  (cond ((null? lista1)
         '())
        ((member (car lista1) lista2)
         (removerElementosDeL1EmL2 (cdr lista1) lista2))
        (else
         (cons (car lista1) (removerElementosDeL1EmL2 (cdr lista1) lista2))
        )
  )
)

;
;Casos de teste
;

;caso de teste 1
;B = ((1,2) (2,3) (2,4))
;R = ((1,1) (2,2) (3,3) (4,4))
;T = ((1,3) (1,4))
;RT = ((1,1) (2,2) (3,3) (4,4) (1,3) (1,4))

;(define relacaoB (list (cons 1 2) (cons 2 3) (cons 2 4) (cons 3 1)))

;(display "Relacao = ")
;(display relacaoB)
;(display "\n\n")

;(display "Fecho Reflexivo = ")
;(display (determinaFechoReflexivo relacaoB))
;(display "\n")
;(display "Fecho Transitivo = ")
;(display (determinaFechoTransitivo relacaoB))
;(display "\n")
;(display "Fecho Reflexivo e Transitivo = ")
;(display (determinaFechoReflexivoTransitivo relacaoB))
;(display "\n")


;caso de teste 2
;B = ((1,1) (1,2) (1,4) (2,3) (2,4) (3,2) (4,1))
;R = ((2,2) (3,3) (4,4))
;T = ((1,3) (2,1) (3,4) (4,2))
;RT = ((2,2) (3,3) (4,4) (1,3) (2,1) (3,4) (4,2))

;(define relacaoB (list (cons 1 1) (cons 1 2) (cons 1 4) (cons 2 3) (cons 2 4) (cons 3 2) (cons 4 1)))

;(display "Relacao = ")
;(display relacaoB)
;(display "\n\n")

;(display "Fecho Reflexivo = ")
;(display (determinaFechoReflexivo relacaoB))
;(display "\n")
;(display "Fecho Transitivo = ")
;(display (determinaFechoTransitivo relacaoB))
;(display "\n")
;(display "Fecho Reflexivo e Transitivo = ")
;(display (determinaFechoReflexivoTransitivo relacaoB))
;(display "\n")


;caso de teste 3
;B = ((ab,ce) (3,4) (2a,3P7) (cf,1) (ce,ab) (4,ab) (7f3v,bcD81) (7f3v,7f3v) (45b,2a) (4,1) (1,ab))
;R = ((ab,ab) (3,3) (2a,2a) (cf, cf) (ce,ce) (4,4) (45b,45b) (1,1) (3P7,3P7) (bcD81))
;T = ((3,ab) (3,1) (cf,ab) (1,ce) (45b,3P7) (4,ce))
;RT = ((ab,ab) (3,3) (2a,2a) (cf, cf) (ce,ce) (4,4) (45b,45b) (1,1) (3P7,3P7) (bcD81) (3,ab) (3,1) (cf,ab) (1,ce) (45b,3P7) (4,ce))

;(define rbin (list (cons 'ab 'ce) (cons 3 4) (cons '2a '3P7) (cons 'cf '1) (cons 'ce 'ab) (cons '4 'ab) (cons '7f3v 'bcD81) (cons '7f3v '7f3v) (cons '45b '2a) (cons 4 1) (cons '1 'ab)))

;(display "Relacao = ")
;(display rbin)
;(display "\n\n")

;(display "Fecho Reflexivo = ")
;(display (determinaFechoReflexivo rbin))
;(display "\n")
;(display "Fecho Transitivo = ")
;(display (determinaFechoTransitivo rbin))
;(display "\n")
;(display "Fecho Reflexivo e Transitivo = ")
;(display (determinaFechoReflexivoTransitivo rbin))
;(display "\n")


;caso de teste 4
;B = ()
;R = ()
;T = ()
;RT = ()

;(define rbin (list))

;(display "Relacao = ")
;(display rbin)
;(display "\n\n")

;(display "Fecho Reflexivo = ")
;(display (determinaFechoReflexivo rbin))
;(display "\n")
;(display "Fecho Transitivo = ")
;(display (determinaFechoTransitivo rbin))
;(display "\n")
;(display "Fecho Reflexivo e Transitivo = ")
;(display (determinaFechoReflexivoTransitivo rbin))
;(display "\n")

