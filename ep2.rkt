#lang racket
;uma gramatica definida de acordo com as
;especificacoes do algortimo
;ver algoritmo ep2.txt
(define G (list (list "A" (list "a" "B" "c")) (list "B" (list "b")) ) )
(define NT (list "A" "B"))


;funcao auxiliar for
(define (percorreConjunto myconj)
  (if (empty? myconj)
      ;fim da recursao
      null
      ;esse begin e necessario para executarmos multiplos procedures dentro do if
      (begin
        ;recursao que permite percorrer atraves dos conjuntos...
        ;(cons (car myconj)(percorreConjunto (cdr myconj)))
        (percorreConjunto (cdr myconj))
      )
   )
)


;Recebe nao terminais
;Procura nao terminais na frase
(define (procuraNT frase NT)
  (if (empty? NT)
      null ;frase so tem nao terminais
      (if (member (car NT) frase)
          (car NT)
          (procuraNT frase (cdr NT))
      )
   )
)


;Recebe um nao terminal e procura
;na gramatica, e retorna regras associadas a ele
(define (procuraRegra NT G regras)
  (if (empty? G)
      regras
      (if (member NT (car G))
          (procuraRegra NT (cdr G) (append regras (car G)))
          (procuraRegra NT (cdr G) regras))
  )
)

(define (substituiNTRegra frase regra novaFrase)
  (if (empty? frase)
      novaFrase
      (if (member (car regra)  (list (car frase)))
          (substituiNTRegra (cdr frase) regra (append novaFrase (list (cdr regra))))
          (substituiNTRegra (cdr frase) regra (append novaFrase (list (car frase))))
      )
  )                         
)



;funcao que faz trocas no nao terminal
;por uma nova regra
(define (criaNovasFrases frase G regras novasFrases)
  (if (empty? regras)
      novasFrases
      null
  )
)

(define listaRegras (list))
(define frase (list "a" "A" "b"))
(display "Gramatica: ")
(display (car G))
(newline)
(display "Derivacao da frase dada:")
(newline)
(display frase)
(newline)


(substituiNTRegra frase (procuraRegra (procuraNT (list "A" "b" "c") NT) G listaRegras) (list))