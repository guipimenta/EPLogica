#lang racket
;uma gramatica definida de acordo com as
;especificacoes do algortimo
;ver algoritmo ep2.txt
(define G (list (list "A" (list "a" "b" "c")) (list "B" (list "b")) ) )
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
(define (procuraNT frase NT NTList)
  (if (empty? NT)
      NTList ;frase so tem nao terminais
      (if (isMember (first NT) frase)
          (procuraNT frase (rest NT) (append NTList (list (first NT))))
          (procuraNT frase (rest NT) NTList)
      )
   )
)

;funcao que verifica, atraves de true ou false
;se um elemento e parte de uma lista
; entrada:
;   elemento
;   lista
; saida:
;   true: pertence
;   false: nao pertence
(define (isMember element list)
  (if (empty? list)
      #f
      (if (list? (member element list  ))
          #t
          (isMember element (rest list))
      )
  )
)


;Recebe um nao terminal e procura
;na gramatica, e retorna regras associadas a ele
;retorna uma lista de TODAS as regras associadas a
;esse nao terminal (lista regras)
(define (procuraRegra NT G regras)
  (if (empty? G)
      regras
      (if (isMember NT (first G))
          (procuraRegra NT (rest G) (append (list (first G) ) regras  ))
          (procuraRegra NT (rest G) regras)
       )
          
  )
)


; Para uma dada regra, substitui um NT pela regra equivalente
(define (substituiNTFrase frase regra novafrase)
  (if (empty? frase)
      novafrase
      (if (isMember (first (first regra)) (list (first frase)))
          (substituiNTFrase (rest frase) (list) (append (second (first regra)) novafrase))
          (substituiNTFrase (rest frase) regra (append (list (first frase)) novafrase))
      )
  )
)

(define (substituiNTRegra frase regra novaFrase)
  (if (empty? frase)
      novaFrase
      (if (isMember (first regra)  (list (first frase)))
          (substituiNTRegra (rest frase) regra (append novaFrase (list (rest regra))))
          (substituiNTRegra (rest frase) regra (append novaFrase (list (rest frase))))
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
(define frase (list "a" "A" "B" "B"))
(display "Gramatica: ")
(display (car G))
(newline)
(display "Derivacao da frase dada:")
(newline)
(display frase)
(newline)

(define NTList (list))
(procuraNT frase NT NTList)
(define regras (procuraRegra "A" G (list)) )

(define novafrase (list))
(substituiNTFrase frase regras  novafrase)

;(substituiNTRegra frase (procuraRegra (procuraNT (list "A" "b" "c") NT) G listaRegras) (list))