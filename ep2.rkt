#lang racket
; Exercicio-Programa 2: recursividade para reconhecimento de cadeias
; Grupo: Guilherme Pimenta Sorregotti
;        Tomas Azevedo


;
; DEFINICAO: uma regra do tipo A-> abc sera definida, em scheme, como
;            ("A", ("a" "b" "c"))
; DEFINICAO: uma gramatica sera definida, para o projeto, como uma lista de regras
;            e um conjunto NT de nao terminais
;            ex: A->abc B->aBa
;                Regras: ((("A") ("a" "b" "c")), ( "B" ("aBa")))
;                NT: ("A" "B")
(define G (list (list "A" (list "a" "b" "c")) (list "B" (list "b")) ) )
(define NT (list "A" "B"))


; Descricao:
;              Funcao auxiliar que identifica TODOS os nao-terminais em uma frase
; Entrada:
;      frase:  LISTA aonde a funcao vai verificar a existencia de NT
;         NT:  ELEMENTO a ser procurado
;     NTList:  LISTA vazia
; saida:
;     NTLIST:  LISTA com todos NT terminais pertencentes a frase

(define (procuraNT frase NT NTList)
  (if (empty? NT)
      NTList ;frase so tem nao terminais
      (if (isMember (first NT) frase)
          (procuraNT frase (rest NT) (append NTList (list (first NT))))
          (procuraNT frase (rest NT) NTList)
      )
   )
)

; Descricao:
;            Funcao auxiliar para determinar se um elemento faz parte de uma lista
; Entrada:
;   elemento: ELEMENTO a ser procurado na lista
;      lista:  lista aonde o elemento anterior vai ser procurado
; saida:
;       true: pertence
;      false: nao pertence
(define (isMember element list)
  (if (empty? list)
      #f
      (if (list? (member element list  ))
          #t
          (isMember element (rest list))
      )
  )
)


;
; Descricao:
;            Recebe um NT e uma gramatica, retorna, em uma lista, todas
;            regras associadas a esse NT
; Entrada:
;            NT: ELEMENTO nao terminal
;             G: Lista que representa as regras da gramatica
;        regras: lista vazia a ser usada como retorno
; Saida:
;        regras: lista armazenando todas as regras possiveis para o NT dado
(define (procuraRegra NT G regras)
  (if (empty? G)
      regras
      (if (isMember NT (first G))
          (procuraRegra NT (rest G) (append (list (first G) ) regras  ))
          (procuraRegra NT (rest G) regras)
       )
          
  )
)

; Descricao:
;              Para uma dada regra, substitui UM UNICO NT pela regra equivalente
;              Recursao dessa funcao podemos obter gerar todas as novas frases
; Argumentos de entrada:
;       frase: lista que denota uma frase
;       regra: regra na codificao padrao adotada pelo EP (vide comentarios acima)
;   novafrase: nova frase, substituindo a primeira ocorrencia do NT pela regra
; Saida:
;   novafrase: nova frase, com um NT substituido pela regra
(define (substituiNTFrase frase regra novafrase)
  (if (empty? frase)
      (reverse novafrase)
      (if (empty? regra)
          (substituiNTFrase (rest frase) regra (append (list (first frase)) novafrase))
          (if (isMember (first (first regra)) (list (first frase)))
              (substituiNTFrase (rest frase) (list) (append (reverse (second (first regra))) novafrase))
              (substituiNTFrase (rest frase) regra (append (list (first frase)) novafrase))
              )
      )
  )
)




(define listaRegras (list))
(define frase (list "a" "A" "A" "B"))
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