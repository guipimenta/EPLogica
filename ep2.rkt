#lang racket
; Exercicio-Programa 2: recursividade para reconhecimento de cadeias
; Grupo: Guilherme Pimenta Sorregotti
;        Tomás Albuquerque Azevedo


;
; DEFINICAO: uma regra do tipo A-> abc sera definida, em scheme, como
;            ("A", ("a" "b" "c"))
; DEFINICAO: uma gramatica sera definida, para o projeto, como uma lista de regras,
;            um conjunto NT de nao terminais,
;            e um NT que é o símbolo inicial (SI) da gramática
;            ex: S-> abA A->cB B->d
;                Regras: ((("S") ("a" "b" "A")), ( "A" ("c" "B")), ( "B" ("d")))
;                NT: ("S" "A" "B")
;                SI: ("S")
; EXEMPLO:
;(define G (list (list "S" (list "a" "b" "A")) (list "A" (list "c" "A")) (list "A" (list)) ) )
;(define NT (list "S" "A"))
;(define SI (list "S"))


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
      (if (list? (member element list) )
          #t
          (isMember element (rest list))
      )
  )
)

; Descricao:
;            Funcao que retorna parte de uma lista
; Entrada:
;      lst: lista a ser manipulada
;      start: posicao do primeiro caracter da novalista (comeca em 1)
;      count: contagem de caracteres a serem extraidos
; saida:
;       lista nova
(define (slice lst start count)
  (if (> start 1)
      (slice (cdr lst) (- start 1) count)
      (get-n-items lst count)
  )
)

; Descricao:
;            Funcao a ser utilizada pela funcao SLICE, que retorna N itens da uma lista
; Entrada:
;      lst: lista a ser manipulada
;      num: numero de itens a serem extraidos
; saida:
;       lista nova
(define (get-n-items lst num)
  (if (> num 0)
      (cons (car lst) (get-n-items (cdr lst) (- num 1)))
      '()
  )
)

; Descricao:
;            Funcao que determina se um elemento é membro de uma lista
; Explicacao:
;            A Funcao utiliza as funcoes isMemberAux1 e isMemberAux2 para separar os casos de
;      uma lista simples e uma lista de listas, respectivamente
; Entrada:
;      element: elemento a ser procurado na lista
;      lista: lista a ser manipulada
; saida:
;       true: se elemento pertence a lista
;       false: se elemento nao pertence a lista
(define (isMember2 element lista)
  (if (empty? lista)
      #f
      (if (list? (first lista))
          (isMemberAux1 element lista #f)
          (isMemberAux2 element lista #f)
      )
  )
)

; Descricao:
;            Funcao que determina se um elemento é membro de uma lista de listas
; Entrada:
;      element: elemento a ser procurado na lista
;      lista: lista a ser manipulada
;      return: variavel auxiliar de retorno da funcao
; saida:
;       true: se elemento pertence a lista
;       false: se elemento nao pertence a lista
(define (isMemberAux1 element lista return)
  (if (empty? element)
      #t
      (if (list? element)
          (if (list? (first element))
              (if (< (length lista) (length element))
                  return
                  (isMemberAux1 element (rest lista) (or (equal? element (slice lista 1 (length element))) return))
              )
              (if (< (length lista) 1)
                  return
                  (isMemberAux1 element (rest lista) (or (equal? (list element) (slice lista 1 1)) return))
              )
          )
          (if (< (length lista) 1)
              return
              (isMemberAux1 element (rest lista) (or (equal? (list (list element)) (slice lista 1 1)) return))
          )
      )
  )
)

; Descricao:
;            Funcao que determina se um elemento é membro de uma lista simples
; Entrada:
;      element: elemento a ser procurado na lista
;      lista: lista a ser manipulada
;      return: variavel auxiliar de retorno da funcao
; saida:
;       true: se elemento pertence a lista
;       false: se elemento nao pertence a lista
(define (isMemberAux2 element lista return)
  (if (empty? element)
      #t
      (if (list? element)
          (if (< (length lista) (length element))
              return
              (isMemberAux2 element (rest lista) (or (equal? element (slice lista 1 (length element))) return))
          )
          (if (< (length lista) 1)
              return
              (isMemberAux2 element (rest lista) (or (equal? (list element) (slice lista 1 1)) return))
          )
      )
  )
)

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
      (if (isMember2 (first NT) frase)
          (procuraNT frase (rest NT) (append NTList (list (first NT))))
          (procuraNT frase (rest NT) NTList)
      )
   )
)

; Descricao:
;              Funcao auxiliar que identifica TODOS os nao-terminais em múltiplas frases
; Entrada:
;      frases:  LISTA de listas aonde a funcao vai verificar a existencia de NT
;         NT:  LISTA de elementos nao terminais a serem procurados
;     NTList:  LISTA vazia
; saida:
;     NTLIST:  LISTA com todos NT terminais pertencentes a frase
(define (procuraNT2 frases NT NTList)
  (if (empty? frases)
      NTList ;frase so tem nao terminais
      (if (list? (first frases))
          (procuraNT2 (rest frases) NT (appendToListIfNotPresent (procuraNT (first frases) NT (list)) NTList))
          (procuraNT2 (list) NT (appendToListIfNotPresent (procuraNT frases NT (list)) NTList))
      )
   )
)

; Descricao:
;              Funcao auxiliar que identifica TODAS as regras de um conjunto de nao-terminais
; Entrada:
;      NTList:  LISTA de listas aonde a funcao vai verificar a existencia de NT
;          G:  LISTA de regras (gramática) disponíveis
;     regras:  variavel auxiliar de retorno (inicialmente nula)
; saida:
;     regras  LISTA com todas as regras de uma lista de terminais
(define (procuraRegrasConjuntoNT NTList G regras)
  (if (empty? NTList)
      regras
      (procuraRegrasConjuntoNT (rest NTList) G (procuraRegra (first NTList) G regras))
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
      (if (isMember2 NT (list (first (first G))))
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
          (if (isMember2 (first (first regra)) (list (first frase)))
              (substituiNTFrase (rest frase) (list) (append (reverse (second (first regra))) novafrase))
              (substituiNTFrase (rest frase) regra (append (list (first frase)) novafrase))
          )
      )
  )
)

; Descricao:
;              Substitui todas as regras nos terminais da frase
; Argumentos de entrada:
;       frase: lista que denota uma frase
;       regras: múltiplas regras na codificao padrao adotada pelo EP (vide comentarios acima)
;   novafrase: nova frase, substituindo a primeira ocorrencia do NT pela regra
; Saida:
;   novafrase: nova frase, com um NT substituido pela regra
(define (substituiNTFrase2 frase regras novafrase)
  (if (empty? regras)
      novafrase
      (if (empty? novafrase)
          (substituiNTFrase2 frase (rest regras) (append (substituiNTFrase frase (list (first regras)) (list)) novafrase))
          (if (list? (first novafrase))
              (substituiNTFrase2 frase (rest regras) (append (list (substituiNTFrase frase (list (first regras)) (list))) novafrase))
              (substituiNTFrase2 frase (rest regras) (append (list (substituiNTFrase frase (list (first regras)) (list))) (list novafrase)))
          )
      )
  )
)

; Descricao:
;              Substitui todas as regras em todas as frases passadas
; Argumentos de entrada:
;       frases: lista de listas que denota multiplas frases
;       regras: múltiplas regras na codificao padrao adotada pelo EP (vide comentarios acima)
;   novasfrases: novas frases, substituindo a primeira ocorrencia do NT pela regra
;           NT: nao-terminais das regras (para passar aquelas regras referentes aos nao-terminais de cada frase)
; Saida:
;   novasfrases: novas frases, com um NT substituido pela regra
(define (substituiNTFrase3 frases regras novasfrases NT)
  (if (empty? frases)
      novasfrases
      (if (list? (first frases))
          (substituiNTFrase3 (rest frases) regras (appendToListIfNotPresent (substituiNTFrase2 (first frases) (procuraRegrasConjuntoNT (procuraNT2 (first frases) NT (list)) regras (list)) (list)) novasfrases) NT)
          (substituiNTFrase3 (list) regras (appendToListIfNotPresent (substituiNTFrase2 frases (procuraRegrasConjuntoNT (procuraNT2 frases NT (list)) regras (list)) (list)) novasfrases) NT)
      )
  )
)

;teriamos que ter um protipo desse tipo
;que lindo!
(define (achaTodasFraseProfundidadeNAux SI G NT novafrase SIZE )
  (if (empty? SI)
      (frasesDeTamanhoNOuMenor novafrase SIZE (list))
      (achaTodasFraseProfundidadeNAux (frasesDeTamanhoNOuMenor (substituiNTFrase3 SI (procuraRegrasConjuntoNT (procuraNT2 SI NT (list)) G (list)) (list) NT) (+ SIZE 1) (list)) G NT (appendToListIfNotPresent SI novafrase) SIZE )
  )
)

(define (achaTodasFraseProfundidadeN SI G NT SIZE )
  (achaTodasFraseProfundidadeNAux SI G NT (list) SIZE)
)

;funcao que retorna as frases (de uma lista de frases) com tamanho menor ou igual a SIZE
(define (frasesDeTamanhoNOuMenor frases SIZE novasfrases)
  (if (empty? frases)
      novasfrases
      (if (list? (first frases))
          (if (<= (length (first frases)) SIZE)
              (frasesDeTamanhoNOuMenor (rest frases) SIZE (appendToListIfNotPresent (first frases) novasfrases))
              (frasesDeTamanhoNOuMenor (rest frases) SIZE novasfrases)
          )
          (if (<= (length frases) SIZE)
              (frasesDeTamanhoNOuMenor (list) SIZE (appendToListIfNotPresent frases novasfrases))
              (frasesDeTamanhoNOuMenor (list) SIZE novasfrases)
          )
      )
  )
)

;funcao que dá um append de um elemento no final de uma lista
(define (appendToList element lista)
  (if (empty? lista)
      (if (list? element)
          element
          (list element)
      )
      (if (list? (first lista))
          (append lista (list element))
          (append (list lista) (list element))
      )
  )
)

;funcao que  dá um append de um elemento no final de uma lista caso esse elemento ainda nao pertenca a lista
(define (appendToListIfNotPresent element lista)
  (if (empty? element)
      lista
      (if (empty? lista)
          (if (list? (first element))
              (if (isMember2 (first element) lista)
                  (appendToListIfNotPresent (rest element) lista)
                  (appendToListIfNotPresent (rest element) (appendToList (first element) lista))
              )
              (if (isMember2 element lista)
                  (appendToListIfNotPresent (list) lista)
                  (appendToListIfNotPresent (list) (appendToList element lista))
              )
          )
          (if (list? (first lista))
              (if (list? (first element))
                  (if (isMember2 (first element) lista)
                      (appendToListIfNotPresent (rest element) lista)
                      (appendToListIfNotPresent (rest element) (appendToList (first element) lista))
                  )
                  (if (isMember2 element lista)
                      (appendToListIfNotPresent (list) lista)
                      (appendToListIfNotPresent (list) (appendToList element lista))
                  )
              )
              (if (list? (first element))
                  (if (isMember2 (first element) (list lista))
                      (appendToListIfNotPresent (rest element) lista)
                      (appendToListIfNotPresent (rest element) (appendToList (first element) lista))
                  )
                  (if (isMember2 element (list lista))
                      (appendToListIfNotPresent (list) lista)
                      (appendToListIfNotPresent (list) (appendToList element lista))
                  )
              )
          )
      )
  )
)

;caso de teste 1 - linguagem regular (tipo 3)
;(define G (list (list "S" (list "a" "S")) (list "S" (list "b" "A")) (list "A" (list "c" "A")) (list "A" (list)) ) )
;(define NT (list "S" "A"))
;(define SI (list "S"))

;(achaTodasFraseProfundidadeN SI G NT 3)

;caso de teste 2 - linguagem livre de contexto (tipo 2)
(define G (list (list "S" (list "a" "S" "b")) (list "S" (list))) )
(define NT (list "S"))
(define SI (list "S"))

(achaTodasFraseProfundidadeN SI G NT 10)

