#lang scheme
;Função que embaralha palavra

(define shuffle 
  (lambda (list)
    (if (< (length list) 2) 
        list
        (let ((item (list-ref list (random (length list)))))
          (cons item (shuffle (remove item list)))))))

;------------------------------------------------------
;Função que deleta um elemento de uma lista

(define delete
  (lambda (item list)
    (cond
     ((equal? item (car list)) (cdr list))
     (else (cons (car list) (delete item (cdr list)))))))

;----------------------------------------------------------
;Definição da sintaxe do while

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

;----------------------------------------------

;Definição executa

(define executa
  (lambda (arquivo PalavraEmbaralhada)
     (define p (first arquivo))
     (define a (string->list p))
     (define PalavraEmbaralhada2 " ")
    ;(print arquivo)
  (define PalavrasAcertadas '())
  (let ()
     (while (not(empty? arquivo))
         (display "Palavras já encontradas: ")
         (displayln PalavrasAcertadas)
         (display "Score: ")
         (displayln score)
         (display PalavraEmbaralhada)
         (displayln "           DIGITE 1 PARA EMBARALHAR A PALAVRA")
         (define u(read-line))
           (if(equal? u "1")
              (begin
                 (set! PalavraEmbaralhada (list->string (shuffle a)))
                
              )(begin
               
                 (if (member u arquivo)
                     (begin
                       (println "ACERTOU!")
                       (newline)
                       
                       (set! score(+ score 10))
                       (set! PalavrasAcertadas(append PalavrasAcertadas (list u)))
                       (set! arquivo(delete u arquivo))
                       )
                     (begin
                       (if(member u PalavrasAcertadas)
                          (begin
                            (println "ESSA PALAVRA JÁ FOI ENCONTRADA!!")
                            (newline)
                            )
                          (begin
                            (println "ERROU!")
                            (set! score (- score 5))
                            (newline)
                           )
                       )
                      )
                  )
               )
           )
         )
     )
    )
)



;---------------------------------------------    

(define abrirArquivo
  (lambda (arquivo nomearq)
  
     ;Abrindo arquivo
     (define x (file->lines nomearq))
    
     (set! arquivo (append arquivo x) )
    
      ;Pegando palavra principal
     (define PalavraPrincipal (first arquivo))

      ;quebrando palavra em letras para embaralhar
     (define aux (string->list PalavraPrincipal))

      ;embaralhando palavra
     (define PalavraEmbaralhada (list->string (shuffle aux)))
     ;(print PalavraEmbaralhada)


    (executa arquivo PalavraEmbaralhada)
    
  )
)

(define score 0)


;nomeArquivos



(define main
 (lambda()
   (define nomeArquivos (shuffle (file->lines "Arquivos.txt")))
   (define arquivo '())
   (set! score 0)
   (let ()
     (while (not(empty? nomeArquivos))

            (define nomearq (first nomeArquivos))

            (abrirArquivo arquivo nomearq)

            (set! nomeArquivos(delete nomearq nomeArquivos))
            (displayln "PARABÉNS, VOCÊ ACERTOU TODAS AS PALAVRAS!\n")
            ;(display nomeArquivos)
     )
   )

   
   (displayln "FIM DE JOGO!")
   (displayln "DIGITE: 1 - JOGAR NOVAMENTE  2 - SAIR")
   (define menu (read-line))
   (if (equal? menu "1")(main) 'saindo)

   
   

 )
)

(main)





