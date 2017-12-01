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
  (lambda (arquivo)
    (print arquivo)
  (define PalavrasAcertadas '())
  (let ()
     (while (not(empty? arquivo))
         (displayln PalavrasAcertadas)
         (define u(read-line))
           (if (member u arquivo)
              (begin
                  (print" acertou!")
                  (newline)
                  (set! PalavrasAcertadas(append PalavrasAcertadas (list u)))
                  (set! arquivo(delete u arquivo))
              )
              (begin
                  (if(member u PalavrasAcertadas)
                     (begin
                         (print "Palavra já foi encontrada!!")
                         (newline)
                     )
                     (begin
                         (print "Errou!")
                         (newline)
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
     (print PalavraEmbaralhada)


    (executa arquivo)
    
  )
)
  

(define arquivo '())

(define nomearq "teste.txt")


(abrirArquivo arquivo nomearq) 







