# Pacotes

library(gtools)

# slide 3
permutations( v = c('preta', 
                    'vermelha', 
                    'amarela'), # vetor de elementos
              n = 3, # numero de elementos
              r = 3, # tamanho dos grupos de permutacoes
              repeats.allowed = F # repeticao ou nao repeticao
            )


formula_permutacao <- function( n, r, repeticao = T ){
  
  if( repeticao == T ){
    
    n^r #formula com repeticao
    
  } else {
    
    factorial(n) / factorial(n-r) #formula sem repeticao
    
  }
  
}

formula_permutacao( n = 3, r = 3, repeticao = F )



# slide 4

combinations( v = c('verde', 
                    'vermelha', 
                    'roxo',
                    'azul'), # vetor de elementos,
              n = 4, # numero de elementos
              r = 2, # tamanho dos grupos de combinacoes
              repeats.allowed = F # repeticao ou nao repeticao
            )


formula_combinacao <- function( n, r, repeticao = T ){
  
  if( repeticao == T ){
    
    factorial(n + r - 1) / ( factorial(r) * factorial(n-1) ) #formula com repeticao
    
  } else {
    
    factorial(n) / ( factorial(r) * factorial(n-r) ) #formula sem repeticao
    
  }
  
}

formula_combinacao( n = 4, r = 2, repeticao = T)



# slide 5 e 6

formula_permutacao( n = 10, # 10 maneiras de arranjar
                    r = 6, # escolher 6 numeros  
                    repeticao = F # nao pode repetir o numero 
                  )

# slide 7 e 8

formula_combinacao( n = 60, # 60 maneiras de arranjar 
                    r = 6, # escolher 6 numeros
                    repeticao = F # nao pode repetir o numero
                  )

# slide 9 e 10

formula_combinacao( n = 8, 
                    r = 4, 
                    repeticao = F )







