library(tidyverse)
library(magrittr)
library(text2vec)
library(doFuture)
library(doParallel)
library(tm)

registerDoFuture()
plan(multiprocess)

stop_words <- stopwords('pt')

preprocessamento_texto <- function(texto) {
  
  texto %>%
    gsub('[[:cntrl:]]', "", .) %>%
    gsub("http\\S+\\s*", "", .) %>%
    gsub("\\d+", "", .) %>%
    gsub("[^[:graph:]]", " ", .) %>%
    #gsub(",", " ", ., fixed = TRUE) %>%
    #gsub(".", " ", ., fixed = TRUE) %>%
    
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>%
    
    # collapse multiple spaces
    str_replace_all("\\s+", " ") %>%
    
    # make text lower case
    str_to_lower
}

preprocessamento_token <- function( texto ) {
  
  stringr::str_split( texto, pattern = stringr::boundary("word") )
  
}

funcao_criar_dtm <- function(id_texto, texto, prep_fun, token_fun, min_ngram, max_ngram, minTermfreq, maxTermfreq, n_process){
  
  registerDoParallel( n_process )
  
  ### Vocabulario
  
  iterador <- itoken_parallel( texto, 
                               preprocessor = prep_fun, 
                               tokenizer = token_fun, 
                               ids = id_texto,
                               n_chunks = n_process,
                               progressbar = FALSE )
  
  
  vocabulario <- create_vocabulary( it = iterador, 
                                    ngram = c(min_ngram, max_ngram), 
                                    stopwords = stop_words ) %>% 
    prune_vocabulary( doc_proportion_min = minTermfreq, doc_proportion_max = maxTermfreq )
  
  vocabulario %<>% filter( nchar(term) >= 3 )
  
  
  ### Obter DTM
  
  dtm <- create_dtm( iterador, vocab_vectorizer( vocabulario ) )
  
  ## Continuar daqui
  results <- NULL
  results$dtm <- dtm
  results$vocabulario <- vocabulario
  
  return( results )
  
}


funcao_aplicar_dtm <- function(id_texto,
                               texto,
                               vocabulario,
                               prep_fun,
                               token_fun,
                               min_ngram,
                               max_ngram,
                               n_process){
  
  registerDoParallel( n_process )
  
  ### Vocabulario
  
  iterador <- itoken_parallel( texto,
                               preprocessor = prep_fun,
                               tokenizer = token_fun,
                               ids = id_texto,
                               n_chunks = n_process,
                               progressbar = FALSE )
  
  ### Obter DTM
  
  dtm <- create_dtm( iterador, vocab_vectorizer( vocabulario ) )
  
}


dados <- read_csv2( 'EAeDW_2TBDA_BIGFREIRE_AM2s2018/conteudo_2.csv', 
                    locale = locale(encoding = 'latin1') )

dados %<>% 
  mutate( soup = paste( materia, tema, serie, classificacao) )


dtm_geral <- funcao_criar_dtm( texto = dados$soup,
                                       id_texto = dados$id,
                                       prep_fun = preprocessamento_texto,
                                       token_fun = preprocessamento_token,
                                       min_ngram = 1,
                                       max_ngram = 1,
                                       minTermfreq = 0.005,
                                       maxTermfreq = 1,
                                       n_process = 2 )


# escolher a materia
materias <- dados %>% distinct( materia)

mat <- menu( materias$materia, title = 'Escolha a materia:' )

mat <- materias$materia[mat]


# escolher midia
series <- dados %>% distinct( serie )

seri <- menu( series$serie, title = 'Qual sua serie?' )

seri <- series$serie[seri]

# escolher tema
classif <- readline( "O que você gosta de fazer? " )

# escolher tema
temas <- readline( "Qual seu tema de pesquisa? " )

preferencias <- data_frame( id = 1,
                            soup = paste( mat, seri, classif, temas ) )

dtm_preferencias <- funcao_aplicar_dtm(id_texto = preferencias$id,
                                                 texto = preferencias$soup,
                                                 vocabulario = dtm_geral$vocabulario,
                                                 prep_fun = preprocessamento_texto,
                                                 token_fun = preprocessamento_token,
                                                 min_ngram = 1,
                                                 max_ngram = 1,
                                                 n_process = 1)

similaridades <- sim2( x = dtm_preferencias,
                       y = dtm_geral$dtm,
                       method = "jaccard",
                       norm = "none" )

max( similaridades[1,] )
id_conteudo <- names( which.max( similaridades[1,] ) )

similaridades %<>% 
  as.matrix() %>% 
  t()

conteudos <- data_frame( id_conteudo = rownames(similaridades),
            similaridades = c(similaridades) ) %>% 
  arrange( desc(similaridades) ) %>% 
  head(3)


dados %>% 
  filter( id %in% conteudos$id_conteudo ) %>% 
  select( id, titulo ) %>% 
  show

