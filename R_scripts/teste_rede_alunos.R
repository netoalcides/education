library(tidyverse)
library(magrittr)
library(lubridate)
library(igraph)
library(visNetwork)



###################################################################
########################## Carrega Dados ##########################
###################################################################
dados_alunos <- read_csv2( file = "/home/vm-data-science/dados/rede_alunos_exemplo_aula_8.csv", 
                           locale = locale( encoding = 'latin1' ) )

dados_alunos %<>% 
  mutate( data = dmy(data) ) %>% 
  filter( depois != '-' ) %>% 
  rename( from = antes,
          to = depois )



###################################################################
############################ Rede Geral ###########################
###################################################################

nodes <- dados_alunos %>% 
  distinct( from ) %>% 
  rename( id = from )


edges_gerais <- dados_alunos %>% 
  group_by( from, to ) %>% 
  summarise( weight = n(),
             value = n() ) %>% 
  ungroup() %>% 
  filter( value > 1 )


visNetwork(nodes, 
           edges_gerais, 
           width = "100%", 
           height = "850",
           main = "Rede geral da turma" ) %>% 
  visEdges(arrows = 'from, to') %>% 
  visPhysics( solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -60) ) %>% 
  visInteraction( multiselect = TRUE) %>%
  visOptions( highlightNearest = list(enabled =TRUE, degree = 1),
              nodesIdSelection = TRUE ) %>% 
  visLayout(randomSeed = 123)

## Metricas

nodes %<>% 
  filter( !id %in% c('Fábio', 'Umberto', 'Cleber' ) )


alunos_network <- graph_from_data_frame( d = edges_gerais,
                                         vertices = nodes, 
                                         directed = F )
as.matrix( alunos_network[] )




nodes %>% dim

mean(degree(alunos_network))
vcount(alunos_network)


edge_density(alunos_network, loops=F)

mean(degree(alunos_network)) / (vcount(alunos_network)-1)

sum(degree(alunos_network)) / ((23*(23-1)))



sum(degree(alunos_network)) / ((23*(23-1))/2)


edge_density(alunos_network, loops=F)*2


degree( alunos_network,
        mode = 'all')


closeness( alunos_network, 
           mode = "all", 
           weights = NA, 
           normalized = T )

betweenness(alunos_network, 
            directed = F, 
            weights = NA, 
            normalized = T)



deg <- degree( alunos_network,
        mode = 'all') %>% 
  dplyr::as_data_frame() %>%  
  rownames_to_column() %>% 
  dplyr::rename( aluno = rowname,
                 degree = value )

close <- closeness( alunos_network, 
           mode = "all", 
           weights = NA, 
           normalized = T ) %>% 
  dplyr::as_data_frame() %>% 
  rownames_to_column() %>% 
  dplyr::rename( aluno = rowname,
                 closeness = value )

betw <- betweenness(alunos_network, 
            directed = F, 
            weights = NA, 
            normalized = T) %>% 
  dplyr::as_data_frame() %>%  
  rownames_to_column() %>% 
  dplyr::rename( aluno = rowname,
                 betweenness = value )


deg %>%
  left_join(., close,
            by = 'aluno' ) %>% 
  left_join(., betw,
            by = 'aluno' ) %>% 
  arrange( desc(degree) ) %>% 
  print(n=Inf)




# deg %>%
#   left_join(., close,
#             by = 'aluno' ) %>% 
#   left_join(., betw,
#             by = 'aluno' ) %>% 
#   mutate( aluno = str_replace_all( string = aluno, 
#                                    pattern = 'Benjamin', 
#                                    replacement = 'Binyamin' ),
#           aluno = str_replace_all( string = aluno, 
#                                    pattern = 'Julia', 
#                                    replacement = 'Sheila' ) ) %>% 
#   rename( qtd_conexoes = degree,
#           nivel_influencia = closeness,
#           nivel_integracao = betweenness ) %>% 
#   write_csv(., path = 'dados/metricas_rede_alunos.csv' )









