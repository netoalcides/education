# pacotes
library(recommenderlab)
library(tidyverse)

# https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf

# dados
ratings <- read_csv(file = "education/dados/ml-latest-small/ratings.csv")
movies <- read_csv( file = "education/dados/ml-latest-small/movies.csv" )
movieRatings <- ratings %>% 
  left_join(., movies,
            by = "movieId" )

# recommender matrix

movies <- movieRatings %>% 
  count( movieId ) %>% 
  mutate( perc = n/610) %>% 
  arrange( desc(n) ) %>% 
  filter( perc > 0.2 )

users <- movieRatings %>% 
  count( userId ) %>% 
  mutate( perc = n/9725) %>% 
  arrange( desc(n) ) %>% 
  filter( perc > 0.005 )

movieRatings_matrix <- movieRatings %>%
  filter( movieId %in% movies$movieId ) %>% 
  filter( userId %in% users$userId ) %>% 
  mutate( rating = ifelse( rating > 2.5, 1, 0 ) ) %>% 
  select(userId, movieId, rating) %>% 
  spread( key = movieId, value = rating, fill = 0 ) 



# model train - test

train_df <- movieRatings_matrix %>% 
  sample_frac( 0.8 )

test_df <- movieRatings_matrix %>% 
  filter( !userId %in% train_df$userId )

train <- train_df %>% 
  select( -userId ) %>% 
  as.matrix() %>% 
  as(., "binaryRatingMatrix" )

test <- test_df %>% 
  select( -userId ) %>% 
  as.matrix() %>% 
  as(., "binaryRatingMatrix" )


# # train
# 
# recommender_models <- recommenderRegistry$get_entries(dataType ="binaryRatingMatrix")
# recommender_models$IBCF_binaryRatingMatrix$parameters
# 
# method <- 'IBCF'
# parameter <- list(method = 'Jaccard', k = 10)
# parameter <- list(method = 'Cosine', k = 10)
# n_recommended <- 5
# 
# recc_model <- Recommender(data = train, method = method, parameter = parameter)
# model_details <- getModel(recc_model)
# 
# # test
# 
# recc_predicted <- predict( object = recc_model, 
#                            newdata = test,
#                            n = n_recommended, 
#                            type = "topNList" )
# 
# pred <- recc_predicted@items %>% 
#   data.frame()
# 
# colnames(pred) <- test_df$userId
# 
# movies_test <- test@data@itemInfo %>% 
#   as_tibble() %>% 
#   mutate( id = row_number(),
#           labels = as.numeric(labels) )
# 
# pred %<>% 
#   gather( key = userId, value = recommendations ) %>% 
#   as_tibble() %>% 
#   mutate( userId = as.numeric(userId) ) %>% 
#   left_join(., movies_test,
#             by = c( 'recommendations' = 'id') ) %>% 
#   rename( movie_recs = labels ) %>% 
#   select( -recommendations )
# 
# pred
# 
# 
# tt <- test_df %>% 
#   gather( key = movieId, value = rating, -userId ) %>% 
#   filter( rating == 1 ) %>% 
#   arrange( userId )
# 
# for ( i in unique(test_df$userId) ){
#   
#   real <- tt %>% 
#     mutate( movieId = as.numeric(movieId) ) %>% 
#     filter( userId == i )
#   
#   predicted <- pred %>% 
#     filter( userId == i )
#   
#   real %>% 
#     left_join(., predicted,
#               by = c('movieId' = 'movie_recs') ) %>% 
#     count(userId.y) %>% 
#     print()
#   
# }

model_data <- movieRatings_matrix %>% 
  select( -userId ) %>% 
  as.matrix() %>% 
  as(., "binaryRatingMatrix" )

e <- evaluationScheme( model_data, 
                       method = "split", 
                       train = 0.8,
                       k = 1, 
                       given = 1 )
e

parameter <- list(method = 'Jaccard', k = 10)

r <- Recommender( getData(e, "train"), "IBCF", parameter = parameter )

p <- predict(r, getData(e, "known"), type="topNList", n=10)
p

p@items[4] 

calcPredictionAccuracy(p, getData(e, "unknown"), given = 5 )

movieRatings_matrix %>% 
  slice(13) %>% 
  gather( key = movieId, value = rating, -userId ) %>% 
  filter( rating == 1 ) %>% 
  print(n=Inf)

# testar <- movieRatings_matrix[, -1]

testar[, p@items[[4]] ] %>% colnames()

