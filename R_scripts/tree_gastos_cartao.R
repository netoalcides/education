library(tidyverse)
library(magrittr)
library(rpart)
library(visNetwork)



dados_cartao <- read_csv( file = "/home/vm-data-science/dados/base_gastos_cartao.csv")

tree <- rpart( formula = gastos_cartao ~ ., 
                data = dados_cartao)

tree %>% 
  visTree(., rules = TRUE,
          fallenLeaves = TRUE,
          width = "100%",
          height = "850" ) %>% 
  visOptions(highlightNearest = TRUE, 
             nodesIdSelection = TRUE)

predict( tree )

if (class(tree) != "rpart") {
  warning(class(tree), " is not supported.Please use an rpart model.")
  stop()
}

capture.output({rpartRules <- path.rpart(tree,rownames(tree$frame)[tree$frame$var=="<leaf>"])})
ruleTexts <- "IF "
operators <- c("<=",">=","<",">","=")
i <- 1

mapping_table <- data.frame( labels = tree$frame$yval, node = rownames(tree$frame) )

for (rule in rpartRules) {
  ruleText <- character(0)
  for (component in rule) {
    whichSeparator <- lapply(operators, function(x) length(unlist(strsplit(component,x)))) > 1
    parts <- unlist(strsplit(component,(operators[whichSeparator])[1]))
    if(!(parts[1]=="root")) {
      if (is.finite(suppressWarnings(as.numeric(parts[2])))) {
        ruleText <- c(ruleText,paste("[",parts[1],"]",(operators[whichSeparator])[1],parts[2],sep=""))
      } else {
        ruleText <- c(ruleText,paste0(" (",paste0("[",parts[1],"]","='",unlist(strsplit(parts[2],",")),"'",collapse=" OR "),")"))
      }
    }
  }
  
  ruleTexts <- c(ruleTexts, 
                 paste0( if(length(ruleTexts)>1){"\nELSEIF "}, 
                         paste(ruleText,collapse=" AND "),
                         " THEN '" ,
                         mapping_table[mapping_table$node==as.numeric(names(rpartRules)[i]),]$labels,"'" ) )
  
  if(i==length(rpartRules)) ruleTexts <- c(ruleTexts,"\nEND")
  
  i <- i +1
  
}
tableauFormula <- paste(ruleTexts,collapse=" ")
tableauFormula



IF  [renda] < 3400 AND [renda] < 2720 AND [idade] < 32.5 THEN '473.5' 
ELSEIF [renda] < 3400 AND [renda] < 2720 AND [idade] >= 32.5 THEN '516.969696969697' 
ELSEIF [renda] < 3400 AND [renda] >= 2720 THEN '564' 
ELSEIF [renda] >= 3400 AND [renda] < 4840 AND [renda] < 4120 AND [idade] < 30.5 THEN '605.454545454545' 
ELSEIF [renda] >= 3400 AND [renda] < 4840 AND [renda] < 4120 AND [idade] >= 30.5 THEN '653' 
ELSEIF [renda] >=3400 AND [renda] < 4840 AND [renda] >= 4120 THEN '660.4' 
ELSEIF [renda] >= 3400 AND [renda] >= 4840 THEN '757.777777777778' 
END






