tree_rules <- function( model_tree, model_type ){
  
  
  tree = model_tree
  # observa a classe de modelo
  if (class(tree) != "rpart") {
    warning(class(tree), " is not supported.Please use an rpart model.")
    stop()
  }
  
  # verifica o modelo
  if ( !model_type %in% c( "regression", "classification" ) ) {
    warning(model_type, " is not supported. Please, use only 'regression' or 'classification' ")
    stop()
  }
  
  # ajustes
  capture.output({rpartRules <- path.rpart(tree,rownames(tree$frame)[tree$frame$var=="<leaf>"])})
  ruleTexts <- "IF "
  operators <- c("<=",">=","<",">","=")
  i <- 1
  
  # regressao ou classification
  if( model_type == 'regression' ){
    
    mapping_table <- data.frame( labels = tree$frame$yval, node = rownames(tree$frame) )
    
  } else{
    
    levelToCategory <- data.frame(labels=attr(tree,"ylevels"),catkey=rep(1:length(attr(tree,"ylevels"))))
    nodeToLevel<-data.frame(catkey=tree$frame$yval,node=rownames(tree$frame))
    mapping_table<-merge(levelToCategory,nodeToLevel,by='catkey')[,2:3]
    
  }
  
  # cria as regras
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
  
  rules_constructed <- paste(ruleTexts,collapse=" ")
  return(rules_constructed)
  
}


