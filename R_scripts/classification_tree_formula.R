# funciona para classification trees
tree <- rpart( Species ~ .,
                 data = iris)

if (class(tree) != "rpart") {
  warning(class(tree), " is not supported.Please use an rpart model.")
  stop()
}

capture.output({rpartRules <- path.rpart(tree,rownames(tree$frame)[tree$frame$var=="<leaf>"])})
ruleTexts <- "IF "
operators <- c("<=",">=","<",">","=")
i <- 1

levelToCategory <- data.frame(labels=attr(tree,"ylevels"),catkey=rep(1:length(attr(tree,"ylevels"))))
nodeToLevel<-data.frame(catkey=tree$frame$yval,node=rownames(tree$frame))
mapping_table<-merge(levelToCategory,nodeToLevel,by='catkey')[,2:3]

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
                 paste0(if(length(ruleTexts)>1){"\nELSEIF "}, 
                        paste(ruleText,collapse=" AND "),
                        " THEN '" ,
                        mapping_table[mapping_table$node==as.numeric(names(rpartRules)[i]),]$labels,"'"))
  if(i==length(rpartRules)) ruleTexts <- c(ruleTexts,"\nEND")
  i <- i +1
}
tableauFormula <- paste(ruleTexts,collapse=" ")
return(tableauFormula)


# IF  [Petal.Length] < 2.45 THEN 'setosa' 
# ELSEIF [Petal.Length] >= 2.45 AND [Petal.Width] < 1.75 THEN 'versicolor' 
# ELSEIF [Petal.Length] >= 2.45 AND [Petal.Width]>=1.75 THEN 'virginica' 
# END