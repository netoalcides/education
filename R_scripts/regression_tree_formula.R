tree <- rpart( mpg ~ .,
               data = mtcars)

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


# IF  [cyl] >= 5 AND [hp] >= 192.5 THEN '13.4142857142857' 
# ELSEIF [cyl] >= 5 AND [hp] < 192.5 THEN '18.2642857142857' 
# ELSEIF [cyl] < 5 THEN '26.6636363636364' 
# END






