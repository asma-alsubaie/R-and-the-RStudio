#10-sided dice.
sided.10sided<-sample(1:10, replace = FALSE)
sided.10sided

# function to roll the dice 6 times .
sided.10sided.sum <- function(){
  dice <- sample(1:10, size=1,replace = TRUE)
  return(sum(dice))
}
replicate(n=6, expr=sided.10sided.sum())



#20-sided dice.
sided.20sided<-sample(1:20, replace = FALSE)
sided.20sided

# function to calculate how many dice rolled more than 6 (for the 10-sided) 
#or 16 (for the 20-sided)
sided.20sided.sum <- function(){
  dice <- sample(1:20, size=1,replace = TRUE)
  return(sum(dice))
}
replicate(n=16, expr=sided.20sided.sum())