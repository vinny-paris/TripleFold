#' All Factor Combinations 
#' 
#' This will produce a design matrix for a non-fractional factorial experiment with n number of factors at 3 levels
#' 
#' @param n A natural number, the number of factors in the experiment
#' @return Returns a matrix with n columns. All entries will be 0, 1, or 2. This will be the "perfect" design matrix for an experiment with these many factors.

alias_design <- function(n){

  
  books <- NULL
  for(i in 1:n){
    
    col <- c(kronecker(c(rep(c(0,1,2), alias_dim_calc(n-i)-1), 1), rep(1, 3^(i - 1))), rep(0, (3^(i-1) + 1)/2 - 1))
    
    books <- cbind(col, books)
    
    
    
  }
    

  return(books)
}
  
  
  
  
  
  
  
  
  
