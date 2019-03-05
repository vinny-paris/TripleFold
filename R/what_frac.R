#' Alias Structure Identifier  
#' 
#' 
#' Returns what fraction is the matix is split along be warned that it only pays attention to fully aliased situations so if only 2 of three levels appear the function will not flag them you have been warned. 
#' 
#' @export
#' 
#' @param design_matrix This is the design matrix that is of interest. Each column is for the factors and each row for the treatments. Needs to be a matrix or data.frame.
#' 
#' @return This will return a matrix with one column (entitiled Aliased with Intercept) and rows corresponding to the effects confounded with the intercept.









what_frac <- function(design_matrix){
  design <- design_matrix
  if(sum(class(design) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", call. = FALSE)}
  if(sum(sort(unique(as.vector(design))) == c(0, 1, 2)) != 3) {stop("Please code the matrix with 0, 1 and 2's only!")}
  
  
  x <- design_matrix
  
  holding <- NULL
  
  num_of_vars <- dim(x)[2]
  
  possible_equations <- alias_design(num_of_vars)
  
  eqs_length <- dim(possible_equations)[1]
  
  for(i in 1:eqs_length){
    e <- possible_equations[i,]
    k <- uniqueN((x %*% e) %% 3)
    if(k == 1) {
   
   e <- (as.matrix(e))
   ll <- data.frame(c(1:num_of_vars), (e + 1), row.names = NULL)
   nam <- paste(latin[as.matrix(ll)], collapse = "")
   nam <- substr(nam, 1, nchar(nam) -1)
   holding <- rbind(holding, nam)
    }
  }
  
  if(class(holding) == "NULL"){return("No Aliases")}
  rownames(holding) <- NULL
  colnames(holding) <- "Aliased with the Intercept"
  
  return(holding)
}

