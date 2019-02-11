#' Creates a 3^(f-s) Regular Fraction
#' 
#' This will create a regular fractional factorial design by solving a system of generating equations.
#' 
#' @param idss This is the identifiers for the design factors. It should be a vector with each element being a character string or reducable to a character string. Needs to be of f length.
#' @param l This is the matrix system of equations that the new design is to be generated from. It is a matrix of f width and s length with each element being 0, 1 or 2. Each row corresponds to one generation equation.
#' @param trees A vector of numbers that is 0, 1, or 2 which will be the solution to equation from l (i.e. 1*x + 2*y + 0*z mod 3 = tree). This dictates the fraction you want.
#' 
#' @return A list will be returned. 
#' \item{Short Design}{A matix. This will return all relevant factors and levels as solutions to the design discluding those factors which simply "cycle through" the design matrix.}
#' \item{Expansion Variables}{A vector. The short design should be copied three times, with a new column for the expansion variable. For each copy increase the expansion variables level by one starting at 0. Continue to grow as needed for as many variables as there are in this category.}





part <- function(idss, l, trees){

  
  #create the correct coef.'s as l_* 
  n <- ifelse(is.vector(l) == TRUE, dim(as.data.frame(l))[1], dim(as.data.frame(l))[2])
  
  if(length(idss) != n) stop("Number of variables does not match number of coefs; please correct!")
  

  
  needed <- colSums(abs(l)) != 0
  n <- sum(needed)
  l <- l[, needed]
  x_short <-idss[needed]
  x_useless <- idss[!needed]
  holding <- NULL
  
  for (j in 1:n){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(n-j)))
    holding <- cbind(holding, y)
  }
  
  colnames(holding) <- x_short
  
  
  
  goal <- function(vec, l, trees){
    
    sol <- rowSums(l %*% (vec)) %% 3 - trees 
    
    ifelse(sum(abs(sol)) == 0, TRUE, FALSE)
  }
  
  
  branches <- apply(holding, 1, goal, l = l, trees = trees)
  
  cut <- list(holding[branches, ], x_useless)
  
  names(cut) <- c("Short Design", "Expansion Variables")
  
  
  return(cut)
}




