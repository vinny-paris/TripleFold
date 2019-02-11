#' Creates a 3^(f-1) Regular Fraction
#' 
#' This will create a regular fractional factorial design by solving a single generating equation.
#' @param x This is the identifiers for the design factors. It should be a vector with each element being a character string or reducable to a character string. Needs to be of f length.
#' @param l This is the single equation that the new design is to be generated from. It is a vector of f length with each element being 0, 1 or 2
#' @param tree A single number that is 0, 1, or 2 which will be the solution to equation from l (i.e. 1*x + 2*y + 0*z mod 3 = tree). This dictates the fraction you want.
#' 
#' @return A list will be returned. 
#' \item{Short Design}{A matix. This will return all relevant factors and levels as solutions to the design discluding those factors which simply "cycle through" the design matrix.}
#' \item{Expansion Variables}{A vector. The short design should be copied three times, with a new column for the expansion variable. For each copy increase the expansion variables level by one starting at 0. Continue to grow as needed for as many variables as there are in this category.}

part_single <- function(x, l, tree){
  
  #create the correct coef.'s as l_* 
  n_long <- length(l)
  
  if(length(x) != n_long) stop("Number of variables does not match number of coefs; please correct!")
  
  needed <- (l != 0)
  n <- sum(needed)
  l <- l[needed]
  x_short <- x[needed]
  x_useless <- x[!needed]
  
  holding <- NULL
  
  for (j in 1:n){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(n-j)))
    holding <- cbind(holding, y)
  }
  
  colnames(holding) <- x_short
  
  
  
  goal <- function(vec, l, tree){
    
    sol <- sum(vec * l) %% 3 - tree 
    
    ifelse(sol == 0, TRUE, FALSE)
  }
  
  
  branches <- apply(holding, 1, goal, l = l, tree = tree)
  
  cut <- list(holding[branches, ], x_useless)
  
  names(cut) <- c("Short Design", "Expansion Variables")
  
  
  return(cut)
}


  