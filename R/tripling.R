#'Creates a New Design Matrix with Generating Equations
#' 
#' This function takes in a system of equations and an orginal design matrix. The output will be a new design matrix of three times the size of the orginal. This acts very similar to triple_fold but how it goes about exapnding the orgiinal design matrix is different in view points (but can be shown to be exactly equivalent).
#' 
#' 
#'@param design_matrix The original design matrix that you would like expanded upon, must come in coded with 0,1,2's
#'@param l The system of equations. There must be f-1 equations for f being the number of factors. This should come in as a matrix with a unique, independent equation on each row. Each element of each equation must be coded 0, 1, or 2. 
#' 
#'@return  This will return a list of two parts
#'\item{Aliased_with_Fraction}{These are the effects that are still confounded with the intercept}
#'\item{Design_Matrix}{This is a matrix that is coded 0,1,2 that is the triple foldover created new Design Matrix that is 3 times the size of the orginal design matrix.}
#' 
#' 
#'@examples
#'\donttest{
#' design <- matrix(c(1,2,1,0,0,1,1,0,1,2,2,0), byrow = TRUE, nrow = 3)
#' x <- matrix(c(1,0,2,1, 0,1,2,0, 1,1,2,1), nrow = 3, byrow = TRUE)
#' tripling(design, x)
#' }
#' 


tripling <- function(design_matrix, l){
  x <- design_matrix
  f <- dim(x)[2]
  n <- dim(x)[1]
  l_n <- dim(l)[1]
  
  holding <- NULL
  for (j in 1:f){
    y <- kronecker(rep(c(0,1,2), 3^(j-1)), rep(1, 3^(f-j)))
    holding <- cbind(holding, y)
  }
  
  m <- dim(holding)[1]
  
  qq <- (l %*% t(holding)) %% 3
  
  
    q <- (l %*% t(x)) %% 3
   
  jig <- dim(q)[2]
  
  h1 <- NULL
  h2 <- NULL
  h3 <- NULL
  
  for(i in 1:jig){
    ali <- (1:m)[colSums(q[,i] == qq) == l_n]
    g1 <- holding[ali[1], ]
    g2 <- holding[ali[2], ]
    g3 <- holding[ali[3], ]
    h1 <- rbind(h1, g1)
    h2 <- rbind(h2, g2)
    h3 <- rbind(h3, g3)
  }
  
  
  y <- rbind(h1, h2, h3)
  aliased <- what_frac(y)
  colnames(aliased) <- NULL
  
  helper <- list(aliased,  y)
  
  names(helper) <- c('Aliased_with_Fraction',  'design_matrix')
  
  return(helper)
  

  
  
  }



