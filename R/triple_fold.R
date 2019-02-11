#' Generating a New Design Matrix
#' 
#' Create the new design matrix from a triple foldover techinque using a rotation vector
#'
#' @export
#' 
#' @param design_matrix The original design matrix that you would like expanded upon, must come in coded with 0,1,2's
#' @param rotation_vector The rotation vector with the same length as the column width of the design matirx. Entries may be 0, 1, or 2.
#' 
#' @return  This will return a list of two parts
#' \item{Aliased_with_Fraction}{These are the effects that are still confounded with the intercept}
#' \item{Design_Matrix}{This is a matrix that is coded 0,1,2 that is the triple foldover created new Design Matrix that is 3 times the size of the orginal design matrix.}
#' 
#' @examples
#' \dontrun{
#' design <- matrix(c(1,2,1,0,1,1,1,2,2), byrow = TRUE, nrow = 3)
#' x <- c(1, 2, 0)
#' triple_fold(design, x)
#' }


triple_fold <- function(design_matrix, rotation_vector){
  
  
  if(sum(class(design_matrix) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", call. = FALSE)}
  if(sum(sort(unique(as.vector(design_matrix))) == c(0, 1, 2)) != 3) {stop("Please code the matrix with 0, 1 and 2's only!")}
  if(dim(design_matrix)[2] != length(rotation_vector)) {stop("Please make the rotation vector have the same number of factors as the design matrix!")}
  if(sum(class(rotation_vector) == c("numeric", "vector")) == 0) {stop("Please make the rotation vector a numeric vector!")}

  
   expansion <- rotation_vector
   x <- design_matrix
   
   x1 <- t((t(x) + expansion) %% 3)
   
   x2 <- t((t(x) + 2*expansion) %% 3)
   
   
   y <- rbind(x, x1, x2)
   
   
   aliased <- what_frac(y)
   colnames(aliased) <- NULL
   
   holding <- list(aliased, y)
   
   names(holding) <- c('Aliased_with_Fraction', 'Design_Matrix')
   
   return(holding)
   
 }
 
