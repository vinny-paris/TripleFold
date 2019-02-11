#' Test for optimal rotation vectors
#' 
#'  For some set of generators, this function will show what the corresponding effect the rotation vector will have on it. Most importantly if the effects level come out to be 0 then it is rotationally tolerant.
#'  
#' @export
#'   
#' @param  g3 A matrix. This is the coded psuedo-design matrix with each element being 0, 1, or 2. Each row corresponds to one generator.
#' @param rotation_vector This is the rotation vector that is to be evaluated. 
#' 
#' @return A List. 
#' \item{rotation vector}{This is the vector you put in from the parameters}
#' \item{output}{A data frame with one column being the effect and the second col being the level}
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' g3 <- c(1, 2, 1, 0, 0, 0,
       #'       0, 0, 1, 0, 2, 2,
       #'       0, 0, 1, 1, 0, 1,
       #'        0, 0, 1, 2, 1, 1,
       #'        0, 0, 0, 1, 1, 2)
#' g3 <- matrix(q, nrow = 5, byrow = TRUE)
#' checker(g3, c(2,1,1,0,1,1))
#' }
#' 
#' 
#' 

checker <- function(g3, rotation_vector){
  if(sum(class(g3) == c("matrix", "data.frame")) == 0 ) {stop("Please give the design as a matrix or data.frame!", call. = FALSE)}
  if(sum(sort(unique(as.vector(g3))) == c(0, 1, 2)) != 3) {stop("Please code the matrix with 0, 1 and 2's only!")}
  if(dim(g3)[2] != length(rotation_vector)) {stop("Please make the rotation vector have the same number of factors as the design matrix!")}
  if(sum(class(rotation_vector) == c("numeric", "vector")) == 0) {stop("Please make the rotation vector a numeric vector!")}
  
  
  
  
  q <- g3
  x <- rotation_vector
  q %*% x -> l
  l <- l %% 3
  
  
  
  num_of_vars <- dim(q)[2]
  holding <- NULL
  
  for(i in 1:dim(q)[1]){
    e <- q[i,]
  s <- data.frame(c(1:num_of_vars), e + 1, row.names = NULL)
  j <- paste(latin[as.matrix(s)], collapse = "")
  j <- substr(j, 1, nchar(j) -1)
  holding <- rbind(holding, j)
  }
  
  h <- suppressWarnings(data.frame(holding, l))
  names(h) <- c('Effect', 'Level')
  
  h <- list(x, h)
  
  names(h) <- c('rotation vector', 'output')
  return(h)

  
}









