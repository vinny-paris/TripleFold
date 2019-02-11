#' Aliased effects
#' 
#' During a fractional factorial experiment certain effects are fully confounded. This function will tell display the aliased effects for any effect that is still estimable
#' 
#' 
#' @param num_of_vars A natural number. This tells the function how many variables are intended for your design. The limit is 26.
#' @param l This is the system of equations used to generate the aliasing structure. It is to have each row be one "cut" of the factorial design and must have the same number of coefficents (which take the values 0, 1, 2) as x.
#' 
#' @return A matrix. Each row will be an estimable effect and all it's aliases. 
#' 
#' @examples 
#' \dontrun{
#' l <- matrix(c(1,1,0,0,0,1,2,1), nrow = 2, byrow = TRUE)
#' l
#' alias_machine(4, l)
#' }



alias_machine <- function(num_of_vars, l){

  x <- num_of_vars
  
  latin <- TripleFold::latin
  
  sizing <- if(length(dim(l)) != 0) {dim(l)[1]} else{1}
    
    
  if(length(dim(l)) != 0) {
  l <- t(data.frame(l[1,], l[2,], l[1,] + l[2,], l[1,] + 2*l[2,]))
  l <- l %% 3
  jack <- function(x)  if( length(which(x > 1)) == 0) {x} else if(x[which(x > 0)[1]] == 1) {x} else {(2*x) %% 3}
  l <- t(apply((l), 1, jack))
  } 
  
  
  eqs <- alias_design(x)
  
  all <- rbind(eqs, l)
  eqs <- all[!duplicated(all,fromLast = FALSE)&!duplicated(all,fromLast = TRUE),] 
  

  
  
  
  eqs <- eqs[-dim(eqs)[1], ]
  
  
  n_cuts <- ifelse(is.vector(l) == TRUE, dim(as.data.frame(l))[2], dim(as.data.frame(l))[1])
  aa <- LETTERS[1:x]
  final_tab <- NULL
  for(i in 1:(dim(eqs)[1])){
    official <- NULL
    
    # 2 * ncuts just time saver for now, generalize out of x choose something later
    for(j in 1:(n_cuts)){
      
      if(length(dim(l)) == 0) {new_stuff <- (eqs[i,] %% 3) + l} else{
      new_stuff <- (eqs[i,] %% 3) + 1*as.data.frame(l)[j,]}
      better_stuff <- new_stuff %% 3 + 1
      we <- which(better_stuff > 1)
      if(length(we) == 0) {best_stuff <- better_stuff} else {
      best_stuff <- if(better_stuff[which(better_stuff > 1)[1]] == 2)  {better_stuff} else {(2*(better_stuff - 1)) %% 3 + 1}}    
      colly <- unlist(best_stuff) 
      colsss <- cbind(c(1:x), colly)
      
      
      trt <- paste(latin[colsss], collapse = "")
   
      
      
    if(length(dim(l)) == 0) {new_stuff_2 <- (eqs[i,] %% 3) + 2*l} else{
      new_stuff_2 <- (eqs[i,] %% 3) + 2*as.data.frame(l)[j,]}
     
      better_stuff_2 <- new_stuff_2 %% 3 + 1
      we_2 <- which(better_stuff_2 > 1)
      if(length(we_2) == 0) {best_stuff_2 <- better_stuff_2} else {
      best_stuff_2 <- if(better_stuff_2[which(better_stuff_2 > 1)[1]] == 2)  {better_stuff_2} else {(2*(better_stuff_2 - 1)) %% 3 + 1}}    
      colly_2 <- unlist(best_stuff_2) 
      colsss_2 <- cbind(c(1:x), colly_2)
      
      
      
      trt_2 <- paste(latin[colsss_2], collapse = "")
      
      
      first_name <- unlist(eqs[i,] + 1)
      holding <- cbind(c(1:x), first_name)
      orig_name <- paste(latin[holding], collapse = "")
      
         trt <- c(orig_name, trt, trt_2)
         
      official <- unique(c(trt, official))
    }
    final_tab <- rbind(final_tab, official)
    }
  
solution <- matrix(unique(as.vector(t(final_tab))), ncol = 3^(sizing), byrow = TRUE)      

solution <- substr(solution, 1, nchar(solution) - 1)
     
return(solution)
 
}     
      
      

      