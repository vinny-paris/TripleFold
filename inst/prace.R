
     mat <- matrix(c(1,0,1,0,1,0,1,0,
                    1,2,1,2,2,2,2,2,
                    0,0,1,1,2,2,2,1,
                    0,2,1,2,1,1,1,0), nrow = 4, byrow = TRUE)
     
design <- TripleFrac:::part(idss = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'), mat, trees = c(1,1,2,2))[[1]] 

#What is aliased?
what_frac(design)

#How should I "rotate?"
opt_rotation(design)

#What is the new Design?
news <- triple_fold(design, c(1,0,0,0))
news

#What is stil aliased?
what_frac(news[[2]])

