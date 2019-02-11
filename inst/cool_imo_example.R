d <- c(0,0,0,0,
       1,0,1,1,
       2,0,2,2,
       0,1,2,1,
       1,1,0,2,
       2,1,1,0,
       0,2,1,2,
       1,2,2,0,
       2,2,0,1)

design <- matrix(d, nrow = 9, byrow = TRUE)

#What is aliased?
what_frac(design)

#How should I "rotate?"
opt_rotation(design)

#What is the new Design?
triple_fold(design, c(1,0,0,0))
news

#What is stil aliased?
what_frac(news[[2]])
