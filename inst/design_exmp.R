c1 <- c(rep(1, 9), rep(2, 9), rep(3,9))
c2 <- c(rep(c(1,1,1,2,2,2,3,3,3), 3))
c3 <- c(kronecker(c(1,2,3,2,3,1,3,1,2), c(1,1,1)))
c4 <- c(kronecker(c(1,2,3,3,1,2,2,3,1), c(1,1,1)))
c5 <- c(rep(c(1,2,3), 9))

design <- data.frame(c1, c2, c3, c4, c5)
design

what_frac(as.matrix(design))
