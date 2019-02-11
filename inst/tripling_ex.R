x <- c("a", "b", "c", "d")
l <- matrix(c(1,2,0,1,0,1,2,0), nrow = 2, byrow = TRUE)
trees <- c(1,1)
h <- part(x, l, trees)[[1]]

l <- matrix(c(1,2,0,0,0,1,2,0,0,0,1,1), nrow = 3, byrow = TRUE)


what_frac(h)

what_frac(tripling(h,l)[[5]])




x <- c("a", "b", "c", "d", "e", "f")
l <- matrix(c(1,2,0,1,1,1, 0,1,1,2,0,0, 1,1,1,2,2,0), nrow = 3, byrow = TRUE)
trees <- c(0,0,0)
m <- part(x, l, trees)[[1]]
what_frac(m)

k <- matrix(c(1,2,0,0,0,0, 
              0,1,2,0,0,0,
              0,0,1,2,0,0,
              0,0,0,1,2,0,
              0,0,0,0,1,2), nrow = 5, byrow = TRUE)
what_frac(tripling(m, k)[[5]])





x = c('dog', 'cat', 'bird', 'fish')
l = matrix(c(2, 1,  1, 1, 1, 0, 0, 1), nrow = 2, byrow = TRUE)
trees <- c(1,2)
g <- part(x, l, trees)[[1]]
what_frac(g)
j <- matrix(c(1,2,0,0,
              0,1,2,0,
              0,0,1,2), nrow = 3, byrow = TRUE)
what_frac(tripling(g, j)[[5]])


j <- matrix(c(1,1,0,0,
              0,1,1,0,
              0,0,1,1), nrow = 3, byrow = TRUE)
what_frac(tripling(g, j)[[5]])

what_frac(g)


