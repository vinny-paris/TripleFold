



x = c('dog', 'cat', 'bird', 'fish')
l = matrix(c(2, 1,  1, 1, 1, 0, 0, 1), nrow = 2, byrow = TRUE)
trees <- c(1,2)

#This is the design matrix for experiment for a regular fraction cut along the generating relationship
#given to it by l, trees. The Short Design is for the more detailed work, the Expansion Variables is
#nothing but a room saver. If a variable (say C) is there then it means to just replicate the "Short 
#Design" three times, one at each level of C. 
part(x, l, trees)

g <- part(x, l, trees)[[1]]
n <- part(x, l, trees)


#This function returns what is aliased with the intercept in the design. There is a big caveat that this
#is only effective for regular fractions. It can take in irregular fractions and return things but it 
#cannot tell the difference between all levels present vs only 2 out of 3 levels present. It always returns 
#things that are completely confounded with the intercept and are constants through the whole design
what_frac(g)


# Fun time!! So x is your original design matrix coded with 0,1,2. The other variable 'expansion' is
#the vector for factor rotations. It must have the same length as the number of factors and in sequential
#ordering you may put in a 0, 1, or 2. A 0 will leave the factor unchanged thorugh the folding process.
#A one will shift the factor up 1 level, and a 2 will shift it up by two levels.
triple_fold(x = g, expansion = c(1,1,1,1))
triple_fold(g, c(1,2,1,1))[[1]]
triple_fold(g, c(1,2,2,1))[[1]]
triple_fold(g, c(1,1,2,1))[[1]]
triple_fold(g, c(2,2,1,1))[[1]]
triple_fold(g, c(1,0,0,0))[[1]]
triple_fold(g, c(1,2,0,0))[[1]]










x <- c("a", "b", "c", "d")
l <- matrix(c(1,2,0,1,0,1,2,0), nrow = 2, byrow = TRUE)
trees <- c(1,1)
h <- part(x, l, trees)[[1]]
part(x, l, trees)
what_frac(h)
triple_fold(h, c(1,2,0,0))[[1]]

triple_fold(h, c(1,0,0,0))[[1]]
triple_fold(h, c(0,1,1,0))[[1]]
triple_fold(h, c(0,1,2,0))[[1]]



x <- c("a", "b", "c", "d", "e", "f", "g")
l <- matrix(c(1,2,0,1,0,0,0,0,1,2,0,1,1,0, 0,1,1,2,0,1,0), nrow = 3, byrow = TRUE)
trees <- c(1,1,1)
h <- part(x, l, trees)[[1]]
h <- cbind(rbind(h, h, h), kronecker(c(1,2,3), rep(1, 27)))
what_frac(h)
triple_fold(h, c(1,0,1,0,0,0,0))[[1]]
triple_fold(h, c(0,1,1,0))[[1]]
triple_fold(h, c(0,1,2,0))[[1]]



x <- c("a", "b", "c", "d", "e", "f")
l <- matrix(c(1,2,0,1,1,1, 0,1,1,2,0,0, 1,1,1,2,2,0), nrow = 3, byrow = TRUE)
trees <- c(0,0,0)
m <- part(x, l, trees)[[1]]
part(x, l, trees)
what_frac(m)

triple_fold(m, c(0,0,1,0,1,1))[[1]]
 triple_fold(m, c(1,0,2,0,0,0))[[1]]
triple_fold(m, c(1,0,0,0,0,0))[[1]]
triple_fold(m, c(1,1,0,1,0,0))[[1]]
triple_fold(m, c(1,1,0,2,0,0))[[1]]














#Used for Claim 3
x <- c("a", "b", "c", "d", "e", "f")
l <- matrix(c(1,2,1,0,0,0,0,0,1,2,1,0,0,0,0,1,1,2), nrow = 3, byrow = TRUE)
trees <- c(0,0,0)
q <- part(x, l, trees)
p <- q[[1]]
what_frac(p)





#Remark on Claim 2
x <- c("a", "b", "c", "d")
l <- matrix(c(1,1,1,0,0,0,0,1), nrow = 2, byrow = TRUE)
trees <- c(0,0)
q <- part(x, l, trees)[[1]]
what_frac(q)

what_frac(triple_fold(q, c(1,1,1,0))[[2]])

