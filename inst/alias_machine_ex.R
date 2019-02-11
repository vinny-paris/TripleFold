#Alias Machine

l <- c(0,1,2)
alias_machine(3,l)

l <- matrix(c(1,1,0,2,1,1,2,0), ncol = 4, byrow = TRUE)
alias_machine(4,l)

l <- matrix(c(1,2,0,0,0,1,2,0), ncol = 4, byrow = TRUE)
alias_machine(4, l)

l <- c(1,0)
alias_machine(2, l)

l <- matrix(c(1,2,2,0,0,1,2,0, 1, 1), ncol = 5, byrow = TRUE)
alias_machine(5, l)

l <- matrix(c(1,2,2,0,0,1,2,0, 1, 1, 0, 2), ncol = 6, byrow = TRUE)
alias_machine(6, l)