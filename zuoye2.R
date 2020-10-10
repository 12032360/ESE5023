matrix_1 <- matrix(as.integer(runif(50,0,50)),nrow=5,ncol=10)
matrix_1
matrix_2 <- matrix(as.integer(runif(50,0,50)),nrow=10,ncol=5)
matrix_2
pom <- function(matrix_1,matrix_2){
  m <- nrow(matrix_1)
  n <- ncol(matrix_2)
  s <- matrix(0,m,n)
  for(i in 1:m)
    for(j in 1:n)
      s[i,j] <- sum(matrix_1[i,]*matrix_2[,j])
  return(s)
}
pom(matrix_1,matrix_2)
matrix_1%*%matrix_2


  