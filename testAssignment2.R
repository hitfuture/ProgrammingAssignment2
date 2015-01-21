#install.packages("testthat")
library(testthat)
source("cachematrix.R")

#We're just 
context("Basic Inversion")
mat<-matrix(c(2,3,4,5),2,2)

expect_equal(dim(mat),c(2,2))
inverseMat <- solve(mat)
#Create a 2 by 2 identity matrix
identity2by2<-matrix(c(1,0,0,1),2,2)
identityMat <- round(mat %*% inverseMat,1)

expect_equal(identity2by2,identityMat,expected.label="Multiplying a matrix by its inverse should end up with identity matrix")

context("Test the cache Matrix functions")
aList<-makeCacheMatrix(mat)
expect_is(aList$get,"function")
expect_is(aList$set,"function")
expect_is(aList$getInverse,"function")
expect_is(aList$setInverse,"function")
context("Let's test to see that the list is correctly saving the matrix")
aList <- makeCacheMatrix(NULL)
expect_equal(aList$get(),NULL)
expect_equal(aList$getInverse(),NULL)
expect_equal(aList$set(mat),NULL)
expect_equal(aList$get(),mat)
expect_equal(cacheSolve(aList),inverseMat)
aList$setInverse(NULL)
expect_equal(aList$getInverse(),NULL)
cacheSolve(aList)
expect_equal(aList$getInverse(),inverseMat)
expect_equal(cacheSolve(aList)%*%aList$get(),identity2by2)

 