## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() will create a special object that will enable the capture of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #set() assign the x matrix to the x variable in the environment
        set <- function(y) {
                #look up variables in the next environment (<<- vs. <-) 
                #x <- y  #Let's break it - why does it break when there is a single less than symbol? 
                #This is due to Lexical scoping. The "set" function environment is different than 
                #the makeCacheMatrix()
                x <<- y
                inv <<- NULL
        }
        #get() Give access to the matrix which is stored in the makeCacheMatrix environment
        get <- function() x
        #setInverse() assign the inverse matrix to the inv variable
        setInverse <- function(invertedMatrix) inv <<- invertedMatrix
        #getInverse() give access to the inverted matrix
        getInverse <- function() inv
        #This list is an object that contains the behaviors defined
        #in the functions above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #Assume that this matrix can be inverted
        inv <- x$getInverse()
        if(!is.null(inv)) { #If the inverse is cached -return it without processing
                message("getting cached matrix")
                return(inv)
        }
        #We know the inverse is not cached - so recompute
        data <- x$get()
        inv <- solve(data, ...)
        #Cache the data
        x$setInverse(inv)
        #Return the inverse matrix
        inv
}
