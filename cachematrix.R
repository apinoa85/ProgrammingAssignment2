## These functions store a matrix object and its 
## respective inverse. In this way, another 
## computation is saved when the inverse of the 
## matrix is required again.

## Function used to store the inverse of a matrix
## and to avoid repeating the same procedure several 
## times. It should be used in conjunction with 
## cacheSolved function.


makeCacheMatrix <- function(matrix = matrix()){
    inverse <- NULL
    
    setmatrix <- function(y){
        matrix <<- y
        inverse <<- NULL
    }
    getmatrix <- function(){
        matrix
    }
    
    getinverse <- function(){
        inverse
    }
    setinverse <- function(inv){
        inverse <<- inv
    }
    
    list(set =set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function uses makeCacheMatrix function to 
## store the computation of the inverse of a matrix.
## If the inverse of the matrix was already computed,
## the inverse is cached from memory, otherwise is 
## computed, stored on memory and returned.


cacheSolve <- function(x, ...){
    inverse <- x$getinverse()
    
    if(is.null(inverse)){
        matrix <- x$getmatrix()
        inverse <- solve(matrix)
        x$setinverse(inverse)
    }
    inverse
}
