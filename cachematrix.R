## makeCacheMatrix returns a list of functions which enables to store a matrix and a cached value of the inverse of this matrix

makeCacheMatrix <- function(x = numeric()) {

    cache <- NULL

    setMatrix <- function(value) {
                 x <<- value
                 cache <<- NULL
    }

    getMatrix <- function() {
                 x
    }

    cacheInverse <- function(solve) {
                    cache <<- solve
    }                

    getInverse <- function() {
                  cache
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
    
}


# This function returns the inverse of the matrix created with the makeCacheMatrix

cacheSolve <- function(y, ...) {

    inverse <- y$getInverse()
    if(!is.null(inverse)) {
                message("fetching cache data")
                return(inverse)
    }
    
    data <- y$getMatrix()
    inverse <- solve(data)
    y$cacheInverse(inverse)
    
    inverse
    
}
