##################################
#### programming assignment 2 ####
##################################

# Example: Caching the mean of a vector 

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y   ## <<- assign a value to an object in an environment that is different from the current one 
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


## Caching the inverse of a matrix 
## Matrix inversion if usually a costly computation, and 
## there may be some benefit to caching the inverse matrix 
## instead of computing it repeatedly. 
## Below are a pair of functions to cache the inverse of a matrix 

## The first function, makeCacheMatrix creates the special "matrix" to cashe the inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL 
    set <- function(y) {
        x <<- y 
        inverseMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
    
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## the cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getinverse()
    if (!is.null (inverseMatrix)) {
        message ("getting cached data") 
        return(inverseMatrix)
    }
    mat <- x$get()
    inverseMatrix <- solve(mat, ...)
    x$setinverse(inverseMatrix)
    inverseMatrix
}


## test the pair of functions

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
