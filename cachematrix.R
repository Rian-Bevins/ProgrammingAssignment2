## Assignment 2: Lexical Scoping

## Creating a special 'matrix' object that can cache its inverse
## makeCacheMatrix uses set, get, setInverse, and getInverse
makeCacheMatrix <- function(x = matrix(sample(1:100, 9), 3, 3)) {
        solveInverse <- NULL
        set <- function(y) {
                x <<- y
                solveInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) solveInverse <- solve
        getinverse <- function() solveInverse
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}

# Computes inverse of special 'matrix', 
# if already solved, get inverse from cache
cacheSolve <- function(x, ...) {
        solveInverse <- x$getinverse()
        if (!is.null(solveInverse)) { ## If inverse is already solved
                message("Getting inversed matrix")
                return(solveInverse) ## Return inverse value from cache
        }
        data <- x$get()
        solveInverse <- solve(data, ...)
        x$setinverse(solveInverse)
        solveInverse
}