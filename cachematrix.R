## This function calculates and caches the inverse of a matrix, storing the
## inverse until the provided matrix changes.

## This function creates a "matrix" object to cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {            ## Set input matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x             ## Retrieves input matrix
        setsolve <- function(solve) m <<- solve  ## Sets inverse matrix
        getsolve <- function() m        ## Gets inverse matrix
        list(set = set, get = get,      ## Create result list
             setsolve = setsolve,
             getsolve = getsolve)
}


## Checks and returns cached inverse solution
## If not, calculates and returns inverse solution

cacheSolve <- function(x, ...) {
        m <- x$getsolve()               ## Retrieves cached solution or NULL
        if(!is.null(m)) {               ## Checks if cached solution exists
                message("getting cached data")
                return(m)               ## Prints cached solution
        }
        data <- x$get()                 ## Calculates the inverse solution
        m <- solve(data, ...) 
        x$setsolve(m)
        m                               ## Returns calculated matrix solution
}
