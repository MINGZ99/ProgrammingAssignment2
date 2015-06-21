## Programming Assignment 2 for R Programming 

## This function is a framework to cache matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## This function will calculate the Inverse of a matrix if it is not previously cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse ()
        if(!any(is.na(m))) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
