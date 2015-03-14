## Inverse matrix with caching

## Returns matrix that is able to cache its inverse (as a list)
## $get, $set - gets and sets matrix itself
## $getsolve, $setsolve - retrieve and update cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Finds inverse matrix and stores result in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()

        if (!is.null(s)) {
                ## Found cache - return it
                message("getting cached data")
                return(s)
        }
        ## No cache found - find inverse and save it
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
