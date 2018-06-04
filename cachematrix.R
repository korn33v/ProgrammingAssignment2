## Functions below helps to solve matrix using cache for optimization purpose.
##
## Example:
## m <- cbind(c(0,0,1), c(0,1,0), c(1,1,1))
## m_c <- makeCacheMatrix(m)
## cacheSolve(m_c)

## Makes an object which store matrix, solved matrix, getters and setters for them

makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setSolved <- function(y) solved <<- y
        getSolved <- function() solved
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)
}


## Takes an object produced by makeCacheMatrix, try to return solved matrix from cache.
## If cached value is NULL, calculates solved matrix and stores it in cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        solved <- x$getSolved()
        if(!is.null(solved)) {
                message("getting cached data")
                return(solved)
        }
        data <- x$get()
        solved <- solve(data)
        x$setSolved(solved)
        solved
}
