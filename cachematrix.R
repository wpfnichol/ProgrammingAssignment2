##I use two functions to creat a object that stores an invertible 
## square matrix and cache the inverse of the matrix.

## First function creats a list containing four functions.
## "inver" is a variable that caches the inverse of x or y.

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(solve) inver <<- solve
        getinver <- function() inver
        list(set = set, get = get, setinver = setinver, getinver = getinver)
}


## The following function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cache data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
        ## Return a matrix that is the inverse of 'x'
}
