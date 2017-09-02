## Creating a matrix called makeCaheMatrix, which is really a list containing a function to
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                x <<- y
                m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse()
            getinverse <- function() m
            list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve is a function that calculates the inverse of the matrix that we created above.
## First, it checks to see if the inverse has already been calculated.  If yes, it takes the cached mean.
## If no, it calculates the inverse and sets its value in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
