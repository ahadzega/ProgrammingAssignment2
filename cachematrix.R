## Make a vector that will create a special matrix object
## that can cashe its inverse then compute the inverse if not already calculated

## Create a matrix which gets and sets the value of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x 
        setinverse <- function(inverse) m <<- inverse    
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Compute the inverse if not already computed - if computed get inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting matrix inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
