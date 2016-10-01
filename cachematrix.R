## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function contains getters and setters for the matrix creation as well as for obtaining matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m<<- solve
        getInverse <- function() m
        list(get = get,set = set,getInverse = getInverse,setInverse = setInverse)
        
}


## Write a short comment describing this function
## This function will first fetch the inverse and if it is null then it will fetch the matrix and obtain its inverse otherwise it will return the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting the cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
