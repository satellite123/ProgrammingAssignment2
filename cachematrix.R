# to make computations more effective its better to use cache
# function makeCacheMatrix creates a list that contains
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversematr <- function(inverse) inv <<- inverse
    getinversematr <- function() inv
    list(set=set, get=get, setinversematr=setinversematr, getinversematr=getinversematr)
}


#  this function returns the inverse of matrix
# it first checks if the inverse was compuded
# in othercase it inverses it and puts it in cashe with setinversematr 
cacheSolve <- function(x, ...) {
    inv <- x$getinversematr()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinversematr(inv)
    inv
}
