## These functions will Cache the Inverse of a matrix

## This function creates a special "matrix" like object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        # set i to NULL
        i <- NULL
        # set matrix inverse to NULL
        set <- function(y){
        # this function will assing a new value of the matrix x and reset the inverse to NULL
                x<<-y
                i <<- NULL
        }
        get <- function() x
        # the get function returns the matrix
        setinverse <- function(inverse) i <<-inverse
        # setinverse will assign the i to be the inverse in the parent environment
        getinverse <- function() i
        # getinverse will return the inverse of the matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        # stores the properties
}


## This function will return the inverse of a a matrix x using the cached version
## if it has already been found

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached inverse")
                return(i)
        
        }
        else{
        matrix <- x$get()
        i <- solve(matrix,...)
        x$setinverse(i)
        i}
}
