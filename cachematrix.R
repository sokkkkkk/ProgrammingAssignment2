## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # store the inversion result
        set <- function(y){ # set a matrix to object created by this function
                x <<- y
                inv <<- NULL
        }
        get <-function() x # retrun the input matrix 
        setinv <- function(inverse) inv <<- inverse # set the inversed matrix
        getinv <- function() inv # return the inversed matrix 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() # get the inversed matrix from x 
        if(!is.null(inv)){ # check if the inversion result been calculated 
                message("getting cached data")
                return(inv) # return the calculated inversion 
        }
        data <- x$get() # get the maxtrix object
        inv <- solve(data, ...) # use solve function compute the inverse 
        x$setinv(inv) # set inversed matrix to the object
        inv # return the result
}
