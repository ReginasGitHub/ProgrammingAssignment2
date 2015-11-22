## The input to the makeCacheMatrix function should be an invertable matrix
## The output of this function is a list that is made up of four functions: set, get, setinverse and getinverse
## The four functions in the list set and get matrix and set and get the inverse matrix
## That list is used as an input to the next function cacheSolve
## the <<- is used to pull the values outside of a current environment

makeCacheMatrix <- function(x = matrix()) {
        matrixinv <- NULL
        set <- function(y) {
                x <<- y
                matrixinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrixinv <<- inverse
        getinverse <- function() matrixinv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function uses a list from previous fnction as an input
## It calculates the inverse of a matrix that is stored in get function of output list from makeCacheMatrix
## if(!is.null... checks whether or not the inverse of the matrix was already calculated and cached
## If it was calculated then the function uses cached inverse matrix and outputs it together wit the message "getting cahced data"
## If it was not calculated the function calculates the inverse using solve() and outputs the result

cacheSolve <- function(x, ...) {
        matrixinv <- x$getinverse()
        if(!is.null(matrixinv)) {
                message("getting cached data")
                return(matrixinv)
        }
        data <- x$get()
        matrixinv <- solve(data)
        x$setinverse(matrixinv)
        matrixinv
}

## Example:
## test <- makeCacheMatrix(matrix(c(4,3,1,1), nrow=2))
## cacheSolve(test)
## Result
##       [,1] [,2]
## [1,]    1   -1
## [2,]   -3    4
