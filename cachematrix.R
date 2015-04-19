################################################################################
## Author ; LiRMChip
## Ensemble of functions to compute the inverse of the matrix only when
## needed (the inverse of the matrix is not cached).
################################################################################
## makeCacheMatrix returns a list of functions 
## set the value of a matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

	
	inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(invertedMatrix) inv <<- invertedMatrix

        getinverse <- function() inv
	
	## Return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

################################################################################
## cacheSolve compute the inverse of the special matrix "x" 
## called by x <- makeCacheMatrix(...) or give the result in cache if the matrix 
## to be inversed is already in cache
cacheSolve <- function(x, ...) {

        inv <- x$getinverse()

	## If the inverse is in cache
        if(!is.null(inv)) {
                message("getting cached inverted matrix")
                return(inv)
        }

	## Else compute the inverse
        mat <- x$get()
        inv <- solve(mat)
        x$setinverse(inv)

	## Return inverse
        inv
}


