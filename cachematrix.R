## Put comments here that give an overall description of what your
## functions do

## This functions get the invers of a matrix, using cached matrix to skip calcultations

## Write a short comment describing this function. 
## makeCacheMatrix create a special matrix (realy a list) that can cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Clean m from cache
        m <<- NULL
        
        ## Create function set (set the value of the matrix)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## ... get value of the matrix
        get <- function() x
        ## setinv. Set tehe inverse matrix 
        setinv <- function(inv) m <<- inv
        ## GetInv. Get the inverse matrix
        getinv <-function() m
        
        ## Create a list 
        list(set = set, get = get,
             getinv = getinv,
             setinv = setinv)
        
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        
        
        if (!is.null(m) ) {
                message("getting cached data")
                return(m);
        }
        
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
