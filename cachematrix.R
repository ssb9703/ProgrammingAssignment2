## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 ## a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {

        inv = NULL # giving it type null
        set = function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL # setting the environment to null
        }
        get = function() x # empty function that does x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv # empty function that does inv
        list(set=set, get=get, setinv=setinv, getinv=getinv) # list all of the stored variables in the environment for x

}


## Write a short comment describing this function
 ## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()  #gets and sets the inverse
        
        # if the inverse has already been calculated, not null
        if (!is.null(inv)){
                # get's and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inversema
        mat.data = x$get() # make a new matrix frame from $get
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)