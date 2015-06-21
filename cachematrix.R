makeCacheMatrix <- function(x = matrix()) {
        ## Input -> x: a square invertible matrix
        ## Output ->  a list containing a function to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
               inverse = NULL #Initial value of the inverse
        
        set = function(z) {
                
                x <<- z
                inverse <<- NULL
                # <<- operator which can be used to assign a value to an object in an environment 
                # that is different from the current environment
        }
        
        get = function() x
        setinv = function(inverse2) inverse <<- inverse2 
        getinv = function() inverse
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inverse = x$getinv()
        
        #We need to check if the inverse has been calculated
        if (!is.null(inverse)){
                message("cached data obtained")
                return(inverse)
        }
        
        # if the inverse is not in cache, calculates the inverse 
        m.data = x$get()
        inverse = solve(m.data, ...)
        
        # Put the inverse in cache
        x$setinv(inverse)
        
        return(inverse)
}
