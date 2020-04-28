#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        #assigns NULL to a variable within the current environment
        m <- NULL
        
        #set the matrix value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the matrix value with setmatrix
        get <- function() x
        
        #save the cached value of inverse matrix in m
        setInv <- function(solve) m <<- solve
        
        #get the saved value of inverse matrix m
        getInv <- function() m
        
        #return a list of the methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}



#This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        
        #get if the inverse matrix already calculated
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #get our matrix from input of function
        data <- x$get()
        
        #calculate the inverse of our matrix
        m <- solve(data)
        
        #cache the inverse of our matrix
        x$setInv(m)
        
        #return the inverse matrix
        m
}