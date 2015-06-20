#two functions that cache, retrieve and compute the inverse of a matrix

#function to store the matrix and inversed matrix, to be called upon by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {         #argument takes a matrix, default is an empty matrix
    m <- NULL                                       #creates an empty variable called "m"
    set <- function(y) {                            #stores the inputted matrix in variable "x"
        x <<- y
        m <<- NULL                                  #resets "m" to being empty (as a new matrix is being inverted)
    }
    get <- function() x                             #retrieves the matrix stored in variable "x"
    setinverse <- function(inverse) m <<- inverse   #stores the inputted matrix in variable "m"
    getinverse <- function() m                      #retrieves the matrix stored in variable "m"
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    #creates a list that allows subsetting of the makeCacheMatrix function, so that its nested functions can be called upon
}

#function to either retrieve cached matrix, or calculate inverse of matrix and then store it

cacheSolve <- function(x, ...) {                    #first argument takes the makeCacheMatrix function, the second any extra data you want to invert
    m <- x$getinverse()                             #calls on the getinverse function, and stores its result in variable "m"
    if(!is.null(m)) {                               #if there is a value (a matrix) already within the m variable..    
        message("getting cached data")
        return(m)                                   #return that matrix  
    }
    data <- x$get()                                 #else if m is empty, use the get function to assign the inputted matrix to variable "data" 
    m <- solve(data, ...)                           #invert the matrix within "data" along with any other inputted data, and assign to "m"
    x$setinverse(m)                                 #store the contents of "m" in the "m" variable within the first function's environment
    m                                               #return the inversed matrix stored within "m"
}