## makeCacheMatrix stores and retrieves the inverse for given matrix as well as the matrix, It takes input a matrix.
## cacheSolve is responsible for verifying whether the inverse for matrix is already stored, If so it returns cached inverse
## Else it calculates inverse for given matrix and caches it for further retrivals.

## makeCacheMatrix: takes input a matrix. it returns a list of 4 functions. set: to store the matrix. 
## get:to retrive the matrix. getinverse:to retrieve cached inverse of matrix setinverse:to store inverse of matrix in cache.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get<-function()x
    
    setinverse<- function(inverse) m <<- inverse
    
    getinverse<- function()m
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve: takes input of type makeCacheMatrix. Checks whether inverse of matrix is cached.if its cached it returns inverse of matrix from cache to display, 
## else calcualtes the inverse of matrix and stores in cache and returns inverse of matrix to display.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
