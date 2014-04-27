## makeCacheMatrix stores and retrieves the mean for given matrix as well as the matrix, It takes input a matrix.
## cacheSolve is responsible for verifying whether the mean for matrix is already stored, If so it returns cached mean.
## Else it calculates mean for given matrix and caches it for further retrivals.

## makeCacheMatrix: takes input a matrix. it returns a list of 4 functions. set: to store the matrix. get:to retrive the matrix. getmean:to retrieve cached mean. setmean:to store mean in cache.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y)
    {
        x <<- y
        m <<- NULL
    }
    
    get<-function()x
    
    setmean<- function(mean) m <<- mean
    
    getmean<- function()m
    
    list(set=set,get=get,setmean=setmean,getmean=getmean)
}


## cacheSolve: takes input of type makeCacheMatrix. Checks whether mean is cached.if its cached it returns mean from cache, else calcualtes the mean and stores in cache.

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmean(m)
    m
    ## Return a matrix that is the inverse of 'x'
}
