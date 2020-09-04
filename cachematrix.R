## makeCacheMatrix is a function that creates an object of class matrix
## which allows us to manage the value of the evaluated function to 
## another environment so we can recycle the matrix stored in cache

makeCacheMatrix <- function(x = matrix()) {
    minv<-NULL
    set<-function(x1){
        x<<-x1
        minv<<-NULL
    }
    get<-function() x
    
    setsolve<-function(solve) minv<<-solve
    getsolve<-function() minv
    
    list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## This function will evaluate if the inverse of a matrix has been already
## calculated. If it has been calculated, it get it from the cache.
## If the inverse has not been calculated, it do that and update the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minv<-x$getsolve()
        ## Check if it's in cache
    if(!is.null(minv)){
        message("getting cached data")
        return(minv)
    }
    data<- x$get()
    minv<-solve(data)
    x$setsolve(minv)
    minv
    
}

