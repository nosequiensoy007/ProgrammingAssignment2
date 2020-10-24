

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL 
    set <- function(y){##Give Inverse of matrix
        x<<-y
        m<<-NULL}
    get<- function()x
    setinv<- function(inv)m<<-inv
    getint<- function()m
    list(set=set, get=get,
         setinv=setinv,
         getinv=getinv)
}

cacheSolve <- function(x, ...) {
    ## Return inverse matrix x
    m<- x$getinv
    if (is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m<- solve(data, ...)
    x$setinv(m)
    
}