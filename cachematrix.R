## Caching the Inverse of a matrix

## creates the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
##initialization
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
#calculates the inverse of non-singular matrix via the solve function
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
## passes the value of the function makeCacheMatrix   
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


##tries to get the cached inverse if present or calculate the inverse 
##if cache is not present

cacheSolve <- function(x, ...) {
        m<-x$getmatrix()
        ##getting the cached value if it exists
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
