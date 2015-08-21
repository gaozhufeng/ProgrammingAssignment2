## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinv<-function(z){
                inv<<-z
        }
        getinv<-function() inv
        list(set = set, get = get,setinv = setinv,getinv = getinv)

}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        
        if(!is.null(inv)){
                message("The matrix has not changed and get cached data")
                return(inv)
        }
        inv<-solve(x$get())
        x$setinv(inv)
        inv
}

#test
x=matrix(1:4,2,2)
cache_x<-makeCacheMatrix(x)
cacheSolve(cache_x)
cacheSolve(cache_x)
y=matrix(c(1,0,0,0,1,0,0,0,2),3,3)
cache_x$set(y)
cacheSolve(cache_x)

