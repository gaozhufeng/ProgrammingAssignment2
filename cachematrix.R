## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix reates a special "matrix" object that can cache its inverse.
# cacheSolve calculates the mean of the special "matrix" created with the function makeCacheMatrix.
# Then there is a test part to test the two functions with real exmaples

## Write a short comment describing this function
#This function makeVector creates a special "vector", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of this matrix
#get the value of the inverse of this matrix

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
#The following function calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the mean from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix by using sovle and get function,
#and sets the value of the inverse of the matrix in the cache via the setinv function.

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

#The following part tests whether the funtion makeCacheMatrix and cacheSolve work 
#Note, the input x and y are invertible matrices
#x is an invertible 2*2 matrix. It is taken as input to create cache_x using function makeCacheMatrix.
#When call cacheSolve(cache_x) for the first time, it calcuated the inverse.
#For the second time, it just got the inverse
#When change cache_x by another matix y, the inverse was reset to NULL
#When call cacheSolve(cache_x) after setting cache_x using y, it calcuated the inverse
#
#You expect to see the following outputs
#> x=matrix(1:4,2,2)
#> cache_x<-makeCacheMatrix(x)
#> cacheSolve(cache_x)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(cache_x)
#The matrix has not changed and get cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> y=matrix(c(1,0,0,0,1,0,0,0,2),3,3)
#> cache_x$set(y)
#> cacheSolve(cache_x)
#[,1] [,2] [,3]
#[1,]    1    0  0.0
#[2,]    0    1  0.0
#[3,]    0    0  0.5

x=matrix(1:4,2,2)
cache_x<-makeCacheMatrix(x)
cacheSolve(cache_x)
cacheSolve(cache_x)
y=matrix(c(1,0,0,0,1,0,0,0,2),3,3)
cache_x$set(y)
cacheSolve(cache_x)

