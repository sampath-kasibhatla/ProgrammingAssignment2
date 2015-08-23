## These two functions compute the inverse of a matrix, but instead of calculating every time the same inverse, it caches
## the last value & returns it, if the specified matrix is the same.


## makeCacheMatrix takes in an argument which is by default an empty matrix. It returns a list of functions which
## are (i)set(sets the matrix vector), (ii)get (gives the matrix stored), (iii)setsolve(stores the inverse 
##of the matrix x),and (iv)getsolve(it gets the cached inverse of a matrix for a specified matrix stored in x)

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<- function() x
        setsolve <-function(solve) s <<- solve
        getsolve <-function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## This function either computes the inverse of a matrix or returns the last computed(cached) value if the
## matrix specified is unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix)
        x$setsolve(s)
        s
}


##  Results after running a sample matrix

##  > m<-matrix(c(1,2,5,6,4,5,4,5,4),nrow=3,ncol=3)
##  > m
##        [,1] [,2] [,3]
##  [1,]    1    6    4
##  [2,]    2    4    5
##  [3,]    5    5    4

##  > x<-makeCacheMatrix(m)
##  > cacheSolve(x)
##           [,1]       [,2]        [,3]
##  [1,] -0.1698113 -0.0754717  0.26415094
##  [2,]  0.3207547 -0.3018868  0.05660377
##  [3,] -0.1886792  0.4716981 -0.15094340

##  > cacheSolve(x)

##  getting cached data
##  [,1]       [,2]        [,3]
##  [1,] -0.1698113 -0.0754717  0.26415094
##  [2,]  0.3207547 -0.3018868  0.05660377
##  [3,] -0.1886792  0.4716981 -0.15094340

##  > solve(m)
##           [,1]       [,2]        [,3]
##  [1,] -0.1698113 -0.0754717  0.26415094
##  [2,]  0.3207547 -0.3018868  0.05660377
##  [3,] -0.1886792  0.4716981 -0.15094340
