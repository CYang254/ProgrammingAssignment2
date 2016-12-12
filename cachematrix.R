## The file contains two functions that calculate the matrixs' inverse
## If the inverse is calculated before and cached, the program will extract the inverse directly rather than calculating it again

## The first function creates a special "matrix" object that can cache its inverse
## It returns a list of four functions which are "setters" and "getters"

makeCacheMatrix <- function(x = matrix()) {
        Inv<-NULL
        set<-function(y){
               x<<-y
               Inv<<-NULL
        }
        get<-function() x       
        setInv<-function(Inverse) Inv<<-Inverse       
        getInv<-function() Inv
                
        list(set=set,get=get,setInv=setInv,
             getInv=getInv)

}


## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        Inv<-x$getInv()
        if(!is.null(Inv)){
               message("getting cached data")
               return(Inv)
        }
        matrix<-x$get()
        Inv<-solve(matrix,...)
        x$setInv(Inv)
        Inv
}
