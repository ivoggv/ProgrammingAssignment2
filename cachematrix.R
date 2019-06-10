## This is my first attempt doing this assignment. The first function will
## create a cache matrix based on the info I set.
## 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                
                x<<-y
                m<<-NULL
        }
        get<-function() {
                x
        }
        setinv <- function(inv) m<<-inv
        getinv <- function()m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Given the last function then this one gets the information from the 
## previous chunk of code. IF there is no info then it proceeds to calculate 
## its own. 

cacheSolve <- function(x, ...) {
        m<-x$getinv()
        if (!is.null(m)) {
                message("Getting chached matrix")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinv(m)
        m
}
