#makeCacheMatrix

makeCacheMatrix<-function(x=matrix()) {
    inv<-NULL
    set<-function(y) {
        x<<-y
        inv<<-NULL							                          	#set the value of the vector
    }
    get<-function() x							                	        #get the value of the vector
    setinverse<-function(inverse) inv<<-inverse 			    	#set the value of the mean
    getinverse<-function() inv						                	#get the value of the mean
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve

cacheSolve<-function(x, ...) {
    inv<-x$getinverse()
    if(!is.null(inv)) {
        message("the cach data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data)
    x$setinverse(inv)
    inv
}
