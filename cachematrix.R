#makeCacheMatrix() will create a object to solve and store the values of the matrix and its inverse.
#cacheSolve() will check whether the inverse of a matrix was previously stored. If so, it will return the value from the cache. If not, it will solve and cache the value.

#The following function creates an object of type 'list'. This object stores the original matrix's value and a  cached value, initially set to 'NULL'. There are four internal function s or methods, two to read the value of the stored values and two to change them.
makeCacheMatrix <- function(x = matrix()) { 
  
  i <- NULL #i is the inverse of the matrix and is reset to NULL for every time makeCacheMatrix() is called.
  
  #set lets you assign a new value to the object, if that's necessary to save memory. But it is not used by cacheSolve().
  set <- function(y) { 	#Takes an input vector
    x <<- y 					#Saves the input vector 
    i <<- NULL 				#Resets the mean to NULL 
  }
  
  get <- function() {x} 	#Returns the value of the original matrix

  setsolve <- function(solve) i <<- solve 	#This is called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment.
  
  getsolve <- function() {i} 	#Return the cached value of i to cacheSolve() on subsequent accesses.
  
  list(set = set, get = get, 	#List of internal methods, so that cacheSolve() or any other calling function  
       setsolve = setsolve,		#knows how to access those methods.
       getsolve = getsolve)
}


#The following function checks whether for the inverse of 'x' and displays the cached result if previously solved. If not, the function solves for the inverse and stores the solution in the cache.
cacheSolve <- function(x, ...) {  #x is an object created by makeCacheMatrix().

  i <- x$getsolve()   #Access the object 'x' and gets the value for the inverse.
  
  if(!is.null(i)) { #If inverse is already cached (i.e. not NULL)...
    message("getting cached data") #... print message 
    return(i) #Returns value and ends function.
  }
  
  data <- x$get() #This part will only be read if x$getsolve is NULL.
  
  i <- solve(data,...) #Solve for the inverse.
  
  x$setsolve(i) #Store the calculated inverse in x.
  
  i # Return the inverse of the matrix to the code that called the function
}