# A series of functions to perform an inverse of a matrix and cache the matrix and the inverse, 
# so that future inverse calculations need not be performed unless the original matrix changes.
# 
# Example functions from the homework assignment, with explanations gleaned from forum.
#
# Test functions gleaned from forum to test matrix inversion and caching.
#
# Note: Single-letter variable and function names aren't good form.  They are easily forgotten and mixed up
# and are difficult to maintain after the program is written.  Since I used the example code for reference, 
# it may be useful to include a little translation table between the variables used in the example code, and 
# the variable names I use.
# Example Code:                                 My Code:
# m (stood for mean)                            calculated_inverse
# mean (new mean to set m to)                   new_inverse
# x (stood for vector inside makeVector)        matrix_to_cache (inside makeCacheMatrix) 
# x (stood for vector inside cachemean)         matrix_to_invert (inside cacheSolve) 
# y (stood for new vector)                      new_matrix
# setmean                                       setCalculatedInverse
# getmean                                       getCalculatedInverse


# Creates a special "matrix" object that can cache its inverse.
# Each call to makeCacheMatrix, instantiates a unique closure containing the functions 
# set, get, setCalculatedInverse, getCalculatedInverse.
makeCacheMatrix <- function(matrix_to_cache = matrix()) {
	# initializes the variable "calculated_inverse", clearing any old values. 
	# calculated_inverse is local to makeCacheMatrix here.
	calculated_inverse <- NULL
	
	# Subfunction setter for matrix_to_cache.  When called, matrix_to_cache is set to passed-in value, 
	# and the inverse is reset to NULL.
	set <- function(new_matrix) {
		# Set "matrix_to_cache" to "new_matrix" within the whole function "makeCacheMatrix", not just the set sub-function.
		matrix_to_cache <<- new_matrix
		# Since we have a new matrix, we can't keep the old calculated_inverse.
		# We must re-set it to NULL to flag it for re-calculating.
		calculated_inverse <<- NULL
	}
	
	# Sub-function getter for matrix_to_cache.
	# Return the current value of matrix_to_cache in the environment (lexical scope) of when the function was defined.
	get <- function() { 
		return(matrix_to_cache)
	}
	
	# Sub-function setter for the inverse.  Outside functions, s.a. cacheSolve, call it and ask it to 
	# set the cached inverse calculation to the passed-in value.
	setCalculatedInverse <- function(new_inverse) { 
		calculated_inverse <<- new_inverse
	}
	
	# Sub-function getter for the inverse.  
	# Returns the inverse stored in the parent makeCacheMatrix function
	getCalculatedInverse <- function() { 
		return(calculated_inverse)
	}
	
	# Create the list of sub-functions. Each element of the list has two parts: 
	# the name of the element before the "=" and the pointer to the function object after the "=".
	list(set = set, get = get, setCalculatedInverse = setCalculatedInverse, getCalculatedInverse = getCalculatedInverse)	
}



# Computes the inverse matrix of the special "matrix" saved into makeCacheMatrix above. 
# If the inverse has been previously calculated (and the matrix is not flagged as changed), 
# then retrieves the inverse from the cache without re-doing the costly calculation.
# Otherwise, performs the calculation, and sets the cache to hold the result for future requests.
# Assumption: my_matrix is a square invertible matrix.
cacheSolve <- function(matrix_to_invert, ...) {
	my_inverse <- matrix_to_invert$getCalculatedInverse()
	
	if (!is.null(my_inverse)) {
		message("cacheSolve: Getting cached data.")
		return(my_inverse)
	}
	
	message("cacheSolve: No cached data. Performing new calculations.")
	saved_matrix <- matrix_to_invert$get()
	
	my_inverse <- solve(saved_matrix)
	
	message("cacheSolve: Setting calculation result into cached data.")
	matrix_to_invert$setCalculatedInverse(my_inverse)

	return(my_inverse)
}




## ############ Study examples with explanations from forum posts for reference: ############

# x <- 1:10
# xx <- makeVector(x)
# ls.str(environment(xx$get))
# cachemean(xx)
# ls.str(environment(xx$get))


# create a special "vector", which is really a list containing a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean
# line 1 (function declaration) creates a function called "makeVector", which takes one argument and coerces it to be numeric. Everything that follows is part of this function.
# If I understand, calling "test <- makeVector(someVector)" CREATES an environment where m and x are stored. And you can access the values in that invisible environment using things like test$getmean(). That's what they're calling "caching"--accessing some environment where there are name/value pairs, such as m and x, that are in memory from when that original line was called. And you can alter and access those name/values with things like test$getmean() and test$setmean(example_mean) and the other two functions in the list also. Remember though, the only output of makeVector is a list of functions (function handles?), but you can access the environment with m and x in it through that object.
# Except for the last line, "list(...)", in makeVector, the order of content doesn't matter, I believe. It's just getting those functions defined. It's a list of definitions. The order of lines in cachemean, on the other hand, very much matters. 
# Don't mix up the "x" in makeVector and the x in cachemean; they refer to different things. You might already know that, but it's annoying to keep straight at first when this pair of functions is given to you and you're trying to sort everything. "m" and "mean" both show up multiple times too, so just keep straight what they're referring to.
makeVector <- function(x = numeric()) {
	# initializes the variable "m" as a NULL. That is to say, it clears any values from "m", so you don't have to worry about some leftover value getting introduced into your function.
	m <- NULL
	
	# creates a sub-function called "set", which takes an argument "y".
	# You might wonder why the function set is included, since cachemean doesn't call it. It's for, hypothetically, if we wanted to have our same object, "test", after having already done "test <- makeVector(someVector)", and keep it but just store/cache a different set of name/value pairs associated with it, namely a new vector, and flush the mean.
	# There's some miss understanding on what you state above: y is not undefined; it's the value passed when calling x$set (In which case x is reset to this value, and the mean value m is "cleaned" by assigning NULL to it).
	set <- function(y) {
		# is part of the "set" sub-function, and sets "x" to "y" within the whole function "makeVector", not just the sub-function.
		# from TA: <<- will look for a variable starting at the scope of the parent and going to the global environment.  So it does not require that variable getting assigned to be actually in global().  In the makeVector instance, it is actually finds variables in the scope of when the function was created.
		# the times you see that double arrow symbol in makeVector, those are just defining those functions, not making any calls to try to make changes in other environments. However, cachemean uses the <<- to make assignments, when it calls x$setmean(m), because that's actually calling and using the function setmean, which contains <<- in it.
		x <<- y
		# also part of "set", initializes "m". This makes sense because you are telling the function that you made a new vector (which likely has a different mean, a.k.a. a different "m").
		# The first m<-NULL assignment above initializes the var. The 2nd assignment cleans it's value in case you change the initial vector.
		m <<- NULL
	}
	
	# creates a sub-function called "get", which reports the value of x.
	get <- function() { 
		x # return the current value of x in the environment (lexical scope) of when the function was defined
	}
	
	# creates a sub-function called "setmean", which <strike>uses the built-in "mean" function to get the mean of x (no idea how, I assume it automatically takes x as an argument because of the parent function).</strike> It then assigns the result of the mean function to "m". EDIT: tried to strikeout the old. After some more examination, I believe that "setmean" actually works mostly through its interaction with the cachemean function, which applies mean() to the vector data before using setmean to assign a value to m.
	# the "proper" use of setmean() appears to be that of an "internal" function.  You override the purpose of cachemean() if you call setmean() directly.  So if you want the mean of 1:4 to be 400, then that's how you do it!  But that is a matter of data integrity and security, not R
	setmean <- function(mean) { 
		m <<- mean
	}
	
	# creates a sub-function called "getmean", which reports the value of "m".
	getmean <- function() { 
		m
	}
	
	# create the list itself. For each element of the list, I believe that the part before the "=" indicates the name or key to be used to call that item, and the part after identifies the item (in this case, the sub-function) that the name or key will call. I don't know if or why this part is necessary.
	# Note that makeVector is only useful when you use it to assign a variable.  That variable you assigned with makeVector is now an instantiation of a makeVector closure (i.e. when you call makeVector, you instantiate a closure containing the functions set, get, getmean, setmean)
	# e.g.
	# xx <- makeVector()
	# yy <- makeVector()
	# xx and yy are two separate instantiations of makeVector closures
	# xx$set(1)
	# yy$set(2)
	# Now, xx$get() and yy$get() will pull the "x" variable from their own unique closures
	list(set = set, get = get, setmean = setmean, getmean = getmean)
}



# calculates the mean of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
# retrieves m from the cache, which either turns out to be the initiated value, NULL (remember?) if this is the first time we've run cacheMean on it, OR it will be an actual stored mean. So cachemean tests which of those it is. If it's a mean, it goes ahead and returns it as the answer using return(m). If it was NULL, it skips the "if" section and gets the original vector that we put into makeVector, calculates the mean of it right then and there, AND THEN STORES IT IN THE CACHE using x$setmean(m), which, as you can see in the definitions of setmean, assigns the actual mean to the variable m IN THE ENVIRONMENT from when we first called makeVector, using <<-. The cache is associated with the object "test" (from my previous post), and that object must be referenced to retreive things from the cache, such as in calling test$setmean(some_number) in the command line, which is the same thing as x$setmean(m) in cachemean. Another example is test$getmean() in the command line, which is the same as x$getmean() in cachemean. Any time we make make a call of a function that has "<<-", we're trying to make a new assignent in a non-local environment. And when GETTING something from that environment, we don't need "<<-", but we have to reference "test". 
cachemean <- function(x, ...) {
	m <- x$getmean()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}




## ############ Code to test with from forum post: ############
# generate matrix, and the inverse of the matrix.
testInverseMatrix <- function() {
	size <- 1000 # size of the matrix edge, don't make this too big
	
	mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
	mymatrix.inverse <- solve(mymatrix)
	
	# now solve the matrix via the cache-method
	special.matrix <- makeCacheMatrix(mymatrix)
	
	# this should take long, since it's the first go
	start1 <- Sys.time()
	special.solved.1 <- cacheSolve(special.matrix)
	end1 = Sys.time() - start1
	
	# this should be lightning fast
	start2 <- Sys.time()
	special.solved.2 <- cacheSolve(special.matrix)
	end2 = Sys.time() - start2
	
	# check if all solved matrices are identical
	# should return TRUE
	comparison_result <- identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)
	print(comparison_result)
	print(end1)
	print(end2)
	
	
	## Tests below need review and additional functionality to code or test code repair.
	# now let's call cacheSolve again but with a small variation
	# in this case you should NOT see the "getting cached data" message
	# because the parameters to CacheSolve changed even if the matrix is the same!
	# You will get the cached result, which is incorrect. Try calling solve by itself and you'll see that once the 
	# parameters of the call change, the cache should be invalidated.
	# It is not in the scope of assignment to check for change in the parameters.  This is something extra that should do.
	# Check the docs for solve with ?solve.  You'll find that:
	# solve(a) gives very different results from solve(a,b) or solve (a,b,....)
	# So getting the cached data previously calculated for a solve(a) call and passing that up to a cacheSolve(a,b) call 
	# would give incorrect results to the function caller.  This is beyond the scope of the assignment 
	# but it's great if you could improve your code for this additional case.
	special.solved.3 <- cacheSolve(special.matrix, rep(4,1000))
	
	comparison_result <- identical(special.solved.1, special.solved.3)
	print(comparison_result)
	
	# should return FALSE
	special.solved.4 <- cacheSolve(special.matrix, rep(4,1000))
	
	# now you should get the cached result again
	# should return TRUE
	comparison_result <- identical(special.solved.3, special.solved.4)
	print(comparison_result)
	

	# Enhancement to check whether the function handle matrix change correctly
	# mymatrix2 <- matrix(rnorm(size^2), nrow=size, ncol=size)
	# mymatrix.inverse2 <- solve(mymatrix2)
	
	# Change the matrix inside special.matrix to mymatrix2
	# special.matrix$set(mymatrix2)
	
	# This should take long, since it's the first go and should not come from cache
	# If the result come from cache, your code do not detect the matrix change correctly
	# Most likely, your set fucntion is not working correctly.
	# special.solved.3 <- cacheSolve(special.matrix)
	
	# this should be lightning fast and come from cache
	# special.solved.4 <- cacheSolve(special.matrix)
	
	# check if all solved matrices are identical
	# identical(mymatrix.inverse2, special.solved.3) & identical(mymatrix.inverse2, special.solved.4)
	# should return TRUE
	
	
	# another user:
	# I added these lines to the test code just to be sure the inverse was correct:
	# iden <- mymatrix %*% special.solved.1
	# round(iden[1:5,1:5])
	# this should be the output
	# [,1] [,2] [,3] [,4] [,5]
	# [1,]    1    0    0    0    0
	# [2,]    0    1    0    0    0
	# [3,]    0    0    1    0    0
	# [4,]    0    0    0    1    0
	# [5,]    0    0    0    0    1
	
	
}





