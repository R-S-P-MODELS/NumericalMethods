MaxSteps=100


dfs<-function(x){
return( 1e2*(f(x+1e-2)-f(x)) )
}


Bicessao<-function(a,b,tol){
cont=0
vec=c()
fvec=c()
while(cont<MaxSteps){
 if(abs(f(a))<tol){
	fvec[cont]=f(a)
	vec[cont]=a
	return(list(vec,fvec))
}
 if(abs(f(b))<tol){
	fvec[cont]=f(b)
	vec[cont]=b
	return(list(vec,fvec))
}
 meio=(a+b)/2
 if(abs(f(meio))<tol){
	fvec[cont]=f(meio)
	vec[cont]=meio
 	return(list(vec,fvec))
}
 if(f(meio)> 0)
 	b=meio
 else
	a=meio
 cont=cont+1;
 vec[cont]=meio
 fvec[cont]=f(meio)
}
print("Não Convergiu")
return(list(vec,fvec))
}


bisection <- function(a, b, n = MaxSteps, tol = 1e-5) {
  # If the signs of the function at the evaluated points, a and b, stop the function and return message.
  if (!(f(a) < 0) && (f(b) > 0)) {
    stop('signs of f(a) and f(b) differ')
  } else if ((f(a) > 0) && (f(b) < 0)) {
    stop('signs of f(a) and f(b) differ')
  }
  vec=c()
  fvec=c()
  
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
    vec[i]=c
    fvec[i]=f(c)
    # If the function equals 0 at the midpoint or the midpoint is below the desired tolerance, stop the 
    # function and return the root.
    if ((f(c) == 0) || ((b - a) / 2) < tol) {
      return(list(vec,fvec))
    }
    
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
    ifelse(sign(f(c)) == sign(f(a)), 
           a <- c,
           b <- c)
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
      return(list(vec,fvec))
}


newton.raphson <- function(a, b, tol = 1e-5, n = MaxSteps) {
  #require(numDeriv) # Package for computing f'(x)
  vec=c()
  fvec=c()
  cont=1
  x0 <- a # Set start value to supplied lower bound
  k <- n # Initialize for iteration results
  
  # Check the upper and lower bounds to see if approximations result in 0
  fa <- f(a)
  if (fa == 0.0) {
    return(list(a,f(a)))
    #return(a)
  }
  
  fb <- f(b)
  if (fb == 0.0) {
    return(list(b,f(b)))
  }

  for (i in 1:n) {
    vec[i]=x0
    fvec[i]=f(x0)
    dx <- dfs(x0) # First-order derivative f'(x0)
    x1 <- x0 - (f(x0) / dx) # Calculate next value x1
    k[i] <- x1 # Store x1
    # Once the difference between x0 and x1 becomes sufficiently small, output the results.
    if (abs(x1 - x0) < tol) {
      root.approx <- tail(k, n=1)
      res <- list('root approximation' = root.approx, 'iterations' = k)
      #return(res)
      return(list(vec,fvec))
    }
    # If Newton-Raphson has not yet reached convergence set x1 as x0 and continue
    x0 <- x1
  }
  print('Too many iterations in method')
  return(list(vec,fvec))
}




fixedpoint <- function(x0, tol=1e-07, niter=MaxSteps){
	## fixed-point algorithm to find x such that fun(x) == x
	## assume that fun is a function of a single variable
	## x0 is the initial guess at the fixed point
 	vec=c()
	fvec=c()
	xold <- x0
	xnew <- f(xold)
	for (i in 1:niter) {
		xold <- xnew
		vec[i]=xold
		fvec[i]=f(xold)
		xnew <- f(xold)
		if ( abs((xnew-xold)) < tol )
			return(list(vec,fvec))
		}
	#stop("exceeded allowed number of iterations")
	return(list(vec,fvec))
}


secant <- function(x0, x1, tol=1e-07, niter=MaxSteps){
	vec=c()
	fvec=c()
	for ( i in 1:niter ) {
		x2 <- x1-f(x1)*(x1-x0)/(f(x1)-f(x0))
		vec[i]=x2
		fvec[i]=f(x2)
		if (abs(f(x2)) < tol)
			return(list(vec,fvec))
		x0 <- x1
		x1 <- x2
	}
	return(list(vec,fvec))
	stop("exceeded allowed number of iteractions")
}
