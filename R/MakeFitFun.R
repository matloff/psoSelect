
makeFitFun <- function(data,yName,k) 
{
   newF <- function() 
   {
      qeKNN(data,yName,k)
   }

   return(newF())
}

# for any preditive qeML function

# example

# z <- makeQEfitfun(qeLin,list(data=svcensus,yName='wageinc'))
# dps <- DPSO(num_particles=100, dimensions=5, max_iter=30, z)
# result <- dps$optimize(0.5, 1.5, 1.5)
# result$best_position


makeQEfitfun <- function(ftn,ftnArgs)
{
   if (!('data' %in% names(ftnArgs)) )
      stop("need 'data' arg")
   if (!('yName' %in% names(ftnArgs)) )
      stop("need 'yName' arg")

   
   f <- function(solution) 
   {

      data <- ftnArgs[['data']]
      if (length(solution) != (ncol(data)-1))
        stop("wrong 'dimensions' value")
      yName <- ftnArgs[['yName']]
      yCol <- which(names(data) == yName)
      xCols <- setdiff(1:ncol(data),yCol)
      solutionCols <- xCols * solution
      data <- data[,c(solutionCols,yCol)]
      do.call(ftn,ftnArgs)$testAcc

   }

   return(f)
}


