
  coef.se.multinomial <- function(x, symmetric = TRUE){
  
  # get reference category
  ref.level <- x@misc$refLevel
  
  ### original coefficient matrix, without values for ref.level
  coef.orig <- coef(x, matrix = TRUE)
  
  # number of categories
  k <- ncol(coef.orig)+1
  # number of groups/covariates
  p <- nrow(coef.orig)
  
  # index for parallel parameters  
  is_parallel <- apply(coef.orig,1,function(x){sum(diff(x))})==0
  which.parallel <-  which(is_parallel)
  
  ### coefficient matrix, including reference category or symmetric parameters
  if(!symmetric){
    coefs <- insertCol(coef.orig,ref.level,rep(0,nrow(coef.orig)))
    coefs[is_parallel,ref.level] <- rowMeans(coefs[is_parallel,-ref.level,drop=FALSE])
  }else{
    coefs <- get.coefs.symmetric(coef.orig, k, ref.level, is_parallel)
  }
  
  ### calculate p values
    if(!symmetric){
      vcov<- vcov(x)
      se<- sqrt(diag(vcov))
      
      # create an index to replicate ses for parallel coefficients
      se.index <- c()
      for(i in 1:p){
        if(is_parallel[i]){
          se.index <- c(se.index,k-1)
        }else{
          se.index <- c(se.index,rep(1,k-1))
        }
      }
      
      # use se.index to rep ses for parallel coefficients
      se <- rep(se,se.index)
      # include zeros in ref.level, create matrix
      se <- matrix(se,ncol=k-1,byrow=TRUE)
      se <- insertCol(se,ref.level,rep(0,nrow(se)))
      se[is_parallel,ref.level] <- rowMeans(se[is_parallel,-ref.level,drop=FALSE])
    }else{
      # extra function to calculate ses for symmetric side constraints
      vcov<- vcov(x)
      se <- get.se.symmetric(vcov, k, ref.level, is_parallel)
}
      
  
return(list(coefficients = coefs, stderr = se, which.parallel = which.parallel, 
            is_parallel = is_parallel))
}

