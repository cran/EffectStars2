
  coef.se.ordinal <- function(x){

  ### original coefficient matrix, without values for ref.level
  coef.orig <- coefs <- coef(x, matrix = TRUE)
  
  # number of categories
  k <- ncol(coef.orig)
  # number of groups/covariates
  p <- nrow(coef.orig)
  
  # index for parallel parameters  
  is_parallel <- apply(coef.orig,1,function(x){sum(diff(x))})==0
  which.parallel <-  which(is_parallel)
  
  
  ### calculate p values
      vcov<- vcov(x)
      se<- sqrt(diag(vcov))
      
      # create an index to replicate ses for parallel coefficients
      se.index <- c()
      for(i in 1:p){
        if(is_parallel[i]){
          se.index <- c(se.index,k)
        }else{
          se.index <- c(se.index,rep(1,k))
        }
      }
      
      # use se.index to rep ses for parallel coefficients
      se <- rep(se,se.index)
      # include zeros in ref.level, create matrix
      se <- matrix(se,ncol=k,byrow=TRUE)
      
  
  return(list(coefficients = coefs, stderr = se, which.parallel = which.parallel, 
              is_parallel = is_parallel))
}

