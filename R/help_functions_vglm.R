

get.se.symmetric <- function(vcov, k, ref.level, is_parallel){

  # number of (non-parallel) covariates
  q <- k-1
  p <- length(is_parallel)
  
  # initialize matrix for standard errors and running index s1
  se.mat <- matrix(0, ncol = p, nrow = k)
  s1 <- 1
  
  # initialize transformation matrix TM
  TM <- matrix((-1/k), ncol = q, nrow = q)
  diag(TM) <- q/k

  # go along all covariates
  for (i in 1:p) {
    if(is_parallel[i]){
      se.mat[,i] <- rep(sqrt(vcov[s1,s1]),k)
      s1 <- s1 + 1
    }else{
      s2 <- s1 + k - 2
      cov2 <- vcov[s1:s2, s1:s2]
      cov1 <- TM %*% cov2 %*% t(TM)
      var1 <- diag(cov1)
      var2 <- matrix(rep(-1, q), ncol = q) %*% 
        cov1 %*% matrix(rep(-1, q), ncol = 1)
      se3 <- rep(0, k)
      se3[ref.level] <- var2
      se3[-ref.level] <- var1
      se.mat[, i] <- sqrt(se3)
      s1 <- s2 + 1
    }
   
  }
  
  t(se.mat)
}

get.coefs.symmetric <- function(coef.orig, k, ref.level, is_parallel){

  # number of (non-parallel) covariates
  q <- k-1
  
  # initialize transformation matrix TM
  TM <- matrix((-1/k), ncol = q, nrow = q)
  diag(TM) <- q/k
  
  # transform coef.orig matrix into symmetric side constraints
  coef.sym <- TM %*% t(coef.orig)
  coef.help <- matrix(rep(-1, q), ncol = q) %*% coef.sym
  coefs <- matrix(0, ncol = nrow(coef.orig), nrow = k)
  coefs[ref.level, ] <- coef.help
  coefs[-ref.level, ] <- coef.sym
  
  for(i in seq_along(is_parallel)){
    if(is_parallel[i]){
      coefs[,i] <- rep(coef.orig[i,1],k)
    }
  } 
  
  t(coefs)
}

