#' Plot effect stars for DIFlasso objects.
#' 
#' @aliases effectstars.DIFlasso
#' @export 
#' @export effectstars.DIFlasso
#' @description Plots effect stars for \code{\link[DIFlasso]{DIFlasso}}-objects. The parameter estimates 
#' for DIF-items are plotted, grouped by items. \cr
#' 
#' For more details on plotting effect stars see \code{\link{effectstars}}.
#' 
#' @param x A \code{\link[DIFlasso]{DIFlasso}}-object.
#' @param only.DIFitems If \code{TRUE}, only the estimates unequal to zero (estimates from the DIF-items)
#' are visualized with EffectStars. 
#' @param ... further arguments for generic function \code{\link{effectstars}}.
#' @seealso \code{\link{star.ctrl}},  \code{\link{effectstars}}
#' @author Gunther Schauberger \cr
#' \email{gunther@stat.uni-muenchen.de} \cr \cr
#' \url{http://www.statistik.lmu.de/~schauberger/}
#' 
#' @references  
#' Tutz, G. and Schauberger, G. (2015): \emph{A Penalty Approach to Differential Item Functioning in Rasch Models},
#' , Psychometrika, 80(1), 21 -- 43\cr \cr
#' Tutz, G. and Schauberger, G. (2013): \emph{Visualization of Categorical Response Models -
#' from Data Glyphs to Parameter Glyphs}, Journal of Computational and Graphical Statistics 22(1), 156--177.
#' \cr \cr  Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press


#' @examples
#' \dontrun{
#' ### example for DIFlasso
#' 
#' library(DIFlasso)
#' data(simul.data)
#' 
#' Y <- simul.data[,1:10]
#' X <- simul.data[,11:13]
#' 
#' m1 <- DIFlasso(Y = Y, X = X, trace = TRUE) 
#' 
#' effectstars(m1)
#' 
#' }




effectstars.DIFlasso <- function(x, only.DIFitems = TRUE, ...){
  
  if(only.DIFitems){
    coefs <- exp(t(x$dif.mat[,x$dif.items]))
  }else{
    coefs <- exp(t(x$dif.mat))
  }
  
  
  effectstars(coefs, ...)
}
