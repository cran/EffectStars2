#' Plot effect stars for DIFboost objects.
#' 
#' @aliases effectstars.DIFboost
#' @export 
#' @export effectstars.DIFboost
#' @description Plots effect stars for \code{\link[DIFboost]{DIFboost}}-objects. The parameter estimates 
#' for DIF-items are plotted, grouped by items. \cr
#' 
#' For more details on plotting effect stars see \code{\link{effectstars}}.
#' 
#' @param x A \code{\link[DIFboost]{DIFboost}}-object.
#' @param only.DIFitems If \code{TRUE}, only the estimates unequal to zero (estimates from the DIF-items)
#' are visualized with EffectStars. 
#' @param ... further arguments for generic function \code{\link{effectstars}}.
#' @seealso \code{\link{star.ctrl}},  \code{\link{effectstars}}
#' @author Gunther Schauberger \cr
#' \email{gunther.schauberger@tum.de} \cr \cr
#' \url{https://www.sg.tum.de/epidemiologie/team/schauberger/}
#' 
#' @references
#' Schauberger, G. and Tutz, G. (2016): \emph{Detection of Differential Item Functioning in Rasch Models by Boosting Techniques}, 
#' British Journal of Mathematical and Statistical Psychology, 69(1), 80 - 103  \cr \cr
#' Tutz, G. and Schauberger, G. (2013): \emph{Visualization of Categorical Response Models -
#' from Data Glyphs to Parameter Glyphs}, Journal of Computational and Graphical Statistics 22(1), 156--177.
#' \cr \cr  Gerhard Tutz (2012): \emph{Regression for Categorical Data}, Cambridge University Press
#' 
#' @examples
#' \dontrun{
#' ### example for DIFboost
#' 
#' library(DIFboost)
#' data(simul.data)
#' 
#' Y <- simul.data[,1:10]
#' X <- simul.data[,11:13]
#' 
#' m1 <- DIFboost(Y = Y, X = X) 
#' 
#' effectstars(m1)
#' 
#' }




effectstars.DIFboost <- function(x, only.DIFitems = TRUE, ...){
  
  if(only.DIFitems){
    coefs <- exp(t(x$dif.mat[,x$DIF.Items]))
  }else{
    coefs <- exp(t(x$dif.mat))
  }
  
  
  effectstars(coefs, ...)
}
